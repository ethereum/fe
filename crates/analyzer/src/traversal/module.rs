use crate::errors::{AlreadyDefined, FatalError};
use crate::namespace::scopes::{ModuleScope, Scope, Shared};
use crate::traversal::{contracts, structs, types};
use crate::Context;
use fe_common::diagnostics::Label;
use fe_parser::ast as fe;
use fe_parser::node::Node;
use semver::{Version, VersionReq};
use std::rc::Rc;

/// Gather context information for a module and check for type errors.
pub fn module(context: &mut Context, module: &fe::Module) -> Result<(), FatalError> {
    let scope = ModuleScope::new();

    let mut contracts = vec![];

    for stmt in module.body.iter() {
        match &stmt {
            fe::ModuleStmt::TypeAlias(inner) => type_alias(context, Rc::clone(&scope), inner)?,
            fe::ModuleStmt::Pragma(inner) => pragma_stmt(context, inner),
            fe::ModuleStmt::Struct(inner) => {
                structs::struct_def(context, Rc::clone(&scope), inner)?
            }
            fe::ModuleStmt::Contract(inner) => {
                // Collect contract statements and the scope that we create for them. After we
                // have walked all contracts once, we walk over them again for a
                // more detailed inspection.
                let contract_scope = contracts::contract_def(Rc::clone(&scope), context, inner)?;
                contracts.push((inner, contract_scope))
            }
            fe::ModuleStmt::Import(inner) => context.not_yet_implemented("import", inner.span),
        }
    }

    for (contract, scope) in contracts.iter() {
        contracts::contract_body(Rc::clone(scope), context, contract)?
    }

    context.set_module(scope.into());

    Ok(())
}

fn type_alias(
    context: &mut Context,
    scope: Shared<ModuleScope>,
    type_alias: &Node<fe::TypeAlias>,
) -> Result<(), FatalError> {
    let fe::TypeAlias { name, typ } = &type_alias.kind;
    let typ = types::type_desc(&Scope::Module(Rc::clone(&scope)), context, typ)?;

    if let Err(AlreadyDefined) = scope.borrow_mut().add_type_def(&name.kind, typ) {
        context.fancy_error(
            "a type definition with the same name already exists",
            // TODO: figure out how to include the previously defined definition
            vec![Label::primary(
                type_alias.span,
                format!("Conflicting definition of `{}`", name.kind),
            )],
            vec![format!(
                "Note: Give one of the `{}` definitions a different name",
                name.kind
            )],
        )
    }
    Ok(())
}

fn pragma_stmt(context: &mut Context, stmt: &Node<fe::Pragma>) {
    let version_requirement = &stmt.kind.version_requirement;
    // This can't fail because the parser already validated it
    let requirement =
        VersionReq::parse(&version_requirement.kind).expect("Invalid version requirement");
    let actual_version =
        Version::parse(env!("CARGO_PKG_VERSION")).expect("Missing package version");

    if !requirement.matches(&actual_version) {
        context.fancy_error(
            format!(
                "The current compiler version {} doesn't match the specified requirement",
                actual_version
            ),
            vec![Label::primary(
                version_requirement.span,
                "The specified version requirement",
            )],
            vec![format!(
                "Note: Use `pragma {}` to make the code compile",
                actual_version
            )],
        );
    }
}
