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
        match &stmt.kind {
            fe::ModuleStmt::TypeDef { .. } => type_def(context, Rc::clone(&scope), stmt)?,
            fe::ModuleStmt::Pragma { .. } => pragma_stmt(context, stmt),
            fe::ModuleStmt::StructDef { .. } => {
                structs::struct_def(context, Rc::clone(&scope), stmt)?
            }
            fe::ModuleStmt::ContractDef { .. } => {
                // Collect contract statements and the scope that we create for them. After we
                // have walked all contracts once, we walk over them again for a
                // more detailed inspection.
                let contract_scope = contracts::contract_def(Rc::clone(&scope), context, stmt)?;
                contracts.push((stmt, contract_scope))
            }
            fe::ModuleStmt::FromImport { .. } => {
                context.not_yet_implemented("from import", stmt.span)
            }
            fe::ModuleStmt::SimpleImport { .. } => {
                context.not_yet_implemented("simple import", stmt.span)
            }
        }
    }

    for (stmt, scope) in contracts.iter() {
        if let fe::ModuleStmt::ContractDef { .. } = stmt.kind {
            contracts::contract_body(Rc::clone(&scope), context, stmt)?
        }
    }

    context.set_module(scope.into());

    Ok(())
}

fn type_def(
    context: &mut Context,
    scope: Shared<ModuleScope>,
    stmt: &Node<fe::ModuleStmt>,
) -> Result<(), FatalError> {
    if let fe::ModuleStmt::TypeDef { name, typ } = &stmt.kind {
        let typ = types::type_desc(&Scope::Module(Rc::clone(&scope)), context, &typ)?;

        if let Err(AlreadyDefined) = scope.borrow_mut().add_type_def(&name.kind, typ) {
            context.fancy_error(
                "a type definition with the same name already exists",
                // TODO: figure out how to include the previously defined definition
                vec![Label::primary(
                    stmt.span,
                    format!("Conflicting definition of `{}`", name.kind),
                )],
                vec![format!(
                    "Note: Give one of the `{}` definitions a different name",
                    name.kind
                )],
            )
        }

        return Ok(());
    }

    unreachable!()
}

fn pragma_stmt(context: &mut Context, stmt: &Node<fe::ModuleStmt>) {
    match &stmt.kind {
        fe::ModuleStmt::Pragma {
            version_requirement,
        } => {
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
        _ => unreachable!(),
    }
}
