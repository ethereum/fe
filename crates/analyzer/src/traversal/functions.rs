use crate::context::{AnalyzerContext, ExpressionAttributes, Location, NamedThing};
use crate::display::Displayable;
use crate::namespace::items::{EnumVariantId, Item};
use crate::namespace::scopes::{BlockScope, BlockScopeType};
use crate::namespace::types::{EventField, Type, TypeId};
use crate::pattern_analysis::PatternMatrix;
use crate::traversal::{assignments, call_args, declarations, expressions};
use crate::{
    errors::{self, FatalError},
    namespace::items::EnumVariantKind,
};
use fe_common::diagnostics::Label;
use fe_parser::ast::{self as fe, LiteralPattern, Pattern};
use fe_parser::node::{Node, Span};
use indexmap::map::Entry;
use indexmap::{IndexMap, IndexSet};
use smol_str::SmolStr;

use super::matching_anomaly;

pub fn traverse_statements(
    scope: &mut BlockScope,
    body: &[Node<fe::FuncStmt>],
) -> Result<(), FatalError> {
    for stmt in body.iter() {
        func_stmt(scope, stmt)?
    }
    Ok(())
}

fn func_stmt(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    use fe::FuncStmt::*;
    match &stmt.kind {
        Return { .. } => func_return(scope, stmt),
        VarDecl { .. } => declarations::var_decl(scope, stmt),
        ConstantDecl { .. } => declarations::const_decl(scope, stmt),
        Assign { .. } => assignments::assign(scope, stmt),
        Emit { .. } => emit(scope, stmt),
        AugAssign { .. } => assignments::aug_assign(scope, stmt),
        For { .. } => for_loop(scope, stmt),
        While { .. } => while_loop(scope, stmt),
        If { .. } => if_statement(scope, stmt),
        Match { .. } => match_statement(scope, stmt),
        Unsafe { .. } => unsafe_block(scope, stmt),
        Assert { .. } => assert(scope, stmt),
        Expr { value } => expressions::expr(scope, value, None).map(|_| ()),
        Revert { .. } => revert(scope, stmt),
        Break | Continue => {
            loop_flow_statement(scope, stmt);
            Ok(())
        }
    }
}

fn for_loop(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    match &stmt.kind {
        fe::FuncStmt::For { target, iter, body } => {
            // Make sure iter is in the function scope & it should be an array.
            let iter_type = expressions::assignable_expr(scope, iter, None)?.typ;
            let target_type = if let Type::Array(array) = iter_type.typ(scope.db()) {
                array.inner
            } else {
                return Err(FatalError::new(scope.register_diag(errors::type_error(
                    "invalid `for` loop iterator type",
                    iter.span,
                    &"array",
                    &iter_type.display(scope.db()),
                ))));
            };

            scope.root.map_variable_type(target, target_type);

            let mut body_scope = scope.new_child(BlockScopeType::Loop);
            // add_var emits a msg on err; we can ignore the Result.
            let _ = body_scope.add_var(&target.kind, target_type, false, target.span);

            // Traverse the statements within the `for loop` body scope.
            traverse_statements(&mut body_scope, body)
        }
        _ => unreachable!(),
    }
}

fn loop_flow_statement(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) {
    if !scope.inherits_type(BlockScopeType::Loop) {
        let stmt_name = match stmt.kind {
            fe::FuncStmt::Continue => "continue",
            fe::FuncStmt::Break => "break",
            _ => unreachable!(),
        };
        scope.error(
            &format!("`{}` outside of a loop", stmt_name),
            stmt.span,
            &format!(
                "`{}` can only be used inside of a `for` or `while` loop",
                stmt_name
            ),
        );
    }
}

fn if_statement(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    match &stmt.kind {
        fe::FuncStmt::If {
            test,
            body,
            or_else,
        } => {
            let test_type = expressions::value_expr(scope, test, None)?.typ;
            error_if_not_bool(
                scope,
                test_type,
                test.span,
                "`if` statement condition is not bool",
            );
            traverse_statements(&mut scope.new_child(BlockScopeType::IfElse), body)?;
            traverse_statements(&mut scope.new_child(BlockScopeType::IfElse), or_else)?;
            Ok(())
        }
        _ => unreachable!(),
    }
}

fn match_statement(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    match &stmt.kind {
        fe::FuncStmt::Match { expr, arms } => {
            let expr_type = expressions::expr(scope, expr, None)?.typ;

            let match_scope = scope.new_child(BlockScopeType::Match);

            // Do type check on pattern, then do analysis on arm body.
            let mut err_in_pat_check = Ok(());
            for arm in arms {
                let mut arm_scope = match_scope.new_child(BlockScopeType::MatchArm);

                // Collect binds in the pattern.
                let binds = match match_pattern(&mut arm_scope, &arm.kind.pat, expr_type) {
                    Ok(binds) => binds,
                    Err(err) => {
                        err_in_pat_check = Err(err);
                        continue;
                    }
                };

                // Introduce binds into arm body scope.
                let mut is_add_var_ok = true;
                for (name, bind) in binds {
                    // We use a first bind span in `add_var` here if multiple binds appear in the
                    // same pattern.
                    is_add_var_ok &= arm_scope
                        .add_var(&name, bind.ty, false, bind.spans[0])
                        .is_ok();
                }

                if is_add_var_ok {
                    traverse_statements(&mut arm_scope, &arm.kind.body).ok();
                }
            }

            err_in_pat_check?;
            matching_anomaly::check_match_exhaustiveness(scope, arms, stmt.span, expr_type)?;
            matching_anomaly::check_unreachable_pattern(scope, arms, stmt.span, expr_type)?;
            let pattern_matrix = PatternMatrix::from_arms(scope, arms, expr_type);
            scope.root.map_pattern_matrix(stmt, pattern_matrix);
            Ok(())
        }
        _ => unreachable!(),
    }
}

fn match_pattern(
    scope: &mut BlockScope,
    pat: &Node<Pattern>,
    expected_type: TypeId,
) -> Result<IndexMap<SmolStr, Bind>, FatalError> {
    match &pat.kind {
        Pattern::WildCard => Ok(IndexMap::new()),

        Pattern::Literal(lit_pat) => {
            let lit_ty = match lit_pat.kind {
                LiteralPattern::Bool(_) => TypeId::bool(scope.db()),
            };
            if expected_type == lit_ty {
                Ok(IndexMap::new())
            } else {
                let err = scope.type_error("", pat.span, expected_type, lit_ty);
                Err(FatalError::new(err))
            }
        }

        Pattern::Tuple(elts) => {
            let expected_elts = if let Type::Tuple(tup) = expected_type.typ(scope.db()) {
                tup.items
            } else {
                let label_msg = format!(
                    "expected {}, but found tuple`",
                    expected_type.display(scope.db())
                );
                return Err(FatalError::new(scope.fancy_error(
                    "mismatched types",
                    vec![Label::primary(pat.span, label_msg)],
                    vec![],
                )));
            };

            match_tuple_pattern(scope, elts, &expected_elts, pat.span, None)
        }

        Pattern::Path(path) => match scope.maybe_resolve_path(&path.kind) {
            Some(NamedThing::EnumVariant(variant)) => {
                let db = scope.db();
                let parent_type = variant.parent(db).as_type(db);
                let kind = variant.kind(db)?;
                if kind != EnumVariantKind::Unit {
                    let variant_kind_name = kind.display_name();
                    let err = scope.fancy_error(
                        "expected an unit variant",
                        vec![
                            Label::primary(
                                path.span,
                                &format!("the variant is defined as {variant_kind_name}"),
                            ),
                            Label::secondary(
                                variant.span(scope.db()),
                                &format! {"{} is defined here", variant.name(scope.db())},
                            ),
                        ],
                        vec![],
                    );
                    return Err(FatalError::new(err));
                }

                if parent_type != expected_type {
                    let err = scope.type_error("", pat.span, expected_type, parent_type);
                    Err(FatalError::new(err))
                } else {
                    Ok(IndexMap::new())
                }
            }

            Some(NamedThing::Variable { name, span, .. }) => {
                let err = scope.duplicate_name_error(
                    &format!("`{name}` is already defined"),
                    &name,
                    span,
                    pat.span,
                );
                Err(FatalError::new(err))
            }

            None if path.kind.segments.len() == 1 => {
                let name = path.kind.segments[0].kind.clone();
                let bind = Bind::new(name.clone(), expected_type, pat.span);
                let mut binds = IndexMap::new();
                binds.insert(name, bind);
                Ok(binds)
            }

            None => {
                let path = &path.kind;
                let err = scope.fancy_error(
                    &format! {"failed to resolve `{path}`"},
                    vec![Label::primary(
                        pat.span,
                        &format!("use of undeclared type `{path}`"),
                    )],
                    vec![],
                );
                Err(FatalError::new(err))
            }

            _ => {
                let err = scope.fancy_error(
                    "expected enum variant or variable",
                    vec![Label::primary(
                        pat.span,
                        &format!("`{}` is not a enum variant or variable", path.kind),
                    )],
                    vec![],
                );
                Err(FatalError::new(err))
            }
        },

        Pattern::PathTuple(path, pat_elts) => {
            let variant = match scope.resolve_path(&path.kind, path.span) {
                Some(NamedThing::EnumVariant(variant)) => variant,
                _ => {
                    let err = scope.fancy_error(
                        "expected enum variant",
                        vec![Label::primary(path.span, "expected enum variant here")],
                        vec![],
                    );
                    return Err(FatalError::new(err));
                }
            };

            let parent_type = variant.parent(scope.db()).as_type(scope.db());
            if parent_type != expected_type {
                let err = scope.type_error("", pat.span, expected_type, parent_type);
                return Err(FatalError::new(err));
            }

            let variant_kind = variant.kind(scope.db())?;
            let ty_elts = match variant_kind {
                EnumVariantKind::Tuple(types) => types,
                EnumVariantKind::Unit => {
                    let variant_kind_name = variant_kind.display_name();
                    let err = scope.fancy_error(
                        "expected a tuple variant",
                        vec![
                            Label::primary(
                                path.span,
                                &format!("the variant is defined as {variant_kind_name}"),
                            ),
                            Label::secondary(
                                variant.span(scope.db()),
                                &format! {"{} is defined here", variant.name(scope.db())},
                            ),
                        ],
                        vec![],
                    );
                    return Err(FatalError::new(err));
                }
            };

            match_tuple_pattern(scope, pat_elts, &ty_elts, pat.span, Some(variant))
        }

        Pattern::Or(sub_pats) => {
            let mut subpat_binds = vec![];
            let mut all_variables = IndexSet::new();

            // Collect binds and variable names in the all sub patterns.
            for sub_pat in sub_pats {
                let pattern = match_pattern(scope, sub_pat, expected_type)?;
                for var in pattern.keys() {
                    all_variables.insert(var.clone());
                }

                subpat_binds.push((sub_pat, pattern));
            }

            // Check all variables are defined in the all sub patterns.
            let mut err = None;
            for var in all_variables.iter() {
                for (subpat, binds) in subpat_binds.iter() {
                    if !binds.contains_key(var) {
                        err = Some(scope.fancy_error(
                            &format!("variable `{}` is not bound in all sub patterns", var),
                            vec![Label::primary(
                                subpat.span,
                                &format!("variable `{}` is not bound here", var),
                            )],
                            vec![],
                        ));
                    }
                }
            }
            if let Some(err) = err {
                return Err(FatalError::new(err));
            }

            // Check all variables has the same type.
            err = None;
            for var in all_variables.iter() {
                let first_bind = &subpat_binds.first().unwrap().1[var];
                let ty = first_bind.ty;
                for (_, binds) in subpat_binds.iter().skip(1) {
                    let bind = &binds[var];
                    if bind.ty != ty {
                        bind.spans.iter().for_each(|span| {
                            err = Some(scope.type_error(
                                &format! {"mismatched type for `{}` between sub patterns", var},
                                *span,
                                ty,
                                bind.ty,
                            ));
                        });
                    }
                }
            }
            if let Some(err) = err {
                return Err(FatalError::new(err));
            }

            // Merge subpat binds.
            let mut result: IndexMap<SmolStr, Bind> = IndexMap::new();
            for (_, binds) in subpat_binds {
                for (name, bind) in binds {
                    result
                        .entry(name)
                        .and_modify(|entry| entry.spans.extend_from_slice(&bind.spans))
                        .or_insert(bind);
                }
            }
            Ok(result)
        }
    }
}

fn match_tuple_pattern(
    scope: &mut BlockScope,
    tuple_elts: &[Node<Pattern>],
    expected_elts: &[TypeId],
    pat_span: Span,
    variant: Option<EnumVariantId>,
) -> Result<IndexMap<SmolStr, Bind>, FatalError> {
    if tuple_elts.len() != expected_elts.len() {
        let mut labels = vec![Label::primary(
            pat_span,
            &format! {"expected {} elements, but {}", expected_elts.len(), tuple_elts.len()},
        )];
        if let Some(variant) = variant {
            labels.push(Label::secondary(
                variant.span(scope.db()),
                &format! {"{} is defined here", variant.name(scope.db())},
            ));
        }

        let err = scope.fancy_error("the number of tuple variant mismatch", labels, vec![]);
        return Err(FatalError::new(err));
    }

    let mut binds: IndexMap<SmolStr, Bind> = IndexMap::new();
    for (pat, &ty) in tuple_elts.iter().zip(expected_elts) {
        for (name, bind) in match_pattern(scope, pat, ty)?.into_iter() {
            match binds.entry(name) {
                Entry::Occupied(entry) => {
                    let original = entry.get();
                    let err = scope.fancy_error(
                        "same variable appears in the same pattern",
                        vec![
                            Label::primary(
                                bind.spans[0],
                                &format! {"{} is already defined", bind.name },
                            ),
                            Label::secondary(
                                original.spans[0],
                                &format! {"{} is originally defined here", original.name },
                            ),
                        ],
                        vec![],
                    );
                    return Err(FatalError::new(err));
                }
                Entry::Vacant(entry) => {
                    entry.insert(bind);
                }
            }
        }
    }

    Ok(binds)
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct Bind {
    name: SmolStr,
    ty: TypeId,
    spans: Vec<Span>,
}

impl Bind {
    fn new(name: SmolStr, ty: TypeId, span: Span) -> Self {
        Self {
            name,
            ty,
            spans: vec![span],
        }
    }
}

fn error_if_not_bool(scope: &mut BlockScope, typ: TypeId, span: Span, msg: &str) {
    if typ.typ(scope.db()) != Type::bool() {
        scope.type_error(msg, span, scope.db().intern_type(Type::bool()), typ);
    }
}

fn unsafe_block(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    match &stmt.kind {
        fe::FuncStmt::Unsafe(body) => {
            if scope.inherits_type(BlockScopeType::Unsafe) {
                scope.error(
                    "unnecessary `unsafe` block",
                    stmt.span,
                    "this `unsafe` block is nested inside another `unsafe` context",
                );
            }
            traverse_statements(&mut scope.new_child(BlockScopeType::Unsafe), body)
        }
        _ => unreachable!(),
    }
}

fn while_loop(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    match &stmt.kind {
        fe::FuncStmt::While { test, body } => {
            let test_type = expressions::value_expr(scope, test, None)?.typ;
            error_if_not_bool(
                scope,
                test_type,
                test.span,
                "`while` loop condition is not bool",
            );
            traverse_statements(&mut scope.new_child(BlockScopeType::Loop), body)?;
            Ok(())
        }
        _ => unreachable!(),
    }
}

fn emit(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::Emit { name, args } = &stmt.kind {
        match scope.resolve_name(&name.kind, name.span)? {
            None => {
                scope.error(
                    &format!("undefined event: `{}`", name.kind),
                    name.span,
                    "undefined event",
                );
            }
            Some(NamedThing::Item(Item::Event(event))) => {
                scope.root.add_emit(stmt, event);

                // Check visibility of event.
                if !event.is_public(scope.db()) && event.module(scope.db()) != scope.module() {
                    let module_name = event.module(scope.db()).name(scope.db());
                    scope.fancy_error(
                             &format!(
                                 "the event `{}` is private",
                                 name.kind,
                             ),
                             vec![
                                 Label::primary(name.span, "this event is not `pub`"),
                                 Label::secondary(
                                     event.data(scope.db()).ast.span,
                                     format!("`{}` is defined here", name.kind)
                                 ),
                             ],
                             vec![
                                 format!("`{}` can only be used within `{}`", name.kind, module_name),
                                 format!("Hint: use `pub event {event}` to make `{event}` visible from outside of `{module}`", event=name.kind, module=module_name),
                             ],
                         );
                }

                if let Some(context_type) = scope.get_context_type() {
                    // we add ctx to the list of expected params
                    let params_with_ctx = [
                        vec![EventField {
                            name: "ctx".into(),
                            typ: Ok(context_type),
                            is_indexed: false,
                        }],
                        event.typ(scope.db()).fields.clone(),
                    ]
                    .concat();
                    call_args::validate_named_args(
                        scope,
                        &name.kind,
                        name.span,
                        args,
                        &params_with_ctx,
                    )?;
                } else {
                    scope.fancy_error(
                        "`Context` is not defined",
                        vec![
                            Label::primary(
                                stmt.span,
                                "`ctx` must be defined and passed into the event",
                            ),
                            Label::secondary(
                                scope.parent_function().name_span(scope.db()),
                                "Note: declare `ctx` in this function signature",
                            ),
                            Label::secondary(
                                scope.parent_function().name_span(scope.db()),
                                "Example: `pub fn foo(ctx: Context, ...)`",
                            ),
                        ],
                        vec!["Example: emit MyEvent(ctx, ...)".into()],
                    );
                }
            }
            Some(named_thing) => {
                scope.error(
                    "`emit` expects an event",
                    name.span,
                    &format!(
                        "`{}` is a {} name; expected an event",
                        &name.kind,
                        named_thing.item_kind_display_name(),
                    ),
                );
            }
        }
        return Ok(());
    }
    unreachable!()
}

fn assert(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::Assert { test, msg } = &stmt.kind {
        let test_type = expressions::value_expr(scope, test, None)?.typ;
        error_if_not_bool(
            scope,
            test_type,
            test.span,
            "`assert` condition is not bool",
        );

        if let Some(msg) = msg {
            let msg_attributes = expressions::assignable_expr(scope, msg, None)?;
            if !matches!(msg_attributes.typ.typ(scope.db()), Type::String(_)) {
                scope.error(
                    "`assert` reason must be a string",
                    msg.span,
                    &format!(
                        "this has type `{}`; expected a string",
                        msg_attributes.typ.display(scope.db())
                    ),
                );
            }
        }

        return Ok(());
    }

    unreachable!()
}

fn revert(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::Revert { error } = &stmt.kind {
        if let Some(error_expr) = error {
            let error_attributes = expressions::assignable_expr(scope, error_expr, None)?;
            if error_attributes.typ.as_struct(scope.db()).is_none() {
                scope.error(
                    "`revert` error must be a struct",
                    error_expr.span,
                    &format!(
                        "this has type `{}`; expected a struct",
                        error_attributes.typ.display(scope.db())
                    ),
                );
            }
        }

        return Ok(());
    }

    unreachable!()
}

fn func_return(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::Return { value } = &stmt.kind {
        let expected_type = scope.root.function_return_type()?;

        let attributes = match value {
            Some(val) => expressions::assignable_expr(scope, val, Some(expected_type))?,
            None => ExpressionAttributes::new(TypeId::unit(scope.db()), Location::Value),
        };

        if attributes.typ != expected_type {
            scope.error(
                &format!(
                    "expected function to return `{}` but was `{}`",
                    expected_type.display(scope.db()),
                    attributes.typ.display(scope.db())
                ),
                stmt.span,
                "",
            );
        }

        return Ok(());
    }

    unreachable!()
}
