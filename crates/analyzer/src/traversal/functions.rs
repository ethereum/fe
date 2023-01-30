use crate::context::{AnalyzerContext, ExpressionAttributes, NamedThing};
use crate::display::Displayable;
use crate::errors::{self, FatalError, TypeCoercionError};
use crate::namespace::items::{EnumVariantId, EnumVariantKind, Item, StructId, TypeDef};
use crate::namespace::scopes::{BlockScope, BlockScopeType};
use crate::namespace::types::{Type, TypeId};
use crate::pattern_analysis::PatternMatrix;
use crate::traversal::{assignments, declarations, expressions, types};
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
            let iter_type = expressions::expr(scope, iter, None)?.typ;

            let target_type = match iter_type.deref(scope.db()).typ(scope.db()) {
                Type::Array(array) => {
                    if iter_type.is_sptr(scope.db()) {
                        scope.add_diagnostic(errors::to_mem_error(iter.span));
                    }
                    array.inner
                }
                _ => {
                    return Err(FatalError::new(scope.register_diag(errors::type_error(
                        "invalid `for` loop iterator type",
                        iter.span,
                        "array",
                        iter_type.display(scope.db()),
                    ))))
                }
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
            &format!("`{stmt_name}` outside of a loop"),
            stmt.span,
            &format!("`{stmt_name}` can only be used inside of a `for` or `while` loop"),
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
            expressions::error_if_not_bool(scope, test, "`if` statement condition is not bool")?;
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
            let expr_type = expressions::expr(scope, expr, None)?.typ.deref(scope.db());

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

        Pattern::Rest => Err(FatalError::new(scope.error(
            "`..` is not allowed here",
            pat.span,
            "rest pattern is only allowed in tuple pattern",
        ))),

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

            tuple_pattern(scope, elts, &expected_elts, pat.span, None)
        }

        Pattern::Path(path) => match scope.resolve_visible_path(&path.kind) {
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
                                format!("the variant is defined as {variant_kind_name}"),
                            ),
                            Label::secondary(
                                variant.span(scope.db()),
                                format! {"{} is defined here", variant.name(scope.db())},
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
                        format!("use of undeclared type `{path}`"),
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
                        format!("`{}` is not a enum variant or variable", path.kind),
                    )],
                    vec![],
                );
                Err(FatalError::new(err))
            }
        },

        Pattern::PathTuple(path, pat_elts) => {
            let variant = match scope.resolve_path(&path.kind, path.span)? {
                NamedThing::EnumVariant(variant) => variant,
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
                                format!("the variant is defined as {variant_kind_name}"),
                            ),
                            Label::secondary(
                                variant.span(scope.db()),
                                format! {"{} is defined here", variant.name(scope.db())},
                            ),
                        ],
                        vec![],
                    );
                    return Err(FatalError::new(err));
                }
            };

            tuple_pattern(scope, pat_elts, &ty_elts, pat.span, Some(variant))
        }

        Pattern::PathStruct {
            path,
            fields,
            has_rest,
        } => {
            let (sid, ty) = match scope.resolve_path(&path.kind, path.span)? {
                NamedThing::Item(Item::Type(TypeDef::Struct(sid))) => {
                    (sid, sid.as_type(scope.db()))
                }
                _ => {
                    let err = scope.fancy_error(
                        "expected struct type",
                        vec![Label::primary(
                            pat.span,
                            format!("`{}` is not a struct name", path.kind),
                        )],
                        vec![],
                    );
                    return Err(FatalError::new(err));
                }
            };

            if ty != expected_type {
                let err = scope.type_error("", pat.span, expected_type, ty);
                return Err(FatalError::new(err));
            }

            struct_pattern(scope, fields, *has_rest, sid, pat.span)
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
                            &format!("variable `{var}` is not bound in all sub patterns"),
                            vec![Label::primary(
                                subpat.span,
                                format!("variable `{var}` is not bound here"),
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
                                &format! {"mismatched type for `{var}` between sub patterns"},
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

fn struct_pattern(
    scope: &mut BlockScope,
    fields: &[(Node<SmolStr>, Node<Pattern>)],
    has_rest: bool,
    sid: StructId,
    pat_span: Span,
) -> Result<IndexMap<SmolStr, Bind>, FatalError> {
    let mut pat_fields: IndexMap<SmolStr, (Node<Pattern>, Span)> = IndexMap::new();
    let mut maybe_err = Ok(());
    for (name, pat) in fields.iter() {
        match pat_fields.entry(name.kind.clone()) {
            Entry::Occupied(entry) => {
                let err = scope.fancy_error(
                    &format!("duplicate field `{}` bound in the pattern", name.kind),
                    vec![
                        Label::primary(name.span, "multiple uses here"),
                        Label::secondary(entry.get().1, "first binding of the field"),
                    ],
                    vec![],
                );
                maybe_err = Err(FatalError::new(err));
            }
            Entry::Vacant(entry) => {
                entry.insert((pat.clone(), name.span));
            }
        }
    }

    maybe_err?;
    let mut maybe_err = Ok(());

    let fields_def = sid.fields(scope.db());
    let mut ordered_patterns = Vec::with_capacity(fields_def.len());
    let mut expected_types = Vec::with_capacity(fields_def.len());

    for (f_name, field) in sid.fields(scope.db()).iter() {
        let ty = match field.typ(scope.db()) {
            Ok(ty) => ty,
            Err(err) => {
                maybe_err = Err(err.into());
                continue;
            }
        };

        let pat = match pat_fields.remove(f_name) {
            Some((_, span)) if !field.is_public(scope.db()) => {
                let err = scope.fancy_error(
                    &format!("field `{f_name}` is not public field"),
                    vec![
                        Label::primary(span, format!("`{f_name}` is not public")),
                        Label::secondary(field.span(scope.db()), "field is defined here"),
                    ],
                    vec![],
                );
                maybe_err = Err(FatalError::new(err));
                continue;
            }
            Some((pat, _)) => pat,
            None => {
                if has_rest {
                    let dummy_span = Span::dummy();
                    Node::new(Pattern::WildCard, dummy_span)
                } else {
                    let err = scope.fancy_error(
                        &format!("missing field `{f_name}` in the pattern"),
                        vec![Label::primary(pat_span, "missing field")],
                        vec![],
                    );
                    maybe_err = Err(FatalError::new(err));
                    continue;
                }
            }
        };

        ordered_patterns.push(pat);
        expected_types.push(ty);
    }

    maybe_err?;

    collect_binds_from_pat_vec(scope, &ordered_patterns, &expected_types)
}

fn tuple_pattern(
    scope: &mut BlockScope,
    tuple_elts: &[Node<Pattern>],
    expected_tys: &[TypeId],
    pat_span: Span,
    variant: Option<EnumVariantId>,
) -> Result<IndexMap<SmolStr, Bind>, FatalError> {
    let mut rest_pat_pos: Option<(usize, Span)> = None;
    for (i, pat) in tuple_elts.iter().enumerate() {
        if pat.kind.is_rest() {
            if rest_pat_pos.is_some() {
                let err = scope.fancy_error(
                    "multiple rest patterns are not allowed",
                    vec![
                        Label::primary(pat.span, "multiple rest patterns are not allowed"),
                        Label::secondary(rest_pat_pos.unwrap().1, "first rest pattern is here"),
                    ],
                    vec![],
                );
                return Err(FatalError::new(err));
            } else {
                rest_pat_pos = Some((i, pat.span));
            }
        }
    }

    let emit_len_error = |actual, expected| {
        let mut labels = vec![Label::primary(
            pat_span,
            format! {"expected {expected} elements, but {actual}"},
        )];
        if let Some(variant) = variant {
            labels.push(Label::secondary(
                variant.span(scope.db()),
                format! {"{} is defined here", variant.name(scope.db())},
            ));
        }

        let err = scope.fancy_error("the number of tuple variant mismatch", labels, vec![]);
        Err(FatalError::new(err))
    };

    if rest_pat_pos.is_some() && tuple_elts.len() - 1 > expected_tys.len() {
        return emit_len_error(tuple_elts.len() - 1, expected_tys.len());
    } else if rest_pat_pos.is_none() && tuple_elts.len() != expected_tys.len() {
        return emit_len_error(tuple_elts.len(), expected_tys.len());
    };

    collect_binds_from_pat_vec(scope, tuple_elts, expected_tys)
}

fn collect_binds_from_pat_vec(
    scope: &mut BlockScope,
    pats: &[Node<Pattern>],
    expected_types: &[TypeId],
) -> Result<IndexMap<SmolStr, Bind>, FatalError> {
    let mut binds: IndexMap<SmolStr, Bind> = IndexMap::new();
    let mut types_iter = expected_types.iter();
    for pat in pats.iter() {
        if pat.kind.is_rest() {
            let pat_num_in_rest = expected_types.len() - (pats.len() - 1);
            for _ in 0..pat_num_in_rest {
                types_iter.next();
            }
            continue;
        }

        for (name, bind) in match_pattern(scope, pat, *types_iter.next().unwrap())?.into_iter() {
            match binds.entry(name) {
                Entry::Occupied(entry) => {
                    let original = entry.get();
                    let err = scope.fancy_error(
                        "same variable appears in the same pattern",
                        vec![
                            Label::primary(
                                bind.spans[0],
                                format! {"{} is already defined", bind.name },
                            ),
                            Label::secondary(
                                original.spans[0],
                                format! {"{} is originally defined here", original.name },
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
            expressions::error_if_not_bool(scope, test, "`while` loop condition is not bool")?;
            traverse_statements(&mut scope.new_child(BlockScopeType::Loop), body)?;
            Ok(())
        }
        _ => unreachable!(),
    }
}

fn assert(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::Assert { test, msg } = &stmt.kind {
        expressions::error_if_not_bool(scope, test, "`assert` condition is not bool")?;

        if let Some(msg) = msg {
            let msg_attributes = expressions::expr(scope, msg, None)?;
            match msg_attributes.typ.typ(scope.db()) {
                Type::String(_) => {}
                Type::SPtr(inner) if matches!(inner.typ(scope.db()), Type::String(_)) => {
                    scope.add_diagnostic(errors::to_mem_error(msg.span));
                }
                _ => {
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
        }

        return Ok(());
    }

    unreachable!()
}

fn revert(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::Revert { error } = &stmt.kind {
        if let Some(error_expr) = error {
            let error_attr = expressions::expr(scope, error_expr, None)?;
            if !error_attr.typ.deref(scope.db()).is_struct(scope.db()) {
                scope.error(
                    "`revert` error must be a struct",
                    error_expr.span,
                    &format!(
                        "this has type `{}`; expected a struct",
                        error_attr.typ.deref(scope.db()).display(scope.db())
                    ),
                );
            } else if error_attr.typ.is_sptr(scope.db()) {
                scope.fancy_error(
                    "`revert` value must be copied to memory",
                    vec![Label::primary(error_expr.span, "this value is in storage")],
                    vec!["Hint: values located in storage can be copied to memory using the `to_mem` function.".into(),
                         format!("Example: `{}.to_mem()`", error_expr.kind),
                    ],
                );
            }
        }

        return Ok(());
    }

    unreachable!()
}

fn func_return(scope: &mut BlockScope, stmt: &Node<fe::FuncStmt>) -> Result<(), FatalError> {
    if let fe::FuncStmt::Return { value } = &stmt.kind {
        let expected_type = scope.root.function_return_type()?.deref(scope.db());

        let value_attr = match value {
            Some(val) => expressions::expr(scope, val, Some(expected_type))?,
            None => ExpressionAttributes::new(TypeId::unit(scope.db())),
        };

        match types::try_coerce_type(scope, value.as_ref(), value_attr.typ, expected_type, true) {
            Err(TypeCoercionError::RequiresToMem) => {
                let value = value.clone().expect("to_mem required on unit type?");
                scope.add_diagnostic(errors::to_mem_error(value.span));
            }
            Err(TypeCoercionError::Incompatible) => {
                scope.error(
                    &format!(
                        "expected function to return `{}` but was `{}`",
                        expected_type.display(scope.db()),
                        value_attr.typ.deref(scope.db()).display(scope.db())
                    ),
                    stmt.span,
                    "",
                );
            }
            Err(TypeCoercionError::SelfContractType) => {
                scope.add_diagnostic(errors::self_contract_type_error(
                    value.as_ref().unwrap().span,
                    &expected_type.display(scope.db()),
                ));
            }
            Ok(_) => {}
        }

        return Ok(());
    }

    unreachable!()
}
