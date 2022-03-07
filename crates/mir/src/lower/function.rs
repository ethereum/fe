use std::{rc::Rc, str::FromStr};

use fe_analyzer::{
    builtins::{ContractTypeMethod, GlobalFunction, ValueMethod},
    context::CallType as AnalyzerCallType,
    namespace::{items as analyzer_items, types as analyzer_types},
};
use fe_parser::{ast, node::Node};
use fxhash::FxHashMap;
use id_arena::{Arena, Id};
use num_bigint::BigInt;
use smol_str::SmolStr;

use crate::{
    db::MirDb,
    ir::{
        self, body_builder::BodyBuilder, constant::ConstantValue, function::Linkage,
        inst::CallType, value::Local, BasicBlockId, Constant, FunctionBody, FunctionId,
        FunctionParam, FunctionSignature, SourceInfo, TypeId, ValueId,
    },
};

type ScopeId = Id<Scope>;

pub fn lower_func_signature(db: &dyn MirDb, func: analyzer_items::FunctionId) -> FunctionId {
    // TODO: Remove this when an analyzer's function signature contains `self` type.
    let mut params = vec![];
    let has_self = func.takes_self(db.upcast());
    if has_self {
        let self_ty = func.self_typ(db.upcast()).unwrap();
        let source = self_arg_source(db, func);
        params.push(make_param(db, "self", self_ty, source));
    }

    let analyzer_signature = func.signature(db.upcast());
    params.extend(analyzer_signature.params.iter().map(|param| {
        let source = arg_source(db, func, &param.name);
        make_param(db, param.name.clone(), param.typ.clone().unwrap(), source)
    }));
    let return_type = db.mir_lowered_type(analyzer_signature.return_type.clone().unwrap());

    let linkage = if func.is_public(db.upcast()) {
        if has_self {
            Linkage::Export
        } else {
            Linkage::Public
        }
    } else {
        Linkage::Private
    };

    let sig = FunctionSignature {
        params,
        return_type: Some(return_type),
        module_id: func.module(db.upcast()),
        analyzer_func_id: func,
        linkage,
        has_self,
    };

    db.mir_intern_function(sig.into())
}

pub fn lower_func_body(db: &dyn MirDb, func: FunctionId) -> Rc<FunctionBody> {
    let analyzer_func = func.analyzer_func(db);
    let ast = &analyzer_func.data(db.upcast()).ast;
    let analyzer_body = analyzer_func.body(db.upcast());

    let body = BodyLowerHelper::new(db, func, ast, analyzer_body.as_ref()).lower();
    body.into()
}

struct BodyLowerHelper<'db, 'a> {
    db: &'db dyn MirDb,
    builder: BodyBuilder,
    ast: &'a Node<ast::Function>,
    analyzer_body: &'a fe_analyzer::context::FunctionBody,
    scopes: Arena<Scope>,
    current_scope: ScopeId,
}

impl<'db, 'a> BodyLowerHelper<'db, 'a> {
    fn new(
        db: &'db dyn MirDb,
        func: FunctionId,
        ast: &'a Node<ast::Function>,
        analyzer_body: &'a fe_analyzer::context::FunctionBody,
    ) -> Self {
        let mut builder = BodyBuilder::new(func, ast.into());
        let mut scopes = Arena::new();

        // Make a root scope. A root scope collects function parameters and module
        // constants.
        let root = Scope::root(db, func, &mut builder);
        let current_scope = scopes.alloc(root);

        Self {
            db,
            builder,
            ast,
            analyzer_body,
            scopes,
            current_scope,
        }
    }

    fn lower(mut self) -> FunctionBody {
        for stmt in &self.ast.kind.body {
            self.lower_stmt(stmt)
        }

        let last_block = self.builder.current_block();
        if !self.builder.is_block_terminated(last_block) {
            let unit = self.make_unit();
            self.builder.ret(unit, SourceInfo::dummy());
        }

        self.builder.build()
    }

    fn lower_stmt(&mut self, stmt: &Node<ast::FuncStmt>) {
        match &stmt.kind {
            ast::FuncStmt::Return { value } => {
                let value = if let Some(expr) = value {
                    self.lower_expr(expr)
                } else {
                    self.make_unit()
                };
                self.builder.ret(value, stmt.into());
                let next_block = self.builder.make_block();
                self.builder.move_to_block(next_block);
            }

            ast::FuncStmt::VarDecl { target, value, .. } => {
                // Declare variables.
                let locals = self.lower_var_decl(target);

                // Lower an assignment statement in variable declaration.
                if let Some(expr) = value {
                    let mut rhs = self.lower_expr(expr);
                    let ty = self.builder.value_ty(rhs);
                    for (local, indices) in locals {
                        for index in indices {
                            let ty = ty.projection_ty(self.db, self.builder.value_data(rhs));
                            rhs = self
                                .builder
                                .aggregate_access(rhs, vec![index], ty, expr.into());
                        }
                        self.builder.assign(local, rhs, stmt.into());
                    }
                }
            }

            ast::FuncStmt::ConstantDecl { name, value, .. } => {
                let ty = self
                    .db
                    .mir_lowered_type(self.analyzer_body.var_types[&name.id].clone());

                let value = self.analyzer_body.expressions[&value.id]
                    .const_value
                    .clone()
                    .unwrap();

                let constant =
                    self.make_local_constant(name.kind.clone(), ty, value.into(), stmt.into());
                self.scope_mut().declare_var(&name.kind, constant);
            }

            ast::FuncStmt::Assign { target, value } => {
                let lhs = self.lower_expr(target);
                let rhs = self.lower_expr(value);
                self.builder.assign(lhs, rhs, stmt.into());
            }

            ast::FuncStmt::AugAssign { target, op, value } => {
                let lhs = self.lower_expr(target);
                let rhs = self.lower_expr(value);
                let ty = self.expr_ty(target);

                let tmp = self.lower_binop(op.kind, lhs, rhs, ty, stmt.into());
                self.builder.assign(lhs, tmp, stmt.into());
            }

            ast::FuncStmt::For { target, iter, body } => self.lower_for_loop(target, iter, body),

            ast::FuncStmt::While { test, body } => {
                let header_bb = self.builder.make_block();
                let exit_bb = self.builder.make_block();

                let cond = self.lower_expr(test);
                self.builder
                    .branch(cond, header_bb, exit_bb, SourceInfo::dummy());

                // Lower while body.
                self.builder.move_to_block(header_bb);
                self.enter_loop_scope(header_bb, exit_bb);
                for stmt in body {
                    self.lower_stmt(stmt);
                }
                let cond = self.lower_expr(test);
                self.builder
                    .branch(cond, header_bb, exit_bb, SourceInfo::dummy());

                self.exit_scope();

                // Move to while exit bb.
                self.builder.move_to_block(exit_bb);
            }

            ast::FuncStmt::If {
                test,
                body,
                or_else,
            } => self.lower_if(test, body, or_else),

            ast::FuncStmt::Assert { test, msg } => {
                let then_bb = self.builder.make_block();
                let false_bb = self.builder.make_block();

                let cond = self.lower_expr(test);
                self.builder
                    .branch(cond, then_bb, false_bb, SourceInfo::dummy());

                self.builder.move_to_block(false_bb);
                let msg = if let Some(msg) = msg {
                    self.lower_expr(msg)
                } else {
                    self.make_unit()
                };
                self.builder.revert(msg, stmt.into());

                self.builder.move_to_block(then_bb);
            }

            ast::FuncStmt::Emit { args, .. } => {
                let event_id = self.analyzer_body.emits[&stmt.id];
                let event_type = self.db.mir_lowered_event_type(event_id);
                // NOTE: Event arguments are guaranteed to be in the same order with
                // the definition.
                let lowered_args = args
                    .kind
                    .iter()
                    .map(|arg| self.lower_expr(&arg.kind.value))
                    .collect();

                let emit_arg =
                    self.builder
                        .aggregate_construct(event_type, lowered_args, args.into());
                self.builder.emit(emit_arg, stmt.into());
            }

            ast::FuncStmt::Expr { value } => {
                self.lower_expr(value);
            }

            ast::FuncStmt::Pass => {
                // TODO: Generate appropriate error message.
                let arg = self.make_unit();
                self.builder.revert(arg, stmt.into());
                let next_block = self.builder.make_block();
                self.builder.move_to_block(next_block);
            }

            ast::FuncStmt::Break => {
                let exit = self.scope().loop_exit(&self.scopes);
                self.builder.jump(exit, stmt.into());
                let next_block = self.builder.make_block();
                self.builder.move_to_block(next_block);
            }

            ast::FuncStmt::Continue => {
                let entry = self.scope().loop_entry(&self.scopes);
                if let Some(loop_idx) = self.scope().loop_idx(&self.scopes) {
                    let u256_ty = self.u256_ty();
                    let imm_one = self.builder.make_imm(1u32.into(), u256_ty);
                    let inc = self
                        .builder
                        .add(loop_idx, imm_one, u256_ty, SourceInfo::dummy());
                    self.builder.assign(loop_idx, inc, SourceInfo::dummy());
                    let maximum_iter_count = self.scope().maximum_iter_count(&self.scopes).unwrap();
                    let cond = self
                        .builder
                        .eq(loop_idx, maximum_iter_count, u256_ty, stmt.into());
                    let exit = self.scope().loop_exit(&self.scopes);
                    self.builder.branch(cond, exit, entry, stmt.into());
                } else {
                    self.builder.jump(entry, stmt.into())
                }
                let next_block = self.builder.make_block();
                self.builder.move_to_block(next_block);
            }

            ast::FuncStmt::Revert { error } => {
                let error = if let Some(error) = error {
                    self.lower_expr(error)
                } else {
                    self.make_unit()
                };

                self.builder.revert(error, stmt.into());
                let next_block = self.builder.make_block();
                self.builder.move_to_block(next_block);
            }

            ast::FuncStmt::Unsafe(stmts) => {
                self.enter_scope();
                for stmt in stmts {
                    self.lower_stmt(stmt)
                }
                self.exit_scope()
            }
        }
    }

    fn lower_var_decl(&mut self, var: &Node<ast::VarDeclTarget>) -> Vec<(ValueId, Vec<ValueId>)> {
        match &var.kind {
            ast::VarDeclTarget::Name(name) => {
                let ty = self
                    .db
                    .mir_lowered_type(self.analyzer_body.var_types[&var.id].clone());
                let local = Local::user_local(name.clone(), ty, var.into());

                let value = self.builder.declare(local);
                self.scope_mut().declare_var(name, value);

                vec![(value, vec![])]
            }

            ast::VarDeclTarget::Tuple(decls) => {
                let mut result = vec![];
                for (i, var) in decls.iter().enumerate() {
                    let u256_ty = self.u256_ty();
                    let index = self.builder.make_imm(i.into(), u256_ty);
                    self.lower_var_decl(var)
                        .into_iter()
                        .for_each(|(local, mut local_indices)| {
                            let mut indices = vec![index];
                            indices.append(&mut local_indices);
                            result.push((local, indices))
                        })
                }
                result
            }
        }
    }

    fn lower_if(
        &mut self,
        cond: &Node<ast::Expr>,
        then: &[Node<ast::FuncStmt>],
        else_: &[Node<ast::FuncStmt>],
    ) {
        let cond = self.lower_expr(cond);

        if else_.is_empty() {
            let then_bb = self.builder.make_block();
            let merge_bb = self.builder.make_block();

            self.builder
                .branch(cond, then_bb, merge_bb, SourceInfo::dummy());

            // Lower then block.
            self.builder.move_to_block(then_bb);
            self.enter_scope();
            for stmt in then {
                self.lower_stmt(stmt);
            }
            self.builder.jump(merge_bb, SourceInfo::dummy());
            self.builder.move_to_block(merge_bb);
            self.exit_scope();
        } else {
            let then_bb = self.builder.make_block();
            let else_bb = self.builder.make_block();

            self.builder
                .branch(cond, then_bb, else_bb, SourceInfo::dummy());

            // Lower then block.
            self.builder.move_to_block(then_bb);
            self.enter_scope();
            for stmt in then {
                self.lower_stmt(stmt);
            }
            self.exit_scope();
            let then_block_end_bb = self.builder.current_block();

            // Lower else_block.
            self.builder.move_to_block(else_bb);
            self.enter_scope();
            for stmt in else_ {
                self.lower_stmt(stmt);
            }
            self.exit_scope();
            let else_block_end_bb = self.builder.current_block();

            let merge_bb = self.builder.make_block();
            self.builder.move_to_block(then_block_end_bb);
            self.builder.jump(merge_bb, SourceInfo::dummy());
            self.builder.move_to_block(else_block_end_bb);
            self.builder.jump(merge_bb, SourceInfo::dummy());
            self.builder.move_to_block(merge_bb);
        }
    }

    // NOTE: we assume a type of `iter` is array.
    // TODO: Desugar to `loop` + `match` like rustc in HIR to generate better MIR.
    fn lower_for_loop(
        &mut self,
        loop_variable: &Node<SmolStr>,
        iter: &Node<ast::Expr>,
        body: &[Node<ast::FuncStmt>],
    ) {
        let preheader_bb = self.builder.make_block();
        let entry_bb = self.builder.make_block();
        let exit_bb = self.builder.make_block();
        let iter_elem_ty = self.analyzer_body.var_types[&loop_variable.id].clone();
        let iter_elem_ty = self.db.mir_lowered_type(iter_elem_ty);

        self.builder.jump(preheader_bb, SourceInfo::dummy());

        // `For` has its scope from preheader block.
        self.enter_loop_scope(entry_bb, exit_bb);

        /* Lower preheader. */
        self.builder.move_to_block(preheader_bb);

        // Declare loop_variable.
        let loop_value = self.builder.declare(Local::user_local(
            loop_variable.kind.clone(),
            iter_elem_ty,
            loop_variable.into(),
        ));
        self.scope_mut()
            .declare_var(&loop_variable.kind, loop_value);

        // Declare and initialize `loop_idx` to 0.
        let u256_ty = self.u256_ty();
        let loop_idx = Local::tmp_local("$loop_idx_tmp".into(), u256_ty);
        let loop_idx = self.builder.declare(loop_idx);
        let imm_zero = self.builder.make_imm(0u32.into(), u256_ty);
        self.builder.assign(loop_idx, imm_zero, SourceInfo::dummy());

        // Evaluates loop variable.
        let iter = self.lower_expr(iter);

        // Create maximum loop count.
        let iter_ty = self.builder.value_ty(iter);
        let maximum_iter_count = match iter_ty.data(self.db).as_ref() {
            ir::Type::Array(ir::types::ArrayDef { len, .. }) => *len,
            _ => unreachable!(),
        };
        let maximum_iter_count = self.builder.make_imm(maximum_iter_count.into(), u256_ty);
        let cond = self
            .builder
            .eq(loop_idx, maximum_iter_count, u256_ty, SourceInfo::dummy());
        self.builder
            .branch(cond, exit_bb, entry_bb, SourceInfo::dummy());
        self.scope_mut().loop_idx = Some(loop_idx);
        self.scope_mut().maximum_iter_count = Some(maximum_iter_count);

        /* Lower body. */
        self.builder.move_to_block(entry_bb);

        // loop_variable = array[loop_idx]
        let iter_elem =
            self.builder
                .aggregate_access(iter, vec![loop_idx], iter_elem_ty, SourceInfo::dummy());
        self.builder
            .assign(loop_value, iter_elem, SourceInfo::dummy());

        for stmt in body {
            self.lower_stmt(stmt);
        }

        // loop_idx += 1
        let imm_one = self.builder.make_imm(1u32.into(), u256_ty);
        let inc = self
            .builder
            .add(loop_idx, imm_one, u256_ty, SourceInfo::dummy());
        self.builder.assign(loop_idx, inc, SourceInfo::dummy());
        let cond = self
            .builder
            .eq(loop_idx, maximum_iter_count, u256_ty, SourceInfo::dummy());
        self.builder
            .branch(cond, exit_bb, entry_bb, SourceInfo::dummy());

        /* Move to exit bb */
        self.exit_scope();
        self.builder.move_to_block(exit_bb);
    }

    fn lower_expr(&mut self, expr: &Node<ast::Expr>) -> ValueId {
        let ty = self.expr_ty(expr);
        match &expr.kind {
            ast::Expr::Ternary {
                if_expr,
                test,
                else_expr,
            } => {
                let true_bb = self.builder.make_block();
                let false_bb = self.builder.make_block();
                let merge_bb = self.builder.make_block();

                let tmp = self
                    .builder
                    .declare(Local::tmp_local("$ternary_tmp".into(), ty));

                let cond = self.lower_expr(test);
                self.builder
                    .branch(cond, true_bb, false_bb, SourceInfo::dummy());

                self.builder.move_to_block(true_bb);
                let value = self.lower_expr(if_expr);
                self.builder.assign(tmp, value, SourceInfo::dummy());
                self.builder.jump(merge_bb, SourceInfo::dummy());

                self.builder.move_to_block(false_bb);
                let value = self.lower_expr(else_expr);
                self.builder.assign(tmp, value, SourceInfo::dummy());
                self.builder.jump(merge_bb, SourceInfo::dummy());

                self.builder.move_to_block(merge_bb);
                tmp
            }

            ast::Expr::BoolOperation { left, op, right } => {
                self.lower_bool_op(op.kind, left, right, ty)
            }

            ast::Expr::BinOperation { left, op, right } => {
                let lhs = self.lower_expr(left);
                let rhs = self.lower_expr(right);
                self.lower_binop(op.kind, lhs, rhs, ty, expr.into())
            }

            ast::Expr::UnaryOperation { op, operand } => {
                let value = self.lower_expr(operand);
                match op.kind {
                    ast::UnaryOperator::Invert => self.builder.inv(value, ty, expr.into()),
                    ast::UnaryOperator::Not => self.builder.not(value, ty, expr.into()),
                    ast::UnaryOperator::USub => self.builder.neg(value, ty, expr.into()),
                }
            }

            ast::Expr::CompOperation { left, op, right } => {
                let lhs = self.lower_expr(left);
                let rhs = self.lower_expr(right);
                self.lower_comp_op(op.kind, lhs, rhs, ty, expr.into())
            }

            ast::Expr::Attribute { .. } => {
                let mut indices = vec![];
                let value = self.lower_aggregate_access(expr, &mut indices);
                let ty = self.expr_ty(expr);
                self.builder
                    .aggregate_access(value, indices, ty, expr.into())
            }

            ast::Expr::Subscript { value, index } => {
                let result_ty = self.expr_ty(expr);

                if !self.expr_ty(value).is_aggregate(self.db) {
                    // Indices is empty is the `expr` is map
                    let value = self.lower_expr(value);
                    let key = self.lower_expr(index);
                    self.builder.map_access(value, key, result_ty, expr.into())
                } else {
                    let mut indices = vec![];
                    let value = self.lower_aggregate_access(expr, &mut indices);
                    self.builder
                        .aggregate_access(value, indices, result_ty, expr.into())
                }
            }

            ast::Expr::Call {
                func,
                generic_args,
                args,
            } => {
                let ty = self.expr_ty(expr);
                self.lower_call(func, generic_args, &args.kind, ty, expr.into())
            }

            ast::Expr::List { elts } | ast::Expr::Tuple { elts } => {
                let args = elts.iter().map(|elem| self.lower_expr(elem)).collect();
                self.builder.aggregate_construct(ty, args, expr.into())
            }

            ast::Expr::Bool(b) => self.builder.make_imm_from_bool(*b, ty),

            ast::Expr::Name(name) => self.resolve_name(name),

            ast::Expr::Path(path) => self.resolve_path(path),

            ast::Expr::Num(num) => {
                let imm = BigInt::from_str(num).unwrap();
                self.builder.make_imm(imm, ty)
            }

            ast::Expr::Str(s) => {
                let ty = self.expr_ty(expr);
                self.make_local_constant(
                    "str_in_func".into(),
                    ty,
                    ConstantValue::Str(s.clone()),
                    expr.into(),
                )
            }

            ast::Expr::Unit => self.make_unit(),
        }
    }

    fn expr_ty(&self, expr: &Node<ast::Expr>) -> TypeId {
        let analyzer_ty = self.analyzer_body.expressions[&expr.id].typ.clone();
        self.db.mir_lowered_type(analyzer_ty)
    }

    fn lower_bool_op(
        &mut self,
        op: ast::BoolOperator,
        lhs: &Node<ast::Expr>,
        rhs: &Node<ast::Expr>,
        ty: TypeId,
    ) -> ValueId {
        let true_bb = self.builder.make_block();
        let false_bb = self.builder.make_block();
        let merge_bb = self.builder.make_block();

        let lhs = self.lower_expr(lhs);
        let tmp = self
            .builder
            .declare(Local::tmp_local(format!("${}_tmp", op).into(), ty));

        match op {
            ast::BoolOperator::And => {
                self.builder
                    .branch(lhs, true_bb, false_bb, SourceInfo::dummy());

                self.builder.move_to_block(true_bb);
                let rhs = self.lower_expr(rhs);
                self.builder.assign(tmp, rhs, SourceInfo::dummy());
                self.builder.jump(merge_bb, SourceInfo::dummy());

                self.builder.move_to_block(false_bb);
                let false_imm = self.builder.make_imm_from_bool(false, ty);
                self.builder.assign(tmp, false_imm, SourceInfo::dummy());
                self.builder.jump(merge_bb, SourceInfo::dummy());
            }

            ast::BoolOperator::Or => {
                self.builder
                    .branch(lhs, true_bb, false_bb, SourceInfo::dummy());

                self.builder.move_to_block(true_bb);
                let true_imm = self.builder.make_imm_from_bool(true, ty);
                self.builder.assign(tmp, true_imm, SourceInfo::dummy());
                self.builder.jump(merge_bb, SourceInfo::dummy());

                self.builder.move_to_block(false_bb);
                let rhs = self.lower_expr(rhs);
                self.builder.assign(tmp, rhs, SourceInfo::dummy());
                self.builder.jump(merge_bb, SourceInfo::dummy());
            }
        }

        self.builder.move_to_block(merge_bb);
        tmp
    }

    fn lower_binop(
        &mut self,
        op: ast::BinOperator,
        lhs: ValueId,
        rhs: ValueId,
        ty: TypeId,
        source: SourceInfo,
    ) -> ValueId {
        match op {
            ast::BinOperator::Add => self.builder.add(lhs, rhs, ty, source),
            ast::BinOperator::Sub => self.builder.sub(lhs, rhs, ty, source),
            ast::BinOperator::Mult => self.builder.mul(lhs, rhs, ty, source),
            ast::BinOperator::Div => self.builder.div(lhs, rhs, ty, source),
            ast::BinOperator::Mod => self.builder.modulo(lhs, rhs, ty, source),
            ast::BinOperator::Pow => self.builder.pow(lhs, rhs, ty, source),
            ast::BinOperator::LShift => self.builder.shl(lhs, rhs, ty, source),
            ast::BinOperator::RShift => self.builder.shr(lhs, rhs, ty, source),
            ast::BinOperator::BitOr => self.builder.bit_or(lhs, rhs, ty, source),
            ast::BinOperator::BitXor => self.builder.bit_xor(lhs, rhs, ty, source),
            ast::BinOperator::BitAnd => self.builder.bit_and(lhs, rhs, ty, source),
        }
    }

    fn lower_comp_op(
        &mut self,
        op: ast::CompOperator,
        lhs: ValueId,
        rhs: ValueId,
        ty: TypeId,
        source: SourceInfo,
    ) -> ValueId {
        match op {
            ast::CompOperator::Eq => self.builder.eq(lhs, rhs, ty, source),
            ast::CompOperator::NotEq => self.builder.ne(lhs, rhs, ty, source),
            ast::CompOperator::Lt => self.builder.lt(lhs, rhs, ty, source),
            ast::CompOperator::LtE => self.builder.le(lhs, rhs, ty, source),
            ast::CompOperator::Gt => self.builder.gt(lhs, rhs, ty, source),
            ast::CompOperator::GtE => self.builder.ge(lhs, rhs, ty, source),
        }
    }

    fn lower_call(
        &mut self,
        func: &Node<ast::Expr>,
        _generic_args: &Option<Node<Vec<ast::GenericArg>>>,
        args: &[Node<ast::CallArg>],
        ty: TypeId,
        source: SourceInfo,
    ) -> ValueId {
        let call_type = &self.analyzer_body.calls[&func.id];

        let mut args: Vec<_> = args
            .iter()
            .map(|arg| self.lower_expr(&arg.kind.value))
            .collect();

        match call_type {
            AnalyzerCallType::BuiltinFunction(GlobalFunction::Keccak256) => {
                self.builder.keccak256(args[0], ty, source)
            }

            AnalyzerCallType::Intrinsic(intrinsic) => {
                self.builder
                    .yul_intrinsic((*intrinsic).into(), args, ty, source)
            }

            AnalyzerCallType::BuiltinValueMethod { method, .. } => {
                let arg = self.lower_method_receiver(func);
                match method {
                    ValueMethod::ToMem => self.builder.to_mem(arg, ty, source),
                    ValueMethod::Clone => self.builder.clone(arg, ty, source),
                    ValueMethod::AbiEncode => self.builder.abi_encode(arg, ty, source),
                }
            }

            AnalyzerCallType::BuiltinAssociatedFunction { contract, function } => match function {
                ContractTypeMethod::Create => self.builder.create(args[0], *contract, ty, source),
                ContractTypeMethod::Create2 => self
                    .builder
                    .create2(args[0], args[1], *contract, ty, source),
            },

            AnalyzerCallType::AssociatedFunction { function, .. }
            | AnalyzerCallType::Pure(function) => {
                let func_id = self.db.mir_lowered_func_signature(*function);
                self.builder
                    .call(func_id, args, CallType::Internal, ty, source)
            }

            AnalyzerCallType::ValueMethod { method, .. } => {
                let mut method_args = vec![self.lower_method_receiver(func)];
                method_args.append(&mut args);

                let func_id = self.db.mir_lowered_func_signature(*method);
                self.builder
                    .call(func_id, method_args, CallType::Internal, ty, source)
            }

            AnalyzerCallType::External { function, .. } => {
                let mut method_args = vec![self.lower_method_receiver(func)];
                method_args.append(&mut args);
                let func_id = self.db.mir_lowered_func_signature(*function);
                self.builder
                    .call(func_id, method_args, CallType::External, ty, source)
            }

            AnalyzerCallType::TypeConstructor(_) => {
                if ty.is_primitive(self.db) {
                    let arg = args[0];
                    let arg_ty = self.builder.value_ty(arg);
                    if arg_ty == ty {
                        arg
                    } else {
                        self.builder.cast(arg, ty, source)
                    }
                } else if ty.is_aggregate(self.db) {
                    self.builder.aggregate_construct(ty, args, source)
                } else {
                    unreachable!()
                }
            }
        }
    }

    // FIXME: This is ugly hack to properly analyze method call. Remove this when  https://github.com/ethereum/fe/issues/670 is resolved.
    fn lower_method_receiver(&mut self, receiver: &Node<ast::Expr>) -> ValueId {
        match &receiver.kind {
            ast::Expr::Attribute { value, .. } => self.lower_expr(value),
            _ => unreachable!(),
        }
    }

    fn lower_aggregate_access(
        &mut self,
        expr: &Node<ast::Expr>,
        indices: &mut Vec<ValueId>,
    ) -> ValueId {
        match &expr.kind {
            ast::Expr::Attribute { value, attr } => {
                let index =
                    self.expr_ty(value)
                        .index_from_fname(self.db, &attr.kind, self.u256_ty());
                let value = self.lower_aggregate_access(value, indices);
                indices.push(self.builder.make_value(index));
                value
            }

            ast::Expr::Subscript { value, index } if self.expr_ty(value).is_aggregate(self.db) => {
                let value = self.lower_aggregate_access(value, indices);
                indices.push(self.lower_expr(index));
                value
            }

            _ => self.lower_expr(expr),
        }
    }

    fn make_unit(&mut self) -> ValueId {
        let unit_ty = analyzer_types::Type::Base(analyzer_types::Base::Unit);
        let unit_ty = self.db.mir_lowered_type(unit_ty);
        self.builder.make_unit(unit_ty)
    }

    fn make_local_constant(
        &mut self,
        name: SmolStr,
        ty: TypeId,
        value: ConstantValue,
        source: SourceInfo,
    ) -> ValueId {
        let function_id = self.builder.func_id();
        let constant = Constant {
            name,
            value,
            ty,
            module_id: function_id.module(self.db),
            source,
        };

        let constant_id = self.db.mir_intern_const(constant.into());
        self.builder.make_constant(constant_id, ty)
    }

    fn u256_ty(&mut self) -> TypeId {
        self.db.mir_intern_type(ir::Type::U256.into())
    }

    fn enter_scope(&mut self) {
        let new_scope = Scope::with_parent(self.current_scope);
        self.current_scope = self.scopes.alloc(new_scope);
    }

    fn enter_loop_scope(&mut self, entry: BasicBlockId, exit: BasicBlockId) {
        let new_scope = Scope::loop_scope(self.current_scope, entry, exit);
        self.current_scope = self.scopes.alloc(new_scope);
    }

    fn exit_scope(&mut self) {
        self.current_scope = self.scopes[self.current_scope].parent.unwrap();
    }

    /// Resolve a name appeared in an expression.
    /// NOTE: Don't call this to resolve method receiver.
    fn resolve_name(&mut self, name: &str) -> ValueId {
        if let Some(value) = self.scopes[self.current_scope].resolve_name(&self.scopes, name) {
            // Name is defined in local.
            value
        } else {
            // Name is defined in global.
            let func_id = self.builder.func_id();
            let module = func_id.module(self.db);
            let constant = match module
                .resolve_name(self.db.upcast(), name)
                .unwrap()
                .unwrap()
            {
                analyzer_items::Item::Constant(id) => self.db.mir_lowered_constant(id),
                _ => panic!("name defined in global must be constant"),
            };
            let ty = constant.ty(self.db);
            self.builder.make_constant(constant, ty)
        }
    }

    /// Resolve a path appeared in an expression.
    /// NOTE: Don't call this to resolve method receiver.
    fn resolve_path(&mut self, path: &ast::Path) -> ValueId {
        let func_id = self.builder.func_id();
        let module = func_id.module(self.db);
        let constant = match module.resolve_path(self.db.upcast(), path).value.unwrap() {
            analyzer_items::Item::Constant(id) => self.db.mir_lowered_constant(id),
            _ => panic!("path defined in global must be constant"),
        };
        let ty = constant.ty(self.db);
        self.builder.make_constant(constant, ty)
    }

    fn scope(&self) -> &Scope {
        &self.scopes[self.current_scope]
    }

    fn scope_mut(&mut self) -> &mut Scope {
        &mut self.scopes[self.current_scope]
    }
}

#[derive(Debug)]
struct Scope {
    parent: Option<ScopeId>,
    loop_entry: Option<BasicBlockId>,
    loop_exit: Option<BasicBlockId>,
    variables: FxHashMap<SmolStr, ValueId>,
    // TODO: Remove the below two fields when `for` loop desugaring is implemented.
    loop_idx: Option<ValueId>,
    maximum_iter_count: Option<ValueId>,
}

impl Scope {
    fn root(db: &dyn MirDb, func: FunctionId, builder: &mut BodyBuilder) -> Self {
        let mut root = Self {
            parent: None,
            loop_entry: None,
            loop_exit: None,
            variables: FxHashMap::default(),
            loop_idx: None,
            maximum_iter_count: None,
        };

        // Declare function parameters.
        for param in &func.signature(db).params {
            let local = Local::arg_local(param.name.clone(), param.ty, param.source.clone());
            let value_id = builder.store_func_arg(local);
            root.declare_var(&param.name, value_id)
        }

        root
    }

    fn with_parent(parent: ScopeId) -> Self {
        Self {
            parent: parent.into(),
            loop_entry: None,
            loop_exit: None,
            variables: FxHashMap::default(),
            loop_idx: None,
            maximum_iter_count: None,
        }
    }

    fn loop_scope(parent: ScopeId, loop_entry: BasicBlockId, loop_exit: BasicBlockId) -> Self {
        Self {
            parent: parent.into(),
            loop_entry: loop_entry.into(),
            loop_exit: loop_exit.into(),
            variables: FxHashMap::default(),
            loop_idx: None,
            maximum_iter_count: None,
        }
    }

    fn loop_entry(&self, scopes: &Arena<Scope>) -> BasicBlockId {
        match self.loop_entry {
            Some(entry) => entry,
            None => scopes[self.parent.unwrap()].loop_entry(scopes),
        }
    }

    fn loop_exit(&self, scopes: &Arena<Scope>) -> BasicBlockId {
        match self.loop_exit {
            Some(exit) => exit,
            None => scopes[self.parent.unwrap()].loop_exit(scopes),
        }
    }

    fn loop_idx(&self, scopes: &Arena<Scope>) -> Option<ValueId> {
        match self.loop_idx {
            Some(idx) => Some(idx),
            None => scopes[self.parent?].loop_idx(scopes),
        }
    }

    fn maximum_iter_count(&self, scopes: &Arena<Scope>) -> Option<ValueId> {
        match self.maximum_iter_count {
            Some(count) => Some(count),
            None => scopes[self.parent?].maximum_iter_count(scopes),
        }
    }

    fn declare_var(&mut self, name: &SmolStr, value: ValueId) {
        debug_assert!(!self.variables.contains_key(name));

        self.variables.insert(name.clone(), value);
    }

    fn resolve_name(&self, scopes: &Arena<Scope>, name: &str) -> Option<ValueId> {
        match self.variables.get(name) {
            Some(id) => Some(*id),
            None => scopes[self.parent?].resolve_name(scopes, name),
        }
    }
}

fn self_arg_source(db: &dyn MirDb, func: analyzer_items::FunctionId) -> SourceInfo {
    func.data(db.upcast())
        .ast
        .kind
        .args
        .iter()
        .find(|arg| matches!(arg.kind, ast::FunctionArg::Self_))
        .unwrap()
        .into()
}

fn arg_source(db: &dyn MirDb, func: analyzer_items::FunctionId, arg_name: &str) -> SourceInfo {
    func.data(db.upcast())
        .ast
        .kind
        .args
        .iter()
        .find_map(|arg| match &arg.kind {
            ast::FunctionArg::Regular(ast::RegularFunctionArg { name, .. }) => {
                if name.kind == arg_name {
                    Some(name.into())
                } else {
                    None
                }
            }
            ast::FunctionArg::Self_ => None,
        })
        .unwrap()
}

fn make_param(
    db: &dyn MirDb,
    name: impl Into<SmolStr>,
    ty: impl Into<analyzer_types::Type>,
    source: SourceInfo,
) -> FunctionParam {
    FunctionParam {
        name: name.into(),
        ty: db.mir_lowered_type(ty.into()),
        source,
    }
}
