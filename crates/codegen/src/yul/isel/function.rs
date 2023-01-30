#![allow(unused)]
use std::thread::Scope;

use super::{context::Context, inst_order::InstSerializer};
use fe_common::numeric::to_hex_str;

use fe_abi::function::{AbiFunction, AbiFunctionType};
use fe_common::db::Upcast;
use fe_mir::{
    ir::{
        self,
        constant::ConstantValue,
        inst::{BinOp, CallType, CastKind, InstKind, UnOp},
        value::AssignableValue,
        Constant, FunctionBody, FunctionId, FunctionSignature, InstId, Type, TypeId, TypeKind,
        Value, ValueId,
    },
    pretty_print::PrettyPrint,
};
use fxhash::FxHashMap;
use smol_str::SmolStr;
use yultsur::{
    yul::{self, Statement},
    *,
};

use crate::{
    db::CodegenDb,
    yul::isel::inst_order::StructuralInst,
    yul::slot_size::{function_hash_type, yul_primitive_type, SLOT_SIZE},
    yul::{
        runtime::{self, RuntimeProvider},
        YulVariable,
    },
};

pub fn lower_function(
    db: &dyn CodegenDb,
    ctx: &mut Context,
    function: FunctionId,
) -> yul::FunctionDefinition {
    debug_assert!(!ctx.lowered_functions.contains(&function));
    ctx.lowered_functions.insert(function);
    let sig = &db.codegen_legalized_signature(function);
    let body = &db.codegen_legalized_body(function);
    FuncLowerHelper::new(db, ctx, function, sig, body).lower_func()
}

struct FuncLowerHelper<'db, 'a> {
    db: &'db dyn CodegenDb,
    ctx: &'a mut Context,
    value_map: ScopedValueMap,
    func: FunctionId,
    sig: &'a FunctionSignature,
    body: &'a FunctionBody,
    ret_value: Option<yul::Identifier>,
    sink: Vec<yul::Statement>,
}

impl<'db, 'a> FuncLowerHelper<'db, 'a> {
    fn new(
        db: &'db dyn CodegenDb,
        ctx: &'a mut Context,
        func: FunctionId,
        sig: &'a FunctionSignature,
        body: &'a FunctionBody,
    ) -> Self {
        let mut value_map = ScopedValueMap::default();
        // Register arguments to value_map.
        for &value in body.store.locals() {
            match body.store.value_data(value) {
                Value::Local(local) if local.is_arg => {
                    let ident = YulVariable::new(local.name.as_str()).ident();
                    value_map.insert(value, ident);
                }
                _ => {}
            }
        }

        let ret_value = if sig.return_type.is_some() {
            Some(YulVariable::new("$ret").ident())
        } else {
            None
        };

        Self {
            db,
            ctx,
            value_map,
            func,
            sig,
            body,
            ret_value,
            sink: Vec::new(),
        }
    }

    fn lower_func(mut self) -> yul::FunctionDefinition {
        let name = identifier! { (self.db.codegen_function_symbol_name(self.func)) };

        let parameters = self
            .sig
            .params
            .iter()
            .map(|param| YulVariable::new(param.name.as_str()).ident())
            .collect();

        let ret = self
            .ret_value
            .clone()
            .map(|value| vec![value])
            .unwrap_or_default();

        let body = self.lower_body();

        yul::FunctionDefinition {
            name,
            parameters,
            returns: ret,
            block: body,
        }
    }

    fn lower_body(mut self) -> yul::Block {
        let inst_order = InstSerializer::new(self.body).serialize();

        for inst in inst_order {
            self.lower_structural_inst(inst)
        }

        yul::Block {
            statements: self.sink,
        }
    }

    fn lower_structural_inst(&mut self, inst: StructuralInst) {
        match inst {
            StructuralInst::Inst(inst) => self.lower_inst(inst),
            StructuralInst::If { cond, then, else_ } => {
                let if_block = self.lower_if(cond, then, else_);
                self.sink.push(if_block)
            }
            StructuralInst::Switch {
                scrutinee,
                table,
                default,
            } => {
                let switch_block = self.lower_switch(scrutinee, table, default);
                self.sink.push(switch_block)
            }
            StructuralInst::For { body } => {
                let for_block = self.lower_for(body);
                self.sink.push(for_block)
            }
            StructuralInst::Break => self.sink.push(yul::Statement::Break),
            StructuralInst::Continue => self.sink.push(yul::Statement::Continue),
        };
    }

    fn lower_inst(&mut self, inst: InstId) {
        if let Some(lhs) = self.body.store.inst_result(inst) {
            self.declare_assignable_value(lhs)
        }

        match &self.body.store.inst_data(inst).kind {
            InstKind::Declare { local } => self.declare_value(*local),

            InstKind::Unary { op, value } => {
                let inst_result = self.body.store.inst_result(inst).unwrap();
                let inst_result_ty = inst_result.ty(self.db.upcast(), &self.body.store);
                let result = self.lower_unary(*op, *value);
                self.assign_inst_result(inst, result, inst_result_ty.deref(self.db.upcast()))
            }

            InstKind::Binary { op, lhs, rhs } => {
                let inst_result = self.body.store.inst_result(inst).unwrap();
                let inst_result_ty = inst_result.ty(self.db.upcast(), &self.body.store);
                let result = self.lower_binary(*op, *lhs, *rhs, inst);
                self.assign_inst_result(inst, result, inst_result_ty.deref(self.db.upcast()))
            }

            InstKind::Cast { kind, value, to } => {
                let from_ty = self.body.store.value_ty(*value);
                let result = match kind {
                    CastKind::Primitive => {
                        debug_assert!(
                            from_ty.is_primitive(self.db.upcast())
                                && to.is_primitive(self.db.upcast())
                        );
                        let value = self.value_expr(*value);
                        self.ctx.runtime.primitive_cast(self.db, value, from_ty)
                    }
                    CastKind::Untag => {
                        let from_ty = from_ty.deref(self.db.upcast());
                        debug_assert!(from_ty.is_enum(self.db.upcast()));
                        let value = self.value_expr(*value);
                        let offset = literal_expression! {(from_ty.enum_data_offset(self.db.upcast(), SLOT_SIZE))};
                        expression! {add([value], [offset])}
                    }
                };

                self.assign_inst_result(inst, result, *to)
            }

            InstKind::AggregateConstruct { ty, args } => {
                let lhs = self.body.store.inst_result(inst).unwrap();
                let ptr = self.lower_assignable_value(lhs);
                let ptr_ty = lhs.ty(self.db.upcast(), &self.body.store);
                let arg_values = args.iter().map(|arg| self.value_expr(*arg)).collect();
                let arg_tys = args
                    .iter()
                    .map(|arg| self.body.store.value_ty(*arg))
                    .collect();
                self.sink.push(yul::Statement::Expression(
                    self.ctx
                        .runtime
                        .aggregate_init(self.db, ptr, arg_values, ptr_ty, arg_tys),
                ))
            }

            InstKind::Bind { src } => {
                match self.body.store.value_data(*src) {
                    Value::Constant { constant, .. } => {
                        // Need special handling when rhs is the string literal because it needs ptr
                        // copy.
                        if let ConstantValue::Str(s) = &constant.data(self.db.upcast()).value {
                            self.ctx.string_constants.insert(s.to_string());
                            let size = self.value_ty_size_deref(*src);
                            let lhs = self.body.store.inst_result(inst).unwrap();
                            let ptr = self.lower_assignable_value(lhs);
                            let inst_result_ty = lhs.ty(self.db.upcast(), &self.body.store);
                            self.sink.push(yul::Statement::Expression(
                                self.ctx.runtime.string_copy(
                                    self.db,
                                    ptr,
                                    s,
                                    inst_result_ty.is_sptr(self.db.upcast()),
                                ),
                            ))
                        } else {
                            let src_ty = self.body.store.value_ty(*src);
                            let src = self.value_expr(*src);
                            self.assign_inst_result(inst, src, src_ty)
                        }
                    }
                    _ => {
                        let src_ty = self.body.store.value_ty(*src);
                        let src = self.value_expr(*src);
                        self.assign_inst_result(inst, src, src_ty)
                    }
                }
            }

            InstKind::MemCopy { src } => {
                let lhs = self.body.store.inst_result(inst).unwrap();
                let dst_ptr = self.lower_assignable_value(lhs);
                let dst_ptr_ty = lhs.ty(self.db.upcast(), &self.body.store);
                let src_ptr = self.value_expr(*src);
                let src_ptr_ty = self.body.store.value_ty(*src);
                let ty_size = literal_expression! { (self.value_ty_size_deref(*src)) };
                self.sink
                    .push(yul::Statement::Expression(self.ctx.runtime.ptr_copy(
                        self.db,
                        src_ptr,
                        dst_ptr,
                        ty_size,
                        src_ptr_ty.is_sptr(self.db.upcast()),
                        dst_ptr_ty.is_sptr(self.db.upcast()),
                    )))
            }

            InstKind::Load { src } => {
                let src_ty = self.body.store.value_ty(*src);
                let src = self.value_expr(*src);
                debug_assert!(src_ty.is_ptr(self.db.upcast()));

                let result = self.body.store.inst_result(inst).unwrap();
                debug_assert!(!result
                    .ty(self.db.upcast(), &self.body.store)
                    .is_ptr(self.db.upcast()));
                self.assign_inst_result(inst, src, src_ty)
            }

            InstKind::AggregateAccess { value, indices } => {
                let base = self.value_expr(*value);
                let mut ptr = base;
                let mut inner_ty = self.body.store.value_ty(*value);
                for &idx in indices {
                    ptr = self.aggregate_elem_ptr(ptr, idx, inner_ty.deref(self.db.upcast()));
                    inner_ty =
                        inner_ty.projection_ty(self.db.upcast(), self.body.store.value_data(idx));
                }

                let result = self.body.store.inst_result(inst).unwrap();
                self.assign_inst_result(inst, ptr, inner_ty)
            }

            InstKind::MapAccess { value, key } => {
                let map_ty = self.body.store.value_ty(*value).deref(self.db.upcast());
                let value_expr = self.value_expr(*value);
                let key_expr = self.value_expr(*key);
                let key_ty = self.body.store.value_ty(*key);
                let ptr = self
                    .ctx
                    .runtime
                    .map_value_ptr(self.db, value_expr, key_expr, key_ty);
                let value_ty = match &map_ty.data(self.db.upcast()).kind {
                    TypeKind::Map(def) => def.value_ty,
                    _ => unreachable!(),
                };

                self.assign_inst_result(inst, ptr, value_ty.make_sptr(self.db.upcast()));
            }

            InstKind::Call {
                func,
                args,
                call_type,
            } => {
                let args: Vec<_> = args.iter().map(|arg| self.value_expr(*arg)).collect();
                let result = match call_type {
                    CallType::Internal => {
                        self.ctx.function_dependency.insert(*func);
                        let func_name = identifier! {(self.db.codegen_function_symbol_name(*func))};
                        expression! {[func_name]([args...])}
                    }
                    CallType::External => self.ctx.runtime.external_call(self.db, *func, args),
                };
                match self.db.codegen_legalized_signature(*func).return_type {
                    Some(mut result_ty) => {
                        if result_ty.is_aggregate(self.db.upcast())
                            | result_ty.is_string(self.db.upcast())
                        {
                            result_ty = result_ty.make_mptr(self.db.upcast());
                        }
                        self.assign_inst_result(inst, result, result_ty)
                    }
                    _ => self.sink.push(Statement::Expression(result)),
                }
            }

            InstKind::Revert { arg } => match arg {
                Some(arg) => {
                    let arg_ty = self.body.store.value_ty(*arg);
                    let deref_ty = arg_ty.deref(self.db.upcast());
                    let ty_data = deref_ty.data(self.db.upcast());
                    let arg_expr = if deref_ty.is_zero_sized(self.db.upcast()) {
                        None
                    } else {
                        Some(self.value_expr(*arg))
                    };
                    let name = match &ty_data.kind {
                        ir::TypeKind::Struct(def) => &def.name,
                        ir::TypeKind::String(def) => "Error",
                        _ => "Panic",
                    };
                    self.sink.push(yul::Statement::Expression(
                        self.ctx.runtime.revert(self.db, arg_expr, name, arg_ty),
                    ));
                }
                None => self.sink.push(statement! {revert(0, 0)}),
            },

            InstKind::Emit { arg } => {
                let event = self.value_expr(*arg);
                let event_ty = self.body.store.value_ty(*arg);
                let result = self.ctx.runtime.emit(self.db, event, event_ty);
                let u256_ty = yul_primitive_type(self.db);
                self.assign_inst_result(inst, result, u256_ty);
            }

            InstKind::Return { arg } => {
                if let Some(arg) = arg {
                    let arg = self.value_expr(*arg);
                    let ret_value = self.ret_value.clone().unwrap();
                    self.sink.push(statement! {[ret_value] := [arg]});
                }
                self.sink.push(yul::Statement::Leave)
            }

            InstKind::Keccak256 { arg } => {
                let result = self.keccak256(*arg);
                let u256_ty = yul_primitive_type(self.db);
                self.assign_inst_result(inst, result, u256_ty);
            }

            InstKind::AbiEncode { arg } => {
                let lhs = self.body.store.inst_result(inst).unwrap();
                let ptr = self.lower_assignable_value(lhs);
                let ptr_ty = lhs.ty(self.db.upcast(), &self.body.store);
                let src_expr = self.value_expr(*arg);
                let src_ty = self.body.store.value_ty(*arg);

                let abi_encode = self.ctx.runtime.abi_encode(
                    self.db,
                    src_expr,
                    ptr,
                    src_ty,
                    ptr_ty.is_sptr(self.db.upcast()),
                );
                self.sink.push(statement! {
                    pop([abi_encode])
                });
            }

            InstKind::Create { value, contract } => {
                self.ctx.contract_dependency.insert(*contract);

                let value_expr = self.value_expr(*value);
                let result = self.ctx.runtime.create(self.db, *contract, value_expr);
                let u256_ty = yul_primitive_type(self.db);
                self.assign_inst_result(inst, result, u256_ty)
            }

            InstKind::Create2 {
                value,
                salt,
                contract,
            } => {
                self.ctx.contract_dependency.insert(*contract);

                let value_expr = self.value_expr(*value);
                let salt_expr = self.value_expr(*salt);
                let result = self
                    .ctx
                    .runtime
                    .create2(self.db, *contract, value_expr, salt_expr);
                let u256_ty = yul_primitive_type(self.db);
                self.assign_inst_result(inst, result, u256_ty)
            }

            InstKind::YulIntrinsic { op, args } => {
                let args: Vec<_> = args.iter().map(|arg| self.value_expr(*arg)).collect();
                let op_name = identifier! { (format!("{op}").strip_prefix("__").unwrap()) };
                let result = expression! { [op_name]([args...]) };
                // Intrinsic operation never returns ptr type, so we can use u256_ty as a dummy
                // type for the result.
                let u256_ty = yul_primitive_type(self.db);
                self.assign_inst_result(inst, result, u256_ty)
            }

            InstKind::Nop => {}

            // These flow control instructions are already legalized.
            InstKind::Jump { .. } | InstKind::Branch { .. } | InstKind::Switch { .. } => {
                unreachable!()
            }
        }
    }

    fn lower_if(
        &mut self,
        cond: ValueId,
        then: Vec<StructuralInst>,
        else_: Vec<StructuralInst>,
    ) -> yul::Statement {
        let cond = self.value_expr(cond);

        self.enter_scope();
        let then_body = self.lower_branch_body(then);
        self.leave_scope();

        self.enter_scope();
        let else_body = self.lower_branch_body(else_);
        self.leave_scope();

        switch! {
            switch ([cond])
            (case 1 {[then_body...]})
            (case 0 {[else_body...]})
        }
    }

    fn lower_switch(
        &mut self,
        scrutinee: ValueId,
        table: Vec<(ValueId, Vec<StructuralInst>)>,
        default: Option<Vec<StructuralInst>>,
    ) -> yul::Statement {
        let scrutinee = self.value_expr(scrutinee);

        let mut cases = vec![];
        for (value, insts) in table {
            let value = self.value_expr(value);
            let value = match value {
                yul::Expression::Literal(lit) => lit,
                _ => panic!("switch table values must be literal"),
            };

            self.enter_scope();
            let body = self.lower_branch_body(insts);
            self.leave_scope();
            cases.push(yul::Case {
                literal: Some(value),
                block: block! { [body...] },
            })
        }

        if let Some(insts) = default {
            let block = self.lower_branch_body(insts);
            cases.push(case! {
                default {[block...]}
            });
        }

        switch! {
            switch ([scrutinee])
            [cases...]
        }
    }

    fn lower_branch_body(&mut self, insts: Vec<StructuralInst>) -> Vec<yul::Statement> {
        let mut body = vec![];
        std::mem::swap(&mut self.sink, &mut body);
        for inst in insts {
            self.lower_structural_inst(inst);
        }
        std::mem::swap(&mut self.sink, &mut body);
        body
    }

    fn lower_for(&mut self, body: Vec<StructuralInst>) -> yul::Statement {
        let mut body_stmts = vec![];
        std::mem::swap(&mut self.sink, &mut body_stmts);
        for inst in body {
            self.lower_structural_inst(inst);
        }
        std::mem::swap(&mut self.sink, &mut body_stmts);

        block_statement! {(
            for {} (1) {}
            {
                [body_stmts...]
            }
        )}
    }

    fn lower_assign(&mut self, lhs: &AssignableValue, rhs: ValueId) -> yul::Statement {
        match lhs {
            AssignableValue::Value(value) => {
                let lhs = self.value_ident(*value);
                let rhs = self.value_expr(rhs);
                statement! { [lhs] := [rhs] }
            }
            AssignableValue::Aggregate { .. } | AssignableValue::Map { .. } => {
                let dst_ty = lhs.ty(self.db.upcast(), &self.body.store);
                let src_ty = self.body.store.value_ty(rhs);
                debug_assert_eq!(
                    dst_ty.deref(self.db.upcast()),
                    src_ty.deref(self.db.upcast())
                );

                let dst = self.lower_assignable_value(lhs);
                let src = self.value_expr(rhs);

                if src_ty.is_ptr(self.db.upcast()) {
                    let ty_size = literal_expression! { (self.value_ty_size_deref(rhs)) };

                    let expr = self.ctx.runtime.ptr_copy(
                        self.db,
                        src,
                        dst,
                        ty_size,
                        src_ty.is_sptr(self.db.upcast()),
                        dst_ty.is_sptr(self.db.upcast()),
                    );
                    yul::Statement::Expression(expr)
                } else {
                    let expr = self.ctx.runtime.ptr_store(self.db, dst, src, dst_ty);
                    yul::Statement::Expression(expr)
                }
            }
        }
    }

    fn lower_unary(&mut self, op: UnOp, value: ValueId) -> yul::Expression {
        let value_expr = self.value_expr(value);
        match op {
            UnOp::Not => expression! { iszero([value_expr])},
            UnOp::Neg => {
                let zero = literal_expression! {0};
                if self.body.store.value_data(value).is_imm() {
                    // Literals are checked at compile time (e.g. -128) so there's no point
                    // in adding a runtime check.
                    expression! {sub([zero], [value_expr])}
                } else {
                    let value_ty = self.body.store.value_ty(value);
                    self.ctx
                        .runtime
                        .safe_sub(self.db, zero, value_expr, value_ty)
                }
            }
            UnOp::Inv => expression! { not([value_expr])},
        }
    }

    fn lower_binary(
        &mut self,
        op: BinOp,
        lhs: ValueId,
        rhs: ValueId,
        inst: InstId,
    ) -> yul::Expression {
        let lhs_expr = self.value_expr(lhs);
        let rhs_expr = self.value_expr(rhs);
        let is_result_signed = self
            .body
            .store
            .inst_result(inst)
            .map(|val| {
                let ty = val.ty(self.db.upcast(), &self.body.store);
                ty.is_signed(self.db.upcast())
            })
            .unwrap_or(false);
        let is_lhs_signed = self.body.store.value_ty(lhs).is_signed(self.db.upcast());

        let inst_result = self.body.store.inst_result(inst).unwrap();
        let inst_result_ty = inst_result
            .ty(self.db.upcast(), &self.body.store)
            .deref(self.db.upcast());
        match op {
            BinOp::Add => self
                .ctx
                .runtime
                .safe_add(self.db, lhs_expr, rhs_expr, inst_result_ty),
            BinOp::Sub => self
                .ctx
                .runtime
                .safe_sub(self.db, lhs_expr, rhs_expr, inst_result_ty),
            BinOp::Mul => self
                .ctx
                .runtime
                .safe_mul(self.db, lhs_expr, rhs_expr, inst_result_ty),
            BinOp::Div => self
                .ctx
                .runtime
                .safe_div(self.db, lhs_expr, rhs_expr, inst_result_ty),
            BinOp::Mod => self
                .ctx
                .runtime
                .safe_mod(self.db, lhs_expr, rhs_expr, inst_result_ty),
            BinOp::Pow => self
                .ctx
                .runtime
                .safe_pow(self.db, lhs_expr, rhs_expr, inst_result_ty),
            BinOp::Shl => expression! {shl([rhs_expr], [lhs_expr])},
            BinOp::Shr if is_result_signed => expression! {sar([rhs_expr], [lhs_expr])},
            BinOp::Shr => expression! {shr([rhs_expr], [lhs_expr])},
            BinOp::BitOr | BinOp::LogicalOr => expression! {or([lhs_expr], [rhs_expr])},
            BinOp::BitXor => expression! {xor([lhs_expr], [rhs_expr])},
            BinOp::BitAnd | BinOp::LogicalAnd => expression! {and([lhs_expr], [rhs_expr])},
            BinOp::Eq => expression! {eq([lhs_expr], [rhs_expr])},
            BinOp::Ne => expression! {iszero((eq([lhs_expr], [rhs_expr])))},
            BinOp::Ge if is_lhs_signed => expression! {iszero((slt([lhs_expr], [rhs_expr])))},
            BinOp::Ge => expression! {iszero((lt([lhs_expr], [rhs_expr])))},
            BinOp::Gt if is_lhs_signed => expression! {sgt([lhs_expr], [rhs_expr])},
            BinOp::Gt => expression! {gt([lhs_expr], [rhs_expr])},
            BinOp::Le if is_lhs_signed => expression! {iszero((sgt([lhs_expr], [rhs_expr])))},
            BinOp::Le => expression! {iszero((gt([lhs_expr], [rhs_expr])))},
            BinOp::Lt if is_lhs_signed => expression! {slt([lhs_expr], [rhs_expr])},
            BinOp::Lt => expression! {lt([lhs_expr], [rhs_expr])},
        }
    }

    fn lower_cast(&mut self, value: ValueId, to: TypeId) -> yul::Expression {
        let from_ty = self.body.store.value_ty(value);
        debug_assert!(from_ty.is_primitive(self.db.upcast()));
        debug_assert!(to.is_primitive(self.db.upcast()));

        let value = self.value_expr(value);
        self.ctx.runtime.primitive_cast(self.db, value, from_ty)
    }

    fn assign_inst_result(&mut self, inst: InstId, rhs: yul::Expression, rhs_ty: TypeId) {
        // NOTE: We don't have `deref` feature yet, so need a heuristics for an
        // assignment.
        let stmt = if let Some(result) = self.body.store.inst_result(inst) {
            let lhs = self.lower_assignable_value(result);
            let lhs_ty = result.ty(self.db.upcast(), &self.body.store);
            match result {
                AssignableValue::Value(value) => {
                    match (
                        lhs_ty.is_ptr(self.db.upcast()),
                        rhs_ty.is_ptr(self.db.upcast()),
                    ) {
                        (true, true) => {
                            if lhs_ty.is_mptr(self.db.upcast()) == rhs_ty.is_mptr(self.db.upcast())
                            {
                                let rhs = self.extend_value(rhs, lhs_ty);
                                let lhs_ident = self.value_ident(*value);
                                statement! { [lhs_ident] := [rhs] }
                            } else {
                                let ty_size = rhs_ty
                                    .deref(self.db.upcast())
                                    .size_of(self.db.upcast(), SLOT_SIZE);
                                yul::Statement::Expression(self.ctx.runtime.ptr_copy(
                                    self.db,
                                    rhs,
                                    lhs,
                                    literal_expression! { (ty_size) },
                                    rhs_ty.is_sptr(self.db.upcast()),
                                    lhs_ty.is_sptr(self.db.upcast()),
                                ))
                            }
                        }
                        (true, false) => yul::Statement::Expression(
                            self.ctx.runtime.ptr_store(self.db, lhs, rhs, lhs_ty),
                        ),

                        (false, true) => {
                            let rhs = self.ctx.runtime.ptr_load(self.db, rhs, rhs_ty);
                            let rhs = self.extend_value(rhs, lhs_ty);
                            let lhs_ident = self.value_ident(*value);
                            statement! { [lhs_ident] := [rhs] }
                        }
                        (false, false) => {
                            let rhs = self.extend_value(rhs, lhs_ty);
                            let lhs_ident = self.value_ident(*value);
                            statement! { [lhs_ident] := [rhs] }
                        }
                    }
                }
                AssignableValue::Aggregate { .. } | AssignableValue::Map { .. } => {
                    let expr = if rhs_ty.is_ptr(self.db.upcast()) {
                        let ty_size = rhs_ty
                            .deref(self.db.upcast())
                            .size_of(self.db.upcast(), SLOT_SIZE);
                        self.ctx.runtime.ptr_copy(
                            self.db,
                            rhs,
                            lhs,
                            literal_expression! { (ty_size) },
                            rhs_ty.is_sptr(self.db.upcast()),
                            lhs_ty.is_sptr(self.db.upcast()),
                        )
                    } else {
                        self.ctx.runtime.ptr_store(self.db, lhs, rhs, lhs_ty)
                    };
                    yul::Statement::Expression(expr)
                }
            }
        } else {
            yul::Statement::Expression(rhs)
        };

        self.sink.push(stmt);
    }

    /// Extend a value to 256 bits.
    fn extend_value(&mut self, value: yul::Expression, ty: TypeId) -> yul::Expression {
        if ty.is_primitive(self.db.upcast()) {
            self.ctx.runtime.primitive_cast(self.db, value, ty)
        } else {
            value
        }
    }

    fn declare_assignable_value(&mut self, value: &AssignableValue) {
        match value {
            AssignableValue::Value(value) if !self.value_map.contains(*value) => {
                self.declare_value(*value);
            }
            _ => {}
        }
    }

    fn declare_value(&mut self, value: ValueId) {
        let var = YulVariable::new(format!("$tmp_{}", value.index()));
        self.value_map.insert(value, var.ident());
        let value_ty = self.body.store.value_ty(value);

        // Allocate memory for a value if a value is a pointer type.
        let init = if value_ty.is_mptr(self.db.upcast()) {
            let deref_ty = value_ty.deref(self.db.upcast());
            let ty_size = deref_ty.size_of(self.db.upcast(), SLOT_SIZE);
            let size = literal_expression! { (ty_size) };
            Some(self.ctx.runtime.alloc(self.db, size))
        } else {
            None
        };

        self.sink.push(yul::Statement::VariableDeclaration(
            yul::VariableDeclaration {
                identifiers: vec![var.ident()],
                expression: init,
            },
        ))
    }

    fn value_expr(&mut self, value: ValueId) -> yul::Expression {
        match self.body.store.value_data(value) {
            Value::Local(_) | Value::Temporary { .. } => {
                let ident = self.value_map.lookup(value).unwrap();
                literal_expression! {(ident)}
            }
            Value::Immediate { imm, .. } => {
                literal_expression! {(imm)}
            }
            Value::Constant { constant, .. } => match &constant.data(self.db.upcast()).value {
                ConstantValue::Immediate(imm) => {
                    // YUL does not support representing negative integers with leading minus (e.g.
                    // `-1` in YUL would lead to an ICE). To mitigate that we
                    // convert all numeric values into hexadecimal representation.
                    literal_expression! {(to_hex_str(imm))}
                }
                ConstantValue::Str(s) => {
                    self.ctx.string_constants.insert(s.to_string());
                    self.ctx.runtime.string_construct(self.db, s, s.len())
                }
                ConstantValue::Bool(true) => {
                    literal_expression! {1}
                }
                ConstantValue::Bool(false) => {
                    literal_expression! {0}
                }
            },
            Value::Unit { .. } => unreachable!(),
        }
    }

    fn value_ident(&self, value: ValueId) -> yul::Identifier {
        self.value_map.lookup(value).unwrap().clone()
    }

    fn make_tmp(&mut self, tmp: ValueId) -> yul::Identifier {
        let ident = YulVariable::new(format! {"$tmp_{}", tmp.index()}).ident();
        self.value_map.insert(tmp, ident.clone());
        ident
    }

    fn keccak256(&mut self, value: ValueId) -> yul::Expression {
        let value_ty = self.body.store.value_ty(value);
        debug_assert!(value_ty.is_mptr(self.db.upcast()));

        let value_size = value_ty
            .deref(self.db.upcast())
            .size_of(self.db.upcast(), SLOT_SIZE);
        let value_size_expr = literal_expression! {(value_size)};
        let value_expr = self.value_expr(value);
        expression! {keccak256([value_expr], [value_size_expr])}
    }

    fn lower_assignable_value(&mut self, value: &AssignableValue) -> yul::Expression {
        match value {
            AssignableValue::Value(value) => self.value_expr(*value),

            AssignableValue::Aggregate { lhs, idx } => {
                let base_ptr = self.lower_assignable_value(lhs);
                let ty = lhs
                    .ty(self.db.upcast(), &self.body.store)
                    .deref(self.db.upcast());
                self.aggregate_elem_ptr(base_ptr, *idx, ty)
            }
            AssignableValue::Map { lhs, key } => {
                let map_ptr = self.lower_assignable_value(lhs);
                let key_ty = self.body.store.value_ty(*key);
                let key = self.value_expr(*key);
                self.ctx
                    .runtime
                    .map_value_ptr(self.db, map_ptr, key, key_ty)
            }
        }
    }

    fn aggregate_elem_ptr(
        &mut self,
        base_ptr: yul::Expression,
        idx: ValueId,
        base_ty: TypeId,
    ) -> yul::Expression {
        debug_assert!(base_ty.is_aggregate(self.db.upcast()));

        match &base_ty.data(self.db.upcast()).kind {
            TypeKind::Array(def) => {
                let elem_size =
                    literal_expression! {(base_ty.array_elem_size(self.db.upcast(), SLOT_SIZE))};
                self.validate_array_indexing(def.len, idx);
                let idx = self.value_expr(idx);
                let offset = expression! {mul([elem_size], [idx])};
                expression! { add([base_ptr], [offset]) }
            }
            _ => {
                let elem_idx = match self.body.store.value_data(idx) {
                    Value::Immediate { imm, .. } => imm,
                    _ => panic!("only array type can use dynamic value indexing"),
                };
                let offset = literal_expression! {(base_ty.aggregate_elem_offset(self.db.upcast(), elem_idx.clone(), SLOT_SIZE))};
                expression! {add([base_ptr], [offset])}
            }
        }
    }

    fn validate_array_indexing(&mut self, array_len: usize, idx: ValueId) {
        const PANIC_OUT_OF_BOUNDS: usize = 0x32;

        if let Value::Immediate { .. } = self.body.store.value_data(idx) {
            return;
        }

        let idx = self.value_expr(idx);
        let max_idx = literal_expression! {(array_len - 1)};
        self.sink.push(statement!(if (gt([idx], [max_idx])) {
            ([runtime::panic_revert_numeric(
                self.ctx.runtime.as_mut(),
                self.db,
                literal_expression! {(PANIC_OUT_OF_BOUNDS)},
            )])
        }));
    }

    fn value_ty_size(&self, value: ValueId) -> usize {
        self.body
            .store
            .value_ty(value)
            .size_of(self.db.upcast(), SLOT_SIZE)
    }

    fn value_ty_size_deref(&self, value: ValueId) -> usize {
        self.body
            .store
            .value_ty(value)
            .deref(self.db.upcast())
            .size_of(self.db.upcast(), SLOT_SIZE)
    }

    fn enter_scope(&mut self) {
        let value_map = std::mem::take(&mut self.value_map);
        self.value_map = ScopedValueMap::with_parent(value_map);
    }

    fn leave_scope(&mut self) {
        let value_map = std::mem::take(&mut self.value_map);
        self.value_map = value_map.into_parent();
    }
}

#[derive(Debug, Default)]
struct ScopedValueMap {
    parent: Option<Box<ScopedValueMap>>,
    map: FxHashMap<ValueId, yul::Identifier>,
}

impl ScopedValueMap {
    fn lookup(&self, value: ValueId) -> Option<&yul::Identifier> {
        match self.map.get(&value) {
            Some(ident) => Some(ident),
            None => self.parent.as_ref().and_then(|p| p.lookup(value)),
        }
    }

    fn with_parent(parent: ScopedValueMap) -> Self {
        Self {
            parent: Some(parent.into()),
            ..Self::default()
        }
    }

    fn into_parent(self) -> Self {
        *self.parent.unwrap()
    }

    fn insert(&mut self, value: ValueId, ident: yul::Identifier) {
        self.map.insert(value, ident);
    }

    fn contains(&self, value: ValueId) -> bool {
        self.lookup(value).is_some()
    }
}

fn bit_mask(byte_size: usize) -> usize {
    (1 << (byte_size * 8)) - 1
}

fn bit_mask_expr(byte_size: usize) -> yul::Expression {
    let mask = format!("{:#x}", bit_mask(byte_size));
    literal_expression! {(mask)}
}
