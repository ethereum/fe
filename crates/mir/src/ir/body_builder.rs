use crate::ir::{
    body_cursor::{BodyCursor, CursorLocation},
    inst::{BinOp, Inst, InstKind, UnOp},
    value::{AssignableValue, Local, Temporary},
    BasicBlock, BasicBlockId, FunctionBody, FunctionId, SourceInfo, TypeId, ValueId,
};

pub struct BodyBuilder {
    body: FunctionBody,
    loc: CursorLocation,
}

macro_rules! impl_unary_inst {
    ($name:ident, $code:path) => {
        pub fn $name(&mut self, value: ValueId, result_ty: TypeId, source: SourceInfo) -> ValueId {
            let inst = Inst::unary($code, value, source);
            self.insert_inst(inst, Some(result_ty)).unwrap()
        }
    };
}

macro_rules! impl_binary_inst {
    ($name:ident, $code:path) => {
        pub fn $name(
            &mut self,
            lhs: ValueId,
            rhs: ValueId,
            result_ty: TypeId,
            source: SourceInfo,
        ) -> ValueId {
            let inst = Inst::binary($code, lhs, rhs, source);
            self.insert_inst(inst, Some(result_ty)).unwrap()
        }
    };
}

impl BodyBuilder {
    pub fn new(fid: FunctionId, source: SourceInfo) -> Self {
        let body = FunctionBody::new(fid, source);
        let entry_block = body.order.entry_block();
        Self {
            body,
            loc: CursorLocation::BlockTop(entry_block),
        }
    }

    pub fn make_block(&mut self) -> BasicBlockId {
        let block = BasicBlock {};
        self.body.store.store_block(block)
    }

    pub fn move_to_block(&mut self, block: BasicBlockId) {
        self.loc = CursorLocation::BlockBottom(block)
    }

    pub fn declare(&mut self, local: Local) -> ValueId {
        let source = local.source.clone();
        let local_id = self.body.store.store_value(local.into());

        self.body.locals.push(local_id);

        let kind = InstKind::Declare { local: local_id };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst, None);
        local_id
    }

    pub fn assign(&mut self, lhs: AssignableValue, rhs: ValueId, source: SourceInfo) {
        let lhs_id = self.body.store.store_value(lhs.into());
        let kind = InstKind::Assign { lhs: lhs_id, rhs };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst, None);
    }

    impl_unary_inst!(not, UnOp::Not);
    impl_unary_inst!(neg, UnOp::Neg);
    impl_unary_inst!(inv, UnOp::Inv);

    impl_binary_inst!(add, BinOp::Add);
    impl_binary_inst!(sub, BinOp::Sub);
    impl_binary_inst!(mul, BinOp::Mul);
    impl_binary_inst!(div, BinOp::Div);
    impl_binary_inst!(modulo, BinOp::Mod);
    impl_binary_inst!(pow, BinOp::Pow);
    impl_binary_inst!(shl, BinOp::Shl);
    impl_binary_inst!(shr, BinOp::Shr);
    impl_binary_inst!(bit_or, BinOp::BitOr);
    impl_binary_inst!(bit_xor, BinOp::BitXor);
    impl_binary_inst!(bit_and, BinOp::BitAnd);
    impl_binary_inst!(logical_and, BinOp::LogicalAnd);
    impl_binary_inst!(logical_or, BinOp::LogicalOr);
    impl_binary_inst!(eq, BinOp::Eq);
    impl_binary_inst!(ne, BinOp::Ne);
    impl_binary_inst!(ge, BinOp::Ge);
    impl_binary_inst!(gt, BinOp::Gt);
    impl_binary_inst!(le, BinOp::Le);
    impl_binary_inst!(lt, BinOp::Lt);

    pub fn cast(&mut self, value: ValueId, result_ty: TypeId, source: SourceInfo) -> ValueId {
        let kind = InstKind::Cast {
            value,
            to: result_ty,
        };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst, Some(result_ty)).unwrap()
    }

    pub fn aggregate_construct(
        &mut self,
        ty: TypeId,
        args: Vec<ValueId>,
        source: SourceInfo,
    ) -> ValueId {
        let kind = InstKind::AggregateConstruct { ty, args };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst, Some(ty)).unwrap()
    }

    pub fn aggregate_access(
        &mut self,
        value: ValueId,
        indices: Vec<usize>,
        result_ty: TypeId,
        source: SourceInfo,
    ) -> ValueId {
        let kind = InstKind::AggregateAccess { value, indices };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst, Some(result_ty)).unwrap()
    }

    pub fn call(
        &mut self,
        func: FunctionId,
        args: Vec<ValueId>,
        result_ty: TypeId,
        source: SourceInfo,
    ) -> ValueId {
        let kind = InstKind::Call { func, args };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst, Some(result_ty)).unwrap()
    }

    pub fn jump(&mut self, dest: BasicBlockId, source: SourceInfo) {
        let kind = InstKind::Jump { dest };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst, None);
    }

    pub fn branch(
        &mut self,
        cond: ValueId,
        then: BasicBlockId,
        else_: BasicBlockId,
        source: SourceInfo,
    ) {
        let kind = InstKind::Branch { cond, then, else_ };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst, None);
    }

    pub fn revert(&mut self, arg: ValueId, source: SourceInfo) {
        let kind = InstKind::Revert { arg };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst, None);
    }

    pub fn emit(&mut self, arg: ValueId, source: SourceInfo) {
        let kind = InstKind::Emit { arg };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst, None);
    }

    pub fn ret(&mut self, arg: ValueId, source: SourceInfo) {
        let kind = InstKind::Return { arg };
        let inst = Inst::new(kind, source);
        self.insert_inst(inst, None);
    }

    pub fn build(self) -> FunctionBody {
        self.body
    }

    fn insert_inst(&mut self, inst: Inst, result_ty: Option<TypeId>) -> Option<ValueId> {
        let mut cursor = self.cursor();
        let inst_id = cursor.store_and_insert_inst(inst);

        // Set cursor to the new inst.
        let loc = CursorLocation::Inst(inst_id);
        cursor.set_loc(loc);

        let result = if let Some(result_ty) = result_ty {
            // Map a result value to the inst.
            let temp = Temporary {
                inst: inst_id,
                ty: result_ty,
            };
            Some(cursor.store_and_map_result(temp.into()))
        } else {
            None
        };

        self.loc = loc;
        result
    }

    fn cursor(&mut self) -> BodyCursor {
        BodyCursor::new(&mut self.body, self.loc)
    }
}
