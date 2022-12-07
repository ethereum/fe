//! This module contains algorithms to generate Codegen Units (CGUs) from the
//! MIR modules.
//!
//! The algorithm to collect CGUs is basically a normal worklist algorithm.
//! Therefore, the algorithm is summarized by the following steps:
//!
//! 1. Collect all functions which are not generic functions, and add them to
//! the worklist.
//!
//! 2. Pop a function from the worklist. If the function is not in a visited
//! set, add it to the visited set and proceed to step 3.
//!
//! 3. Iterate all the instructions in the current function to find a generic
//! function call. If generic function calls are found, monomorphizes them and
//! add them to the worklist.
//!
//! 4. Add the current function to the CGU of the module.
//!
//! 5. Repeat steps 2-4 until the worklist is empty.    

use std::rc::Rc;

use fe_analyzer::namespace::{
    items::FunctionSigId as AnalyzerFuncSigId,
    items::{IngotId, Item},
};
use fe_mir::ir::{
    body_cursor::{BodyCursor, CursorLocation},
    function::BodyDataStore,
    inst::InstKind,
    types::TypeParamDef,
    FunctionBody, FunctionParam, FunctionSigId, FunctionSignature, InstId, TypeId, TypeKind,
    ValueId,
};
use fxhash::{FxHashMap, FxHashSet};
use indexmap::IndexSet;
use smol_str::SmolStr;

use crate::{
    cgu::{CguFunction, CguFunctionId, CodegenUnit, CodegenUnitId},
    db::CodegenDb,
};

pub fn generate_cgu(db: &dyn CodegenDb, ingot: IngotId) -> Rc<[CodegenUnitId]> {
    let mut worklist = Vec::new();

    // Step 1.
    for &module in ingot.all_modules(db.upcast()).iter() {
        for &function in module.all_functions(db.upcast()).iter() {
            if !function.is_generic(db.upcast()) {
                let sig = function.sig(db.upcast());
                let sig = db.mir_lowered_func_signature(sig);
                let body = (*db.mir_lowered_func_body(function)).clone();
                worklist.push((sig, body));
            }
        }
    }

    let mut cgu_map: FxHashMap<_, CodegenUnit> = FxHashMap::default();
    let mut visited = FxHashSet::default();

    // Step 2-4.
    while let Some((sig, body)) = worklist.pop() {
        // Step 2.
        if !visited.insert(sig) {
            continue;
        }

        // Step 3.
        let (cgu_func_id, instantiated_funcs) = CguFunctionBuilder::new(db).build(sig, body);
        worklist.extend_from_slice(instantiated_funcs.as_slice());

        // Step 4.
        let module = sig.module(db.upcast());
        cgu_map
            .entry(module)
            .or_default()
            .functions
            .push(cgu_func_id);
    }

    cgu_map
        .into_values()
        .map(|cgu| db.codegen_intern_cgu(cgu.into()))
        .collect()
}

struct CguFunctionBuilder<'db> {
    db: &'db dyn CodegenDb,
}

impl<'db> CguFunctionBuilder<'db> {
    fn new(db: &'db dyn CodegenDb) -> Self {
        Self { db }
    }

    fn build(
        &self,
        sig: FunctionSigId,
        mut body: FunctionBody,
    ) -> (CguFunctionId, Vec<(FunctionSigId, FunctionBody)>) {
        let mut mono_funcs = Vec::new();
        let mut callees = IndexSet::new();

        let mut cursor = BodyCursor::new_at_entry(&mut body);
        loop {
            match cursor.loc() {
                CursorLocation::BlockTop(_) | CursorLocation::BlockBottom(_) => cursor.proceed(),
                CursorLocation::NoWhere => {
                    break;
                }
                CursorLocation::Inst(inst) => {
                    if let InstKind::Call { func: callee, .. } =
                        &cursor.body().store.inst_data(inst).kind
                    {
                        if callee.is_generic(self.db.upcast()) {
                            let (mono_sig, mono_body) =
                                self.monomorphize_func(sig, &cursor.body().store, inst);
                            // Rewrite the callee to the mono function.
                            cursor.body_mut().store.rewrite_callee(inst, mono_sig);

                            if callees.insert(mono_sig) {
                                mono_funcs.push((mono_sig, mono_body));
                            }
                        } else {
                            callees.insert(*callee);
                        }
                    }
                    cursor.proceed();
                }
            }
        }
        let cgu_func = CguFunction { sig, body, callees };
        let cgu_func_id = self.db.codegen_intern_cgu_func(cgu_func.into());
        (cgu_func_id, mono_funcs)
    }

    fn monomorphize_func(
        &self,
        caller: FunctionSigId,
        store: &BodyDataStore,
        inst: InstId,
    ) -> (FunctionSigId, FunctionBody) {
        let InstKind::Call {func: callee, args, generic_type, ..} = &store.inst_data(inst).kind else {
            panic!("expected a call instruction");
        };
        debug_assert!(callee.is_generic(self.db.upcast()));

        let callee = *callee;
        let subst = self.get_subst(store, callee, args);

        let mono_sig = self.monomorphize_sig(caller, callee, &subst, generic_type);
        let mono_body = self.monomorphize_body(callee, &subst);

        (self.db.mir_intern_function(mono_sig.into()), mono_body)
    }

    fn monomorphize_sig(
        &self,
        caller: FunctionSigId,
        callee: FunctionSigId,
        subst: &FxHashMap<SmolStr, TypeId>,
        generic_type: &Option<TypeParamDef>,
    ) -> FunctionSignature {
        let params = callee
            .data(self.db.upcast())
            .params
            .iter()
            .map(|param| {
                let ty = match &param.ty.data(self.db.upcast()).kind {
                    TypeKind::TypeParam(def) => subst[&def.name],
                    _ => param.ty,
                };

                FunctionParam {
                    name: param.name.clone(),
                    ty,
                    source: param.source.clone(),
                }
            })
            .collect();

        let return_type = callee.return_type(self.db.upcast());
        // If the callee and caller are in different ingots, we need to generate a
        // function in the caller's module.
        let module_id = if callee.ingot(self.db.upcast()) != caller.ingot(self.db.upcast()) {
            caller.module(self.db.upcast())
        } else {
            callee.module(self.db.upcast())
        };
        let linkage = callee.linkage(self.db.upcast());
        let analyzer_id =
            self.resolve_function(callee.analyzer_sig(self.db.upcast()), subst, generic_type);

        FunctionSignature {
            name: self.mono_func_name(caller, analyzer_id, subst),
            params,
            return_type,
            module_id,
            analyzer_id,
            linkage,
        }
    }

    fn monomorphize_body(
        &self,
        func: FunctionSigId,
        subst: &FxHashMap<SmolStr, TypeId>,
    ) -> FunctionBody {
        let func_id = func
            .analyzer_sig(self.db.upcast())
            .function(self.db.upcast())
            .unwrap();

        let mut body = (*self.db.mir_lowered_func_body(func_id)).clone();
        for value in body.store.values_mut() {
            if let TypeKind::TypeParam(def) = &value.ty().data(self.db.upcast()).kind {
                let subst_ty = subst[&def.name];
                *value.ty_mut() = subst_ty;
            }
        }

        body
    }

    /// Resolve the trait function signature into the corresponding concrete
    /// implementation if the `callee` is a trait function.
    fn resolve_function(
        &self,
        callee: AnalyzerFuncSigId,
        subst: &FxHashMap<SmolStr, TypeId>,
        generic_type: &Option<TypeParamDef>,
    ) -> AnalyzerFuncSigId {
        let trait_id = match callee.parent(self.db.upcast()) {
            Item::Trait(id) => id,
            _ => return callee,
        };

        let concrete_type = subst[&generic_type.as_ref().unwrap().name];
        let impl_ = concrete_type
            .analyzer_ty(self.db.upcast())
            .unwrap()
            .get_impl_for(self.db.upcast(), trait_id)
            .unwrap();

        let resolved = impl_
            .function(self.db.upcast(), &callee.name(self.db.upcast()))
            .expect("missing function");
        resolved.sig(self.db.upcast())
    }

    fn mono_func_name(
        &self,
        caller: FunctionSigId,
        callee: AnalyzerFuncSigId,
        subst: &FxHashMap<SmolStr, TypeId>,
    ) -> SmolStr {
        let callee_ingot = callee.ingot(self.db.upcast());
        let mut func_name = if caller.ingot(self.db.upcast()) != callee_ingot {
            format!(
                "{}${}",
                callee_ingot.name(self.db.upcast()),
                callee.name(self.db.upcast())
            )
        } else {
            callee.name(self.db.upcast()).to_string()
        };
        for ty in subst.values() {
            func_name.push_str(&format!("${}", ty.as_string(self.db.upcast())))
        }
        func_name.into()
    }

    fn get_subst(
        &self,
        store: &BodyDataStore,
        callee: FunctionSigId,
        args: &[ValueId],
    ) -> FxHashMap<SmolStr, TypeId> {
        debug_assert_eq!(callee.data(self.db.upcast()).params.len(), args.len());

        callee
            .data(self.db.upcast())
            .params
            .iter()
            .zip(args)
            .filter_map(|(param, arg)| match &param.ty.data(self.db.upcast()).kind {
                TypeKind::TypeParam(def) => Some((def.name.clone(), store.value_ty(*arg))),
                _ => None,
            })
            .collect()
    }
}
