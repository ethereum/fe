use fe_common::utils::keccak;

use serde::Serialize;

use super::types::AbiType;

/// The mutability of a public function.
#[derive(Serialize, Debug, PartialEq, Eq, Clone)]
#[serde(rename_all = "lowercase")]
pub enum StateMutability {
    Pure,
    View,
    Nonpayable,
    Payable,
}

pub enum SelfParam {
    None,
    Imm,
    Mut,
}
pub enum CtxParam {
    None,
    Imm,
    Mut,
}

impl StateMutability {
    pub fn from_self_and_ctx_params(self_: SelfParam, ctx: CtxParam) -> Self {
        // Check ABI conformity
        // See https://github.com/ethereum/fe/issues/558
        //
        //              no self   |   self   |  mut self  |
        //           ......................................
        // no ctx    :    pure    |   view    |  payable  |
        // ctx       :    view    |   view    |  payable  |
        // mut ctx   :   payable  |  payable  |  payable  |

        match (self_, ctx) {
            (SelfParam::None, CtxParam::None) => StateMutability::Pure,
            (SelfParam::None, CtxParam::Imm) => StateMutability::View,
            (SelfParam::None, CtxParam::Mut) => StateMutability::Payable,
            (SelfParam::Imm, CtxParam::None) => StateMutability::View,
            (SelfParam::Imm, CtxParam::Imm) => StateMutability::View,
            (SelfParam::Imm, CtxParam::Mut) => StateMutability::Payable,
            (SelfParam::Mut, _) => StateMutability::Payable,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AbiFunction {
    #[serde(rename = "type")]
    func_type: AbiFunctionType,
    name: String,
    inputs: Vec<AbiFunctionParamInner>,
    outputs: Vec<AbiFunctionParamInner>,
    #[serde(rename = "stateMutability")]
    state_mutability: StateMutability,
}

impl AbiFunction {
    pub fn new(
        func_type: AbiFunctionType,
        name: String,
        args: Vec<(String, AbiType)>,
        ret_ty: Option<AbiType>,
        state_mutability: StateMutability,
    ) -> Self {
        let inputs = args
            .into_iter()
            .map(|(arg_name, arg_ty)| AbiFunctionParamInner::new(arg_name, arg_ty))
            .collect();
        let outputs = ret_ty.map_or_else(Vec::new, |ret_ty| {
            vec![AbiFunctionParamInner::new("".into(), ret_ty)]
        });

        Self {
            func_type,
            name,
            inputs,
            outputs,
            state_mutability,
        }
    }

    pub fn selector(&self) -> AbiFunctionSelector {
        AbiFunctionSelector::new(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum AbiFunctionType {
    Function,
    Constructor,
    Receive,
    Payable,
    Fallback,
}

pub struct AbiFunctionSelector {
    selector_sig: String,
}

impl AbiFunctionSelector {
    fn new(func_sig: &AbiFunction) -> Self {
        let selector_sig = format!(
            "{}({})",
            func_sig.name,
            func_sig
                .inputs
                .iter()
                .map(|param| param.ty.selector_type_name())
                .collect::<Vec<_>>()
                .join(",")
        );

        Self { selector_sig }
    }

    pub fn selector_signature(&self) -> &str {
        &self.selector_sig
    }

    pub fn selector_raw(&self) -> [u8; 4] {
        keccak::full_as_bytes(self.selector_sig.as_bytes())[..4]
            .try_into()
            .unwrap()
    }

    /// Returns first 4 bytes of signature hash in hex.
    pub fn hex(&self) -> String {
        keccak::partial(self.selector_sig.as_bytes(), 4)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct AbiFunctionParamInner {
    name: String,
    #[serde(flatten)]
    ty: AbiType,
}

impl AbiFunctionParamInner {
    fn new(name: String, ty: AbiType) -> Self {
        Self { name, ty }
    }
}

#[cfg(test)]
mod tests {
    use crate::types::AbiTupleField;

    use super::*;
    use serde_test::{assert_ser_tokens, Token};

    fn simple_tuple() -> AbiType {
        let u16_ty = AbiType::UInt(16);
        let bool_ty = AbiType::Bool;
        let field1 = AbiTupleField::new("field1".into(), u16_ty);
        let field2 = AbiTupleField::new("field2".into(), bool_ty);

        AbiType::Tuple(vec![field1, field2])
    }

    fn test_func(state_mutability: StateMutability) -> AbiFunction {
        let i32_ty = AbiType::Int(32);
        let tuple_ty = simple_tuple();
        let u64_ty = AbiType::UInt(64);

        AbiFunction::new(
            AbiFunctionType::Function,
            "test_func".into(),
            vec![("arg1".into(), i32_ty), ("arg2".into(), tuple_ty)],
            Some(u64_ty),
            state_mutability,
        )
    }

    #[test]
    fn serialize_func() {
        let func = test_func(StateMutability::Payable);

        assert_ser_tokens(
            &func,
            &[
                Token::Struct {
                    name: "AbiFunction",
                    len: 5,
                },
                Token::Str("type"),
                Token::UnitVariant {
                    name: "AbiFunctionType",
                    variant: "function",
                },
                Token::String("name"),
                Token::String("test_func"),
                Token::Str("inputs"),
                Token::Seq { len: Some(2) },
                Token::Map { len: None },
                Token::String("name"),
                Token::String("arg1"),
                Token::String("type"),
                Token::String("int32"),
                Token::MapEnd,
                Token::Map { len: None },
                Token::String("name"),
                Token::String("arg2"),
                Token::String("type"),
                Token::String("tuple"),
                Token::String("components"),
                Token::Seq { len: Some(2) },
                Token::Map { len: None },
                Token::String("name"),
                Token::String("field1"),
                Token::String("type"),
                Token::String("uint16"),
                Token::MapEnd,
                Token::Map { len: None },
                Token::String("name"),
                Token::String("field2"),
                Token::String("type"),
                Token::String("bool"),
                Token::MapEnd,
                Token::SeqEnd,
                Token::MapEnd,
                Token::SeqEnd,
                Token::Str("outputs"),
                Token::Seq { len: Some(1) },
                Token::Map { len: None },
                Token::String("name"),
                Token::String(""),
                Token::String("type"),
                Token::String("uint64"),
                Token::MapEnd,
                Token::SeqEnd,
                Token::Str("stateMutability"),
                Token::UnitVariant {
                    name: "StateMutability",
                    variant: "payable",
                },
                Token::StructEnd,
            ],
        )
    }

    #[test]
    fn test_state_mutability() {
        assert_eq!(
            StateMutability::from_self_and_ctx_params(SelfParam::None, CtxParam::None),
            StateMutability::Pure
        );
        assert_eq!(
            StateMutability::from_self_and_ctx_params(SelfParam::None, CtxParam::Imm),
            StateMutability::View
        );
        assert_eq!(
            StateMutability::from_self_and_ctx_params(SelfParam::None, CtxParam::Mut),
            StateMutability::Payable
        );

        assert_eq!(
            StateMutability::from_self_and_ctx_params(SelfParam::Imm, CtxParam::None),
            StateMutability::View
        );
        assert_eq!(
            StateMutability::from_self_and_ctx_params(SelfParam::Imm, CtxParam::Imm),
            StateMutability::View
        );
        assert_eq!(
            StateMutability::from_self_and_ctx_params(SelfParam::Imm, CtxParam::Mut),
            StateMutability::Payable
        );

        assert_eq!(
            StateMutability::from_self_and_ctx_params(SelfParam::Mut, CtxParam::None),
            StateMutability::Payable
        );
        assert_eq!(
            StateMutability::from_self_and_ctx_params(SelfParam::Mut, CtxParam::Imm),
            StateMutability::Payable
        );
        assert_eq!(
            StateMutability::from_self_and_ctx_params(SelfParam::Mut, CtxParam::Mut),
            StateMutability::Payable
        );

        let pure_func = test_func(StateMutability::Pure);
        assert_eq!(pure_func.state_mutability, StateMutability::Pure);

        let impure_func = test_func(StateMutability::Payable);
        assert_eq!(impure_func.state_mutability, StateMutability::Payable);
    }

    #[test]
    fn func_selector() {
        let func = test_func(StateMutability::Payable);
        let selector = func.selector();

        debug_assert_eq!(
            selector.selector_signature(),
            "test_func(int32,(uint16,bool))"
        );
        debug_assert_eq!(selector.hex(), "79c3c8b2");
    }
}
