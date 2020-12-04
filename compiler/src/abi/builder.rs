use crate::abi::elements::{
    Contract,
    Event,
    EventField,
    FuncInput,
    FuncOutput,
    FuncType,
    Function,
    ModuleABIs,
    VarType,
};
use crate::errors::CompileError;
use fe_parser::ast as fe;
use fe_parser::span::Spanned;
use std::collections::HashMap;

type TypeDefs<'a> = HashMap<&'a str, &'a fe::TypeDesc<'a>>;

/// Parse a map of contract ABIs from the input `module`.
pub fn module<'a>(module: &'a fe::Module<'a>) -> Result<ModuleABIs, CompileError> {
    let mut type_defs = TypeDefs::new();

    module.body.iter().try_fold(ModuleABIs::new(), |mut m, s| {
        match &s.node {
            fe::ModuleStmt::TypeDef { name, typ } => {
                if type_defs.insert(name.node, &typ.node).is_some() {
                    return Err(CompileError::static_str("duplicate type definition"));
                }
            }
            fe::ModuleStmt::ContractDef { name, body } => {
                if m.contracts
                    .insert(name.node.to_string(), contract_def(&type_defs, body)?)
                    .is_some()
                {
                    return Err(CompileError::static_str("duplicate contract definition"));
                }
            }
            _ => {}
        };

        Ok(m)
    })
}

fn contract_def<'a>(
    type_defs: &'a TypeDefs<'a>,
    body: &[Spanned<fe::ContractStmt<'a>>],
) -> Result<Contract, CompileError> {
    body.iter().try_fold(Contract::new(), |mut c, s| {
        match &s.node {
            fe::ContractStmt::FuncDef {
                qual,
                name,
                args,
                return_type,
                ..
            } => {
                if let Some(qual) = qual {
                    if qual.node == fe::FuncQual::Pub {
                        c.functions.push(func_def(
                            type_defs,
                            name.node.to_string(),
                            args,
                            return_type,
                        )?)
                    }
                }
            }
            fe::ContractStmt::EventDef { name, fields } => {
                c.events
                    .push(event_def(type_defs, name.node.to_string(), fields)?)
            }
            fe::ContractStmt::ContractField { .. } => {}
        }

        Ok(c)
    })
}

fn event_def<'a>(
    type_defs: &'a TypeDefs<'a>,
    name: String,
    fields: &[Spanned<fe::EventField<'a>>],
) -> Result<Event, CompileError> {
    let fields = fields
        .iter()
        .map(|field| event_field(type_defs, &field.node))
        .collect::<Result<_, _>>()?;

    Ok(Event {
        name,
        typ: "event".to_string(),
        fields,
        anonymous: false,
    })
}

fn event_field<'a>(
    type_defs: &'a TypeDefs<'a>,
    field: &'a fe::EventField<'a>,
) -> Result<EventField, CompileError> {
    Ok(EventField {
        name: String::from(field.name.node),
        typ: type_desc(&type_defs, &field.typ.node)?,
        indexed: field.qual.is_some(),
    })
}

fn func_def<'a>(
    type_defs: &'a TypeDefs<'a>,
    name: String,
    args: &[Spanned<fe::FuncDefArg<'a>>],
    return_type: &'a Option<Spanned<fe::TypeDesc<'a>>>,
) -> Result<Function, CompileError> {
    let inputs = args
        .iter()
        .map(|arg| func_def_arg(type_defs, &arg.node))
        .collect::<Result<Vec<FuncInput>, CompileError>>()?;

    let outputs = if let Some(return_type) = return_type {
        vec![FuncOutput {
            name: "".to_string(),
            typ: type_desc(type_defs, &return_type.node)?,
        }]
    } else {
        vec![]
    };

    let (name, typ) = if name == "__init__" {
        ("".to_string(), FuncType::Constructor)
    } else {
        (name, FuncType::Function)
    };

    Ok(Function {
        name,
        typ,
        inputs,
        outputs,
    })
}

fn func_def_arg<'a>(
    type_defs: &'a TypeDefs<'a>,
    arg: &'a fe::FuncDefArg<'a>,
) -> Result<FuncInput, CompileError> {
    Ok(FuncInput {
        name: String::from(arg.name.node),
        typ: type_desc(&type_defs, &arg.typ.node)?,
    })
}

fn type_desc<'a>(
    type_defs: &'a TypeDefs<'a>,
    typ: &'a fe::TypeDesc<'a>,
) -> Result<VarType, CompileError> {
    if let fe::TypeDesc::Base { base } = typ {
        if let Some(custom_type) = type_defs.get(base) {
            return type_desc(type_defs, custom_type);
        }
    }

    match typ {
        fe::TypeDesc::Base { base: "u256" } => Ok(VarType::Uint256),
        fe::TypeDesc::Base { base: "u128" } => Ok(VarType::Uint128),
        fe::TypeDesc::Base { base: "u64" } => Ok(VarType::Uint64),
        fe::TypeDesc::Base { base: "u32" } => Ok(VarType::Uint32),
        fe::TypeDesc::Base { base: "u16" } => Ok(VarType::Uint16),
        fe::TypeDesc::Base { base: "u8" } => Ok(VarType::Uint8),
        fe::TypeDesc::Base { base: "i256" } => Ok(VarType::Int256),
        fe::TypeDesc::Base { base: "i128" } => Ok(VarType::Int128),
        fe::TypeDesc::Base { base: "i64" } => Ok(VarType::Int64),
        fe::TypeDesc::Base { base: "i32" } => Ok(VarType::Int32),
        fe::TypeDesc::Base { base: "i16" } => Ok(VarType::Int16),
        fe::TypeDesc::Base { base: "i8" } => Ok(VarType::Int8),
        fe::TypeDesc::Base { base: "bool" } => Ok(VarType::Bool),
        fe::TypeDesc::Base { base: "address" } => Ok(VarType::Address),
        fe::TypeDesc::Base { base } if &base[..6] == "string" => Ok(VarType::String),
        fe::TypeDesc::Base { base } => {
            Err(CompileError::str(format!("unrecognized type: {}", base)))
        }
        fe::TypeDesc::Array { typ, dimension } => {
            if let fe::TypeDesc::Base { base: "bytes" } = &typ.node {
                return Ok(VarType::FixedBytes(*dimension));
            }

            let inner = type_desc(type_defs, &typ.node)?;
            Ok(VarType::FixedArray(Box::new(inner), *dimension))
        }
        fe::TypeDesc::Map { .. } => Err(CompileError::static_str("maps not supported in ABI")),
        fe::TypeDesc::Tuple { .. } => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::abi::builder;
    use crate::abi::elements::VarType;
    use fe_parser::parsers;

    #[test]
    fn module_function() {
        let tokens = fe_parser::get_parse_tokens(
            "\
            \ncontract Foo:\
            \n  event Food:\
            \n    idx barge: u256
            \n  pub def __init__(x: address):\
            \n    pass\
            \n  def baz(x: address) -> u256:\
            \n    pass\
            \n  pub def bar(x: u256) -> u256[10]:\
            \n    pass",
        )
        .expect("unable to parse contract");

        let module = parsers::file_input(&tokens[..])
            .expect("unable to build module AST")
            .1
            .node;
        let abis = builder::module(&module).expect("unable to build ABIs");

        if let Some(abi) = abis.contracts.get("Foo") {
            assert_eq!(abi.events[0].name, "Food", "event name should be Food");
            assert_eq!(abi.functions.len(), 2, "too many functions in ABI");
            assert_eq!(abi.functions[0].name, "", "constructor not found in ABI");
            assert_eq!(
                abi.functions[0].inputs[0].typ,
                VarType::Address,
                "constructor has incorrect input value"
            );
            assert_eq!(
                abi.functions[1].name, "bar",
                "function \"bar\" not found in ABI"
            );
            assert_eq!(
                abi.functions[1].inputs[0].typ,
                VarType::Uint256,
                "function \"bar\" has incorrect input value"
            );
            assert_eq!(
                abi.functions[1].outputs[0].typ,
                VarType::FixedArray(Box::new(VarType::Uint256), 10),
                "function \"bar\" has incorrect output type"
            );
        } else {
            panic!("contract \"Foo\" not found in module")
        }
    }
}
