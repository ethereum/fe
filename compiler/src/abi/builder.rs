use crate::abi::elements::{
    Contract, Event, EventField, FuncInput, FuncOutput, FuncType, Function, ModuleABIs,
    VarType,
};
use crate::errors::CompileError;
use std::collections::HashMap;
use vyper_parser::ast as vyp;
use vyper_parser::span::Spanned;

type TypeDefs<'a> = HashMap<&'a str, &'a vyp::TypeDesc<'a>>;

/// Parse a map of contract ABIs from the input `module`.
pub fn module<'a>(module: &'a vyp::Module<'a>) -> Result<ModuleABIs, CompileError> {
    let mut type_defs = TypeDefs::new();

    module.body.iter().try_fold(ModuleABIs::new(), |mut m, s| {
        match &s.node {
            vyp::ModuleStmt::TypeDef { name, typ } => {
                if type_defs.insert(name.node, &typ.node).is_some() {
                    return Err(CompileError::static_str("duplicate type definition"));
                }
            }
            vyp::ModuleStmt::ContractDef { name, body } => {
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
    body: &'a Vec<Spanned<vyp::ContractStmt<'a>>>,
) -> Result<Contract, CompileError> {
    body.iter().try_fold(Contract::new(), |mut c, s| {
        match &s.node {
            vyp::ContractStmt::FuncDef {
                qual,
                name,
                args,
                return_type,
                ..
            } => {
                if let Some(qual) = qual {
                    if qual.node == vyp::FuncQual::Pub {
                        c.functions.push(func_def(
                            type_defs,
                            name.node.to_string(),
                            args,
                            return_type,
                        )?)
                    }
                }
            }
            vyp::ContractStmt::EventDef { name, fields } => {
                c.events
                    .push(event_def(type_defs, name.node.to_string(), fields)?)
            }
            vyp::ContractStmt::ContractField { .. } => {}
        }

        Ok(c)
    })
}

fn event_def<'a>(
    type_defs: &'a TypeDefs<'a>,
    name: String,
    fields: &'a Vec<Spanned<vyp::EventField<'a>>>,
) -> Result<Event, CompileError> {
    let fields = fields
        .iter()
        .map(|f| event_field(type_defs, &f.node))
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
    field: &'a vyp::EventField<'a>,
) -> Result<EventField, CompileError> {
    Ok(EventField {
        name: String::from(field.name.node),
        typ: type_desc(&type_defs, &field.typ.node)?,
        indexed: false,
    })
}

fn func_def<'a>(
    type_defs: &'a TypeDefs<'a>,
    name: String,
    args: &'a Vec<Spanned<vyp::FuncDefArg<'a>>>,
    return_type: &'a Option<Spanned<vyp::TypeDesc<'a>>>,
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

    Ok(Function {
        name,
        typ: FuncType::Function,
        inputs,
        outputs,
    })
}

fn func_def_arg<'a>(
    type_defs: &'a TypeDefs<'a>,
    arg: &'a vyp::FuncDefArg<'a>,
) -> Result<FuncInput, CompileError> {
    Ok(FuncInput {
        name: String::from(arg.name.node),
        typ: type_desc(&type_defs, &arg.typ.node)?,
    })
}

fn type_desc<'a>(
    type_defs: &'a TypeDefs<'a>,
    typ: &'a vyp::TypeDesc<'a>,
) -> Result<VarType, CompileError> {
    if let vyp::TypeDesc::Base { base } = typ {
        if let Some(custom_type) = type_defs.get(base) {
            return type_desc(type_defs, custom_type);
        }
    }

    match typ {
        vyp::TypeDesc::Base { base: "uint256" } => Ok(VarType::Uint256),
        vyp::TypeDesc::Base { base: "address" } => Ok(VarType::Address),
        vyp::TypeDesc::Base { base } => {
            Err(CompileError::str(format!("unrecognized type: {}", base)))
        }
        vyp::TypeDesc::Array { typ, dimension } => {
            if let vyp::TypeDesc::Base { base: "bytes" } = &typ.node {
                return Ok(VarType::FixedBytes(*dimension));
            }

            let inner = type_desc(type_defs, &typ.node)?;
            Ok(VarType::FixedArray(Box::new(inner), *dimension))
        }
        vyp::TypeDesc::Map { .. } => Err(CompileError::static_str("maps not supported in ABI")),
    }
}

#[cfg(test)]
mod tests {
    use crate::abi::builder;
    use crate::abi::elements::VarType;
    use vyper_parser::parsers;

    #[test]
    fn module_function() {
        let tokens = vyper_parser::get_parse_tokens(
            "\
            \ncontract Foo:\
            \n  event Food:\
            \n    idx barge: uint256
            \n  def baz(x: uint256) -> uint256:\
            \n    pass\
            \n  pub def bar(x: uint256) -> uint256[10]:\
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
            assert_eq!(abi.functions.len(), 1, "too many functions in ABI");
            assert_eq!(
                abi.functions[0].name, "bar",
                "function \"bar\" not found in ABI"
            );
            assert_eq!(
                abi.functions[0].inputs[0].typ,
                VarType::Uint256,
                "function \"bar\" has incorrect input value"
            );
            assert_eq!(
                abi.functions[0].outputs[0].typ,
                VarType::FixedArray(Box::new(VarType::Uint256), 10),
                "function \"bar\" has incorrect output type"
            );
        } else {
            panic!("contract \"Foo\" not found in module")
        }
    }
}
