use crate::abi::elements::{
    Contract, Event, EventField, FuncInput, FuncOutput, FuncType, Function, ModuleABIs,
    VariableType,
};
use crate::errors::CompileError;
use std::collections::HashMap;
use vyper_parser::ast as vyp;

type TypeDefs<'a> = HashMap<&'a str, &'a vyp::TypeDesc<'a>>;
type TypeDef<'a> = (&'a str, &'a vyp::TypeDesc<'a>);

/// Parse a map of contract ABIs from the input `module`.
pub fn module<'a>(module: &'a vyp::Module<'a>) -> Result<ModuleABIs, CompileError> {
    let type_defs = module
        .body
        .iter()
        .filter(|s| is_type_def(&s.node))
        .map(|s| type_def(&s.node))
        .collect::<Result<TypeDefs<'a>, _>>()?;

    Ok(ModuleABIs(
        module
            .body
            .iter()
            .filter(|s| is_contract_def(&s.node))
            .map(|s| contract_def(&type_defs, &s.node))
            .collect::<Result<HashMap<_, _>, _>>()?,
    ))
}

fn contract_def<'a>(
    type_defs: &'a TypeDefs<'a>,
    stmt: &'a vyp::ModuleStmt<'a>,
) -> Result<(String, Contract), CompileError> {
    if let vyp::ModuleStmt::ContractDef { name, body } = stmt {
        let functions = body
            .iter()
            .filter(|s| is_pub_fun_def(&s.node))
            .map(|s| func_def(type_defs, &s.node))
            .collect::<Result<_, _>>()?;

        let events = body
            .iter()
            .filter(|s| is_event_def(&s.node))
            .map(|s| event_def(type_defs, &s.node))
            .collect::<Result<_, _>>()?;

        return Ok((name.node.to_string(), Contract { functions, events }));
    }

    Err(CompileError::static_str("Not a contract definition"))
}

fn event_def<'a>(
    type_defs: &'a TypeDefs<'a>,
    stmt: &'a vyp::ContractStmt<'a>,
) -> Result<Event, CompileError> {
    if let vyp::ContractStmt::EventDef { name, fields } = stmt {
        let fields = fields
            .iter()
            .map(|f| event_field(type_defs, &f.node))
            .collect::<Result<_, _>>()?;

        return Ok(Event {
            name: name.node.to_string(),
            typ: "event".to_string(),
            fields,
            anonymous: false,
        });
    }

    Err(CompileError::static_str("Not an event definition"))
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
    stmt: &'a vyp::ContractStmt<'a>,
) -> Result<Function, CompileError> {
    if let vyp::ContractStmt::FuncDef {
        qual: _,
        name,
        args,
        return_type,
        body: _,
    } = stmt
    {
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

        return Ok(Function {
            name: String::from(name.node),
            typ: FuncType::Function,
            inputs,
            outputs,
        });
    }

    Err(CompileError::static_str("Not a "))
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

fn type_def<'a>(stmt: &'a vyp::ModuleStmt<'a>) -> Result<TypeDef<'a>, CompileError> {
    if let vyp::ModuleStmt::TypeDef { name, typ } = stmt {
        return Ok((name.node, &typ.node));
    }

    Err(CompileError::static_str("Not a TypeDef"))
}

fn type_desc<'a>(
    type_defs: &'a TypeDefs<'a>,
    typ: &'a vyp::TypeDesc<'a>,
) -> Result<VariableType, CompileError> {
    if let vyp::TypeDesc::Base { base } = typ {
        if let Some(custom_type) = type_defs.get(base) {
            return type_desc(&HashMap::new(), custom_type);
        }
    }

    match typ {
        vyp::TypeDesc::Base { base: "uint256" } => Ok(VariableType::Uint256),
        vyp::TypeDesc::Base { base: "address" } => Ok(VariableType::Address),
        vyp::TypeDesc::Array { typ, dimension } => {
            if let vyp::TypeDesc::Base { base: "bytes" } = &typ.node {
                return Ok(VariableType::FixedBytes(*dimension));
            }

            let inner = type_desc(type_defs, &typ.node)?;
            Ok(VariableType::FixedArray(Box::new(inner), *dimension))
        }
        _ => Err(CompileError::static_str("Unrecognized Vyper type")),
    }
}

fn is_event_def(stmt: &vyp::ContractStmt) -> bool {
    if let vyp::ContractStmt::EventDef { .. } = stmt {
        return true;
    }

    false
}

fn is_pub_fun_def(stmt: &vyp::ContractStmt) -> bool {
    if let vyp::ContractStmt::FuncDef {
        qual: Some(qual), ..
    } = stmt
    {
        return qual.node == vyp::FuncQual::Pub;
    }

    false
}

fn is_type_def(stmt: &vyp::ModuleStmt) -> bool {
    if let vyp::ModuleStmt::TypeDef { .. } = stmt {
        return true;
    }

    false
}

fn is_contract_def(stmt: &vyp::ModuleStmt) -> bool {
    if let vyp::ModuleStmt::ContractDef { .. } = stmt {
        return true;
    }

    false
}

#[cfg(test)]
mod tests {
    use crate::abi::builder;
    use crate::abi::elements::VariableType;
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

        if let Some(abi) = abis.0.get("Foo") {
            assert_eq!(abi.events[0].name, "Food", "event name should be Food");
            assert_eq!(abi.functions.len(), 1, "too many functions in ABI");
            assert_eq!(
                abi.functions[0].name, "bar",
                "function \"bar\" not found in ABI"
            );
            assert_eq!(
                abi.functions[0].inputs[0].typ,
                VariableType::Uint256,
                "function \"bar\" has incorrect input value"
            );
            assert_eq!(
                abi.functions[0].outputs[0].typ,
                VariableType::FixedArray(Box::new(VariableType::Uint256), 10),
                "function \"bar\" has incorrect output type"
            );
        } else {
            panic!("contract \"Foo\" not found in module")
        }
    }
}
