use fe_common::Span;
use fe_parser::{
    ast::{ContractStmt, Expr, FuncStmt, Module, ModuleStmt, SmolStr},
    node::Node,
};

pub fn find_span_contain_fn_name(ast: &Module, bytes_position: usize) -> Option<Span> {
    let mut node: Option<Span> = None;

    for stmt in &ast.body {
        node = match stmt {
            ModuleStmt::Contract(contract) => {
                for contract_stmt in &contract.kind.body {
                    if let ContractStmt::Function(contract_func) = contract_stmt {
                        let sig = &contract_func.kind.sig;

                        if sig.span.end >= bytes_position && sig.span.start <= bytes_position {
                            return Some(sig.span);
                        }

                        for func_stmt in &contract_func.kind.body {
                            if let FuncStmt::Expr { value } = &func_stmt.kind {
                                if let Expr::Name(_) = &value.kind {
                                    if value.span.end >= bytes_position
                                        && value.span.start <= bytes_position
                                    {
                                        return Some(sig.span);
                                    }
                                }

                                if let Expr::Call {
                                    func,
                                    generic_args: _,
                                    args: _,
                                } = &value.kind
                                {
                                    if let Expr::Attribute { value, attr } = &func.kind {
                                        if attr.span.end >= bytes_position
                                            && attr.span.start <= bytes_position
                                        {
                                            return Some(attr.span);
                                        }
                                    }
                                    if value.span.end >= bytes_position
                                        && value.span.start <= bytes_position
                                    {
                                        return Some(func.span);
                                    }
                                }
                            }
                        }
                    }
                }
                None
            }
            ModuleStmt::Function(func) => {
                let sig = &func.kind.sig;
                if sig.span.end >= bytes_position && sig.span.start <= bytes_position {
                    return Some(sig.span);
                } else {
                    return None;
                }
            }
            _ => None,
        };

        if node != None {
            return node;
        }
    }

    return node;
}
