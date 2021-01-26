use crate::yul::names;
use fe_analyzer::namespace::types::Struct;
use yultsur::*;

pub fn new(struct_type: &Struct, params: Vec<yul::Expression>) -> yul::Expression {
    let function_name = names::struct_new_call(&struct_type.name);
    expression! { [function_name]([params...]) }
}

pub fn get_attribute(struct_type: &Struct, ptr_name: &str, field_name: &str) -> yul::Expression {
    let function_name = names::struct_getter_call(&struct_type.name, field_name);
    let ptr_name_exp = identifier_expression! {(ptr_name)};
    expression! { [function_name]([ptr_name_exp]) }
}

#[cfg(test)]
mod tests {
    use crate::yul::operations::structs;
    use fe_analyzer::namespace::types::{
        Base,
        Struct,
    };
    use yultsur::*;

    #[test]
    fn test_new() {
        let mut val = Struct::new("Foo");
        val.add_field("bar", &Base::Bool);
        val.add_field("bar2", &Base::Bool);
        let params = vec![
            identifier_expression! { (1) },
            identifier_expression! { (2) },
        ];
        assert_eq!(
            structs::new(&val, params).to_string(),
            "struct_Foo_new(1, 2)"
        )
    }
}
