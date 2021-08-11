use crate::names;
use crate::types::EvmSized;
use fe_analyzer::namespace::types::FixedSize;
use yultsur::*;

/// Generate a YUL function that can be used to create an instance of a struct
pub fn generate_new_fn(struct_name: &str, fields: &[(String, FixedSize)]) -> yul::Statement {
    let function_name = names::struct_new_call(struct_name);

    if fields.is_empty() {
        // We return 0 here because it is safe to assume that we never write to an empty
        // struct. If we end up writing to an empty struct that's an actual Fe
        // bug.
        return function_definition! {
            function [function_name]() -> return_val {
                 (return_val := 0)
            }
        };
    }

    let params = fields
        .iter()
        .map(|(name, _)| {
            identifier! {(name)}
        })
        .collect::<Vec<_>>();

    let body = fields
        .iter()
        .enumerate()
        .map(|(index, (name, _))| {
            if index == 0 {
                let param_identifier_exp = identifier_expression! {(name)};
                statements! {
                    (return_val := alloc(32))
                    (mstore(return_val, [param_identifier_exp]))
                }
            } else {
                let ptr_identifier = format!("{}_ptr", name);
                let ptr_identifier = identifier! {(ptr_identifier)};
                let ptr_identifier_exp = identifier_expression! {(ptr_identifier)};
                let param_identifier_exp = identifier_expression! {(name)};
                statements! {
                    (let [ptr_identifier] := alloc(32))
                    (mstore([ptr_identifier_exp], [param_identifier_exp]))
                }
            }
        })
        .flatten()
        .collect::<Vec<_>>();

    function_definition! {
        function [function_name]([params...]) -> return_val {
            [body...]
        }
    }
}

/// Generate a YUL function that can be used to read a property of a struct
pub fn generate_get_fn(
    struct_name: &str,
    (field_name, field_type): &(String, FixedSize),
    field_index: usize,
) -> yul::Statement {
    let function_name = names::struct_getter_call(struct_name, field_name);

    // The value of each field occupies 32 bytes. This includes values with sizes
    // less than 32 bytes. So, when we get the pointer to the value of a struct
    // field, we must take into consideration the left-padding. The left-padding is
    // equal to the difference between the value's size and 32 bytes, so we end up
    // adding the word offset and the byte offset.
    let field_offset = field_index * 32 + (32 - field_type.size());

    let offset = literal_expression! { (field_offset) };
    function_definition! {
        function [function_name](ptr) -> return_val {
             (return_val := add(ptr, [offset]))
        }
    }
}

/// Builds a set of functions used to interact with structs used in a contract
pub fn struct_apis(name: &str, fields: &[(String, FixedSize)]) -> Vec<yul::Statement> {
    [
        vec![generate_new_fn(name, fields)],
        fields
            .iter()
            .enumerate()
            .map(|(index, field)| generate_get_fn(name, field, index))
            .collect(),
    ]
    .concat()
}
