use fe_common::db::TestDb;
use fe_common::diagnostics::print_diagnostics;
use fe_common::utils::ron::to_ron_string_pretty;
use fe_common::SourceFileId;
use fe_parser::grammar::{expressions, functions, module, types};
use fe_parser::node::Node;
use fe_parser::{ast, ParseResult, Parser};
use insta::assert_snapshot;
use serde::Serialize;
use wasm_bindgen_test::wasm_bindgen_test;

pub fn ast_string<F, T>(test_name: &str, mut parse_fn: F, src: &str) -> String
where
    F: FnMut(&mut Parser) -> ParseResult<T>,
    T: Serialize,
{
    let mut db = TestDb::default();
    let id = SourceFileId::new_local(&mut db, test_name, src.into());
    let mut parser = Parser::new(id, src);

    if let Ok(ast) = parse_fn(&mut parser) {
        if !parser.diagnostics.is_empty() {
            print_diagnostics(&db, &parser.diagnostics);
            panic!("parse error");
        }
        to_ron_string_pretty(&ast).unwrap()
    } else {
        if parser.diagnostics.is_empty() {
            eprintln!("parsing failed, but no diagnostics were generated. this should be fixed.");
            let next = parser.next();
            if let Ok(tok) = next {
                parser.error(
                    tok.span,
                    "this is the next token at time of parsing failure",
                );
                print_diagnostics(&db, &parser.diagnostics);
            } else {
                eprintln!("parser is at end of file");
            }
        } else {
            print_diagnostics(&db, &parser.diagnostics);
        }
        panic!("parse failed");
    }
}

fn try_parse_module(par: &mut Parser) -> ParseResult<Node<ast::Module>> {
    // This is just to make `ast_string` above work; it's fine.
    Ok(module::parse_module(par))
}

macro_rules! test_parse {
    ($name:ident, $parse_fn:expr, $src:expr) => {
        #[test]
        #[wasm_bindgen_test]
        fn $name() {
            if cfg!(target_arch = "wasm32") {
                fe_common::assert_snapshot_wasm!(
                    concat!("snapshots/cases__parse_ast__", stringify!($name), ".snap"),
                    ast_string(stringify!($name), $parse_fn, $src)
                );
            } else {
                assert_snapshot!(ast_string(stringify!($name), $parse_fn, $src));
            }
        }
    };
}

test_parse! { expr_call1, expressions::parse_expr, "foo()" }
test_parse! { expr_call2, expressions::parse_expr, "foo(1,2,x:3)" }
test_parse! { expr_call3, expressions::parse_expr, "bing.foo<Bar>(x:3)" }
test_parse! { expr_call4, expressions::parse_expr, "bang.bing.foo<Bar, Baz>(26, 42)" }
test_parse! { expr_attr1, expressions::parse_expr, "foo.bar[0][y]" }
test_parse! { expr_attr2, expressions::parse_expr, "a[x].b[y](1)" }
test_parse! { expr_num1, expressions::parse_expr, "12345" }
test_parse! { expr_num2, expressions::parse_expr, "00001" }
test_parse! { expr_num3, expressions::parse_expr, "1000_000" }
test_parse! { expr_hex1, expressions::parse_expr, "0xbeefbeef" }
test_parse! { expr_hex2, expressions::parse_expr, "0xFEED1234" }
test_parse! { expr_path_call, expressions::parse_expr, "foo::bar::abc1()" }
test_parse! { expr_string, expressions::parse_expr, r#""hi \tmom\n""# }
test_parse! { expr_list, expressions::parse_expr, "[]" }
test_parse! { expr_list2, expressions::parse_expr, "[x, y, z,]" }
test_parse! { expr_repeat, expressions::parse_expr, "[true; 42]" }
test_parse! { expr_repeat2, expressions::parse_expr, "[5 + 4; 26]" }
test_parse! { expr_ternary, expressions::parse_expr, "x + 1 if y + 2 else z + 3" }
test_parse! { expr_group, expressions::parse_expr, "(1 + 2) * 3" }
test_parse! { expr_tuple1, expressions::parse_expr, "(1,)" }
test_parse! { expr_tuple2, expressions::parse_expr, "(1, 2, \n 3)" }
test_parse! { expr_tuple3, expressions::parse_expr, "(1, (2 + 3), (3 * 4, 5))" }
test_parse! { expr_unit, expressions::parse_expr, "()" }

test_parse! { ops_not, expressions::parse_expr, "x and not y" }
test_parse! { ops_math, expressions::parse_expr, "a + b * -c ** d / e % f" }
test_parse! { ops_neg, expressions::parse_expr, "-x" }
test_parse! { ops_bnot, expressions::parse_expr, "~x" }
// bitwise op precedence: shift > and > xor > or
test_parse! { ops_bit1, expressions::parse_expr, "a & b >> c" }
test_parse! { ops_bit2, expressions::parse_expr, "a ^ b & c" }
test_parse! { ops_bit3, expressions::parse_expr, "a | b ^ c" }
test_parse! { ops_shift, expressions::parse_expr, "a << b >> c" }
test_parse! { ops_bool, expressions::parse_expr, "a or b and c" }

test_parse! { stmt_assert_no_msg, functions::parse_stmt, "assert x == y" }
test_parse! { stmt_assert_msg, functions::parse_stmt, "assert x == y, z" }
test_parse! { stmt_aug_add, functions::parse_stmt, "x += y" }
test_parse! { stmt_aug_sub, functions::parse_stmt, "x -= y" }
test_parse! { stmt_aug_mul, functions::parse_stmt, "x *= y" }
test_parse! { stmt_aug_div, functions::parse_stmt, "x /= y" }
test_parse! { stmt_aug_mod, functions::parse_stmt, "x %= y" }
test_parse! { stmt_aug_and, functions::parse_stmt, "x &= y" }
test_parse! { stmt_aug_or, functions::parse_stmt, "x |= y" }
test_parse! { stmt_aug_xor, functions::parse_stmt, "x ^= y" }
test_parse! { stmt_aug_lsh, functions::parse_stmt, "x <<= y" }
test_parse! { stmt_aug_rsh, functions::parse_stmt, "x >>= y" }
test_parse! { stmt_aug_exp, functions::parse_stmt, "x **= y" }
test_parse! { stmt_path_type, functions::parse_stmt, "let x: foo::Bar = foo::Bar(1, 2)" }
test_parse! { stmt_return1, functions::parse_stmt, "return" }
test_parse! { stmt_return2, functions::parse_stmt, "return x" }
test_parse! { stmt_return3, functions::parse_stmt, "return not x" }
test_parse! { stmt_revert1, functions::parse_stmt, "revert" }
test_parse! { stmt_revert2, functions::parse_stmt, "revert something" }

test_parse! { stmt_if, functions::parse_stmt, "if a { \n b }" }
test_parse! { stmt_if2, functions::parse_stmt, "if a { b } else if c { d } else if e { \n f } \n else {\n g }" }
test_parse! { stmt_match, functions::parse_stmt, r#"match my_enum {
    MyEnum::Unit => {
        return 0
    }
    MyEnum::Tuple(x, y) => {
        let sum: i32 = x + y;
        return sum
    }
    _ => {
        return -1
    }
}"# }
test_parse! { stmt_match2, functions::parse_stmt, r#"match my_enum {
    MyEnum::Unit | MyEnum::Tuple(_, _) => {
        return 0
    }
    _ => {
        return -1
    }
}"# }
test_parse! { stmt_match3, functions::parse_stmt, r#"match my_enum {
    mymod::MyS {x: x, y: true}  => {
        return x
    }
    mymod::MyS {x: _, y: false} => {
        return 0
    }
    mymod::MyS {x: x, ..} => {
        return x - 1
    }
}"# }
test_parse! { stmt_match4, functions::parse_stmt, r#"match my_enum {
    mymod::MyS {}  => {
        return 1
    }
}"# }
test_parse! { stmt_while, functions::parse_stmt, "while a > 5 { \n a -= 1 }" }
test_parse! { stmt_for, functions::parse_stmt, "for a in b[0] {}" }
test_parse! { stmt_var_decl_name, functions::parse_stmt, "let foo: u256 = 1" }
test_parse! { stmt_var_decl_mut, functions::parse_stmt, "let mut x: Foo" }
test_parse! { stmt_var_decl_tuple, functions::parse_stmt, "let (foo, bar): (u256, u256) = (10, 10)" }
test_parse! { stmt_var_decl_tuples, functions::parse_stmt, "let (a, (b, (c, d))): x" }
test_parse! { stmt_semicolons, functions::parse_stmt, "if a { b; c; d; for x in y {}; }" }
test_parse! { type_def, try_parse_module, "type X = Map<address, u256>" }
test_parse! { pub_type_def, try_parse_module, "pub type X = Map<address, u256>" }
test_parse! { type_name, types::parse_type_desc, "MyType" }
test_parse! { type_array, types::parse_type_desc, "Array<address, 25>" }
test_parse! { type_3d, types::parse_type_desc, "Array<Array<Array<u256, 4>, 4>, 4>" }
test_parse! { type_string, types::parse_type_desc, "string<100>" }
test_parse! { type_generic, types::parse_type_desc, "foo<a, b<c>, Array<d, 10>>" }
test_parse! { type_generic_int, types::parse_type_desc, "foo<1, 2>" }
test_parse! { type_map1, types::parse_type_desc, "Map<address, u256>" }
test_parse! { type_map2, types::parse_type_desc, "Map<address, Map<u8, u256>>" }
test_parse! { type_map3, types::parse_type_desc, "Map<address, Map<u8, Map<u8, u8>>>" }
test_parse! { type_map4, types::parse_type_desc, "map < address , map < u8, u256 > >" }
test_parse! { type_tuple, types::parse_type_desc, "(u8, u16, address, Map<u8, u8>)" }
test_parse! { type_unit, types::parse_type_desc, "()" }

test_parse! { fn_def, try_parse_module, "fn transfer(from sender: address, to recip: address, _ val: u64) -> bool {\n false \n}"}

test_parse! { fn_def_generic, try_parse_module, "fn foo<T, R: Event>(this: T, that: R, _ val: u64) -> bool { false }"}
test_parse! { fn_def_pub, try_parse_module, "pub fn foo21(x: bool, y: address,) -> bool { x }"}
test_parse! { fn_def_unsafe, try_parse_module, "unsafe fn foo21(x: bool, y: address,) -> bool {\n x\n}"}
test_parse! { fn_def_pub_unsafe, try_parse_module, "pub unsafe fn foo21(x: bool, y: address,) -> bool{x}"}
test_parse! { fn_def_mut_args, try_parse_module, "fn transfer(mut from sender: address, to recip: address, mut val: u64) -> bool { \n return false \n}"}
test_parse! { const_def, try_parse_module, "const FOO: i32 = 1" }
test_parse! { pub_const_def, try_parse_module, "pub const FOO: i32 = 1" }
test_parse! { pragma1, module::parse_pragma, "pragma 0.1.0" }
test_parse! { pragma2, module::parse_pragma, "pragma 0.1.0-alpha" }
test_parse! { pragma3, module::parse_pragma, "pragma >= 1.2, < 1.5" }

test_parse! { use_simple1, module::parse_use, "use foo::bar" }
test_parse! { use_simple2, module::parse_use, "use foo::bar as baz" }
test_parse! { use_glob, module::parse_use, "use foo::bar::*" }
test_parse! { use_nested1, module::parse_use, "use foo::bar::{bing::*, bang::big, bass as fish, bong::{hello as hi, goodbye}}" }
test_parse! { use_nested2, module::parse_use, r#"use std::bar::{
    bing::*,
    bad::{food as burger, barge::*, bill::bob::{jkl::*}},
    evm as mve
}"#
}
test_parse! { struct_def, try_parse_module, r#"struct S {
  #indexed
  x: address
  pub y: u8
  z: u8
  pub a: Map<u8, foo>

  pub fn foo(self) -> u8 {
    return self.z + self.y
  }
  unsafe fn bar() {}
}"# }
test_parse! { empty_struct_def, try_parse_module, "struct S {}" }

test_parse! { enum_def, try_parse_module, r#"enum E {
    Unit1
    Tuple1(i32, String<10>)
    Unit2
    Tuple2(Array<u32, 10>, u256)
}"# }
test_parse! { enum_enum_def, try_parse_module, r#"enum E {}"# }

test_parse! { contract_def, try_parse_module, r#"contract Foo {
  x: address
  pub y: u8
  pub const z: Map<u8, address>

  pub fn foo() -> u8 {
    return 10
  }
}
"# }

test_parse! { empty_contract_def, try_parse_module, "contract Foo {}" }

test_parse! { pub_contract_def, try_parse_module, r#"
pub contract Foo {
    pub fn foo() -> u8 {
      return 10
    }
}"# }

test_parse! { module_stmts, try_parse_module, r#"
pragma 0.5.0

use foo::bar::{
    bing as bong,
    food::*
}

type X = Map<u8, u16>

pub fn double(x: u8) -> u8 {
    return x * 2
}

fn secret() -> u8 { return 0xBEEF }

contract A {
    pub const x: u256 = 10
}

contract B {
    pub x: X
}"# }

test_parse! { guest_book, try_parse_module, r#"
type BookMsg = Array<bytes, 100>

struct Signed {
    #indexed
    book_msg: BookMsg
}

contract GuestBook {
    pub guest_book: Map<address, BookMsg>

    pub fn sign(self, book_msg: BookMsg) {
        self.guest_book[msg.sender] = book_msg

        ctx.emit(Signed(book_msg: book_msg))
    }
    pub fn get_msg(self, addr: address) -> BookMsg {
        return self.guest_book[addr]
    }
}"# }

test_parse! { module_level_events, try_parse_module, r#"
struct Transfer {
    #indexed
    sender: address
    #indexed
    receiver: address
    value: u256
}
contract Foo {
    fn transfer(ctx: Context, to: address, value: u256) {
        ctx.emit(Transfer(sender: msg.sender, receiver: to, value))
    }
}
"# }
