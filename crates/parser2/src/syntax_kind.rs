//! This module contains the definition of the [`SyntaxKind`].

use logos::Logos;

/// The definition of the `SyntaxKind'.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Logos)]
#[repr(u16)]
pub enum SyntaxKind {
    // Atom kinds. These are leaf nodes.
    #[error]
    InvalidToken = 0,
    #[regex(r"[\n|\r\n|\r]+")]
    Newline,
    #[regex(r"[ ]+")]
    WhiteSpace,
    /// `foo`
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,
    /// `1`, `0b1010`, `0o77`, `0xff`
    #[regex("[0-9]+(?:_[0-9]+)*")]
    #[regex("0[bB][0-1]+")]
    #[regex("0[oO][0-7]+")]
    #[regex("0[xX][0-9a-fA-F]+")]
    Int,
    /// "MyString"
    #[regex(r#""([^"\\]|\\.)*""#)]
    String,
    /// `(`
    #[token("(")]
    LParen,
    /// `)`
    #[token(")")]
    RParen,
    /// `{`
    #[token("{")]
    LBrace,
    /// `}`
    #[token("}")]
    RBrace,
    /// `[`
    #[token("[")]
    LBracket,
    /// `]`
    #[token("]")]
    RBracket,
    /// `:`
    #[token(":")]
    Colon,
    /// `::`
    #[token("::")]
    Colon2,
    /// `;`
    #[token(";")]
    SemiColon,
    /// `.`
    #[token(".")]
    Dot,
    /// `..`
    #[token("..")]
    Dot2,
    /// `,`
    #[token(",")]
    Comma,
    /// `->`
    #[token("->")]
    Arrow,
    /// `=>`
    #[token("=>")]
    FatArrow,
    /// `_`
    #[token("_")]
    Underscore,
    /// `#`
    #[token("#")]
    Pound,
    /// `// Comment`
    #[regex(r"//[^\n\r]*")]
    Comment,
    /// `/// DocComment`
    #[regex(r"///[^\n\r]*")]
    DocComment,

    /// `+`
    #[token("+")]
    Plus,
    /// `-`
    #[token("-")]
    Minus,
    /// `*`
    #[token("*")]
    Star,
    #[token("**")]
    Star2,
    /// `/`
    #[token("/")]
    Slash,
    /// `%`
    #[token("%")]
    Percent,
    #[token("~")]
    Tilde,
    #[token("!")]
    Not,
    #[token("^")]
    Hat,
    /// `&`
    #[token("&")]
    Amp,
    /// `&&`
    #[token("&&")]
    Amp2,
    /// `|`
    #[token("|")]
    Pipe,
    /// `||`
    #[token("||")]
    Pipe2,
    /// `<`
    #[token("<")]
    Lt,
    /// `>`
    #[token(">")]
    Gt,
    /// `=`
    #[token("=")]
    Eq,
    /// `==`
    #[token("==")]
    Eq2,
    /// `!=`
    #[token("!=")]
    NotEq,

    /// `as`'
    #[token("as")]
    AsKw,
    /// `true'
    #[token("true")]
    TrueKw,
    /// `false`
    #[token("false")]
    FalseKw,
    /// `break`
    #[token("break")]
    BreakKw,
    /// `continue`
    #[token("continue")]
    ContinueKw,
    /// `contract`
    #[token("contract")]
    ContractKw,
    /// `fn`
    #[token("fn")]
    FnKw,
    /// `mod`
    #[token("mod")]
    ModKw,
    /// `const`
    #[token("const")]
    ConstKw,
    /// `if`
    #[token("if")]
    IfKw,
    /// `else`
    #[token("else")]
    ElseKw,
    /// `match`
    #[token("match")]
    MatchKw,
    /// `for`
    #[token("for")]
    ForKw,
    /// `in`
    #[token("in")]
    InKw,
    /// `where`
    #[token("where")]
    WhereKw,
    /// `while`
    #[token("while")]
    WhileKw,
    /// `pub`
    #[token("pub")]
    PubKw,
    /// `return`
    #[token("return")]
    ReturnKw,
    /// `self`
    #[token("self")]
    SelfKw,
    #[token("Self")]
    SelfTypeKw,
    /// `struct`
    #[token("struct")]
    StructKw,
    /// `enum`
    #[token("enum")]
    EnumKw,
    /// `trait`
    #[token("trait")]
    TraitKw,
    /// `impl`
    #[token("impl")]
    ImplKw,
    /// `type`
    #[token("type")]
    TypeKw,
    /// `let`
    #[token("let")]
    LetKw,
    /// `mut`
    #[token("mut")]
    MutKw,
    /// `use`
    #[token("use")]
    UseKw,
    /// `extern`
    #[token("extern")]
    ExternKw,
    /// `unsafe`
    #[token("unsafe")]
    UnsafeKw,
    /// `ingot`
    #[token("ingot")]
    IngotKw,
    /// `super`
    #[token("super")]
    SuperKw,

    /// `<<`
    LShift,
    /// `>>`
    RShift,
    /// `<=`
    LtEq,
    /// `>=`
    GtEq,

    /// `1', `false`, `"String"
    Lit,

    // Expressions. These are non-leaf nodes.
    /// { statement-list }
    BlockExpr,
    /// `x + 1`
    BinExpr,
    /// `!x`
    UnExpr,
    /// `foo(x, y)`
    CallExpr,
    /// `(arg: 1, y)`
    CallArgList,
    /// `arg: 1`, `y`
    CallArg,
    /// `foo.bar(x, y)`
    MethodCallExpr,
    /// `<i32, u256, N + 2>`
    GenericArgList,
    /// `T`
    TypeGenericArg,
    /// `1`
    ConstGenericArg,
    /// `FOO::Bar`
    PathExpr,
    /// `Foo { x: 1, y: "String"` }`
    RecordInitExpr,
    /// `{ x: 1, y: "String"` }`
    RecordFieldList,
    /// `x: 1`
    RecordField,
    /// `foo.bar`, `foo.0`
    FieldExpr,
    /// `foo[1]`
    IndexExpr,
    /// `(x ,y)`
    TupleExpr,
    /// `[x, y, z]`
    ArrayExpr,
    /// `[x; 4]`
    ArrayRepExpr,
    /// `1`
    LitExpr,
    /// `if x { 1 } else { 2 }`
    IfExpr,
    /// `match x { pat => { .. } }`
    MatchExpr,
    /// `(1 + 2)`
    ParenExpr,

    // Statements. These are non-leaf nodes.
    /// `let x = 1`
    LetStmt,
    /// `x = 1`
    AssignStmt,
    /// `x += 1`
    AugAssignStmt,
    /// `for x in y {..}`
    ForStmt,
    /// `while expr {..}`
    WhileStmt,
    /// `continue`
    ContinueStmt,
    /// `break`
    BreakStmt,

    /// `return 1`
    ReturnStmt,
    /// `1`
    ExprStmt,

    // Patterns. These are non-leaf nodes.
    /// `_`
    WildCardPat,
    /// `..`
    RestPat,
    /// `x`
    LitPat,
    /// `(x, y)`
    TuplePat,
    /// `(x, y)`
    TuplePatElemList,
    /// `Enum::Variant`
    PathPat,
    /// `Enum::Variant(x, y)`
    PathTuplePat,
    /// `Struct {x, y}`
    RecordPat,
    /// `{a: b, y}`
    RecordPatFieldList,
    /// `a: b`
    RecordPatField,
    /// `pat1 | pat2`
    OrPat,

    // MatchArms.
    // `pat => { stmtlist }`
    MatchArm,
    MatchArmList,

    // Items. These are non-leaf nodes.
    Item,
    /// `mod s { .. }`
    Mod,
    /// `fn foo(x: i32) -> i32 { .. }`
    Func,
    /// `struct Foo { .. }`
    Struct,
    /// `contract Foo { .. }`
    Contract,
    /// `enum Foo { .. }`
    Enum,
    /// `type Foo = i32`
    TypeAlias,
    /// `impl Foo { .. }`
    Impl,
    /// `{ fn ... }`
    ImplItemList,
    /// `trait Foo {..}`
    Trait,
    /// `{ fn foo() {..} }`
    TraitItemList,
    /// `impl Trait for Foo { .. }`
    ImplTrait,
    /// `{ fn foo() {..} }`
    ImplTraitItemList,
    /// `const FOO: i32 = 1`
    Const,
    /// `use foo::{Foo as Foo1, bar::Baz}`
    Use,
    /// `foo::{Foo as Foo1, bar::Baz}`
    UseTree,
    /// `{Foo as Foo1, bar::Baz}`
    UseTreeList,
    /// `Foo::Bar`, `Foo::*`,`*`.
    UsePath,
    /// `Foo`, `self`
    UsePathSegment,
    /// `as Foo`
    UseTreeRename,
    /// `extern { .. }`
    Extern,
    /// `extern { .. }`
    ExternItemList,
    ItemList,

    /// `pub unsafe `
    ItemModifier,

    // Types. These are non-leaf nodes.
    /// `*i32`
    PtrType,
    /// `foo::Type<T, U + 2>`
    PathType,
    /// `Self`
    SelfType,
    /// `(i32, foo::Bar)`
    TupleType,
    /// `[i32; 4]`
    ArrayType,

    // Paths. These are non-leaf nodes.
    /// `Segment1::Segment2`
    Path,
    /// `Segment1`
    PathSegment,

    /// `#attr`
    Attr,
    /// `(key1: value1, key2: value2)`
    AttrArgList,
    /// `key: value`
    AttrArg,
    /// `/// Comment`
    DocCommentAttr,
    AttrList,

    /// `pub`
    Visibility,

    /// `x: i32`
    RecordFieldDef,
    /// `{x: i32, y: u32}`
    RecordFieldDefList,

    VariantDef,
    VariantDefList,

    /// `T`
    /// `T: Trait`
    TypeGenericParam,
    /// `const N: usize`
    ConstGenericParam,
    /// `<T: Trait, U, const N: usize>`
    GenericParamList,

    /// `(x: i32, _ y: mut i32)`
    FuncParamList,

    /// `_ x: mut i32`
    FnParam,

    /// `foo::Trait1 + Trait2`
    TypeBoundList,
    /// `TraitBound` or `TypeKind`.
    TypeBound,
    /// `Trait1`
    TraitBound,
    /// `* -> *` or `(*-> *) -> *`
    KindBoundAbs,
    /// `*`.
    KindBoundMono,
    /// `where Option<T>: Trait1 + Trait2`
    WhereClause,
    /// `Option<T>: Trait1 + Trait2`
    WherePredicate,

    /// Root node of the input source.
    Root,

    /// Represents an error branch.
    Error,
}

impl SyntaxKind {
    /// Returns `true` if this is a trivia token.
    pub fn is_trivia(self) -> bool {
        matches!(self, SyntaxKind::WhiteSpace | SyntaxKind::Comment)
    }

    /// Returns `true` if the token is a literal leaf.
    pub fn is_literal_leaf(self) -> bool {
        matches!(
            self,
            SyntaxKind::Int | SyntaxKind::String | SyntaxKind::TrueKw | SyntaxKind::FalseKw
        )
    }

    pub fn is_open_bracket_kind(self) -> bool {
        matches!(
            self,
            SyntaxKind::LBrace | SyntaxKind::LParen | SyntaxKind::LBracket | SyntaxKind::Lt
        )
    }

    /// Returns its corresponding open bracket kind if it is a close bracket
    /// kind.
    pub fn corresponding_open_bracket_kind(self) -> Option<Self> {
        match self {
            SyntaxKind::RBrace => Some(SyntaxKind::LBrace),
            SyntaxKind::RParen => Some(SyntaxKind::LParen),
            SyntaxKind::RBracket => Some(SyntaxKind::LBracket),
            SyntaxKind::Gt => Some(SyntaxKind::Lt),
            _ => None,
        }
    }

    pub(crate) fn is_modifier_head(self) -> bool {
        matches!(self, SyntaxKind::PubKw | SyntaxKind::UnsafeKw)
    }

    pub(crate) fn is_item_head(self) -> bool {
        self.is_modifier_head()
            || matches!(
                self,
                SyntaxKind::ModKw
                    | SyntaxKind::FnKw
                    | SyntaxKind::StructKw
                    | SyntaxKind::ContractKw
                    | SyntaxKind::EnumKw
                    | SyntaxKind::TypeKw
                    | SyntaxKind::ImplKw
                    | SyntaxKind::TraitKw
                    | SyntaxKind::ConstKw
                    | SyntaxKind::UseKw
                    | SyntaxKind::ExternKw
            )
    }
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}
