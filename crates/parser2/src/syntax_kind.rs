//! This module contains the definition of the `SyntaxKind`.

/// The definition of the `SyntaxKind'.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(u16)]
pub enum SyntaxKind {
    // Atom kinds. These are leaf nodes.
    Error = 0,
    Newline,
    WhiteSpace,
    EOF,
    /// `foo`
    Ident,
    /// `1`
    Int,
    /// "MyString"
    String,

    /// `(`
    LParen,
    /// `)`
    RParen,
    /// `{`
    LBrace,
    /// `}`
    RBrace,
    /// `[`
    LBracket,
    /// `]`
    RBracket,
    /// `:`
    Colon,
    /// `::`
    Colon2,
    /// `;`
    SemiColon,
    /// `.`
    Dot,
    /// `..`
    Dot2,
    /// `,`
    Comma,
    /// `->`
    Arrow,
    /// `=>`
    FatArrow,
    /// `_`
    UnderScore,
    /// `#`
    Pound,
    /// `// Comment`
    Comment,

    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,
    /// `&`
    Amp,
    /// `&&`
    Amp2,
    /// `|`
    Pipe,
    /// `||`
    Pipe2,
    /// `<`
    Lt,
    /// `<<`
    Lt2,
    /// `<=`
    LtEq,
    /// `<<=`
    Lt2Eq,
    /// `<`
    Gt,
    /// `<<`
    Gt2,
    /// `<=`
    GtEq,
    /// `<<=`
    Gt2Eq,
    /// `=`
    Eq,
    /// `==`
    Eq2,
    /// `!=`
    NonEq,

    /// `true'
    TrueKw,
    /// `false`
    FalseKw,
    /// `assert`
    AssertKw,
    /// `break`
    BreakKw,
    /// `continue`
    ContinueKw,
    /// `contract`
    ContractKw,
    /// `fn`
    FnKw,
    /// `const`
    ConstKw,
    /// `if`
    IfKw,
    /// `else`
    ElseKw,
    /// `match`
    MatchKw,
    /// `for`
    ForKw,
    /// `while`
    WhileKw,
    /// `pub`
    PubKw,
    /// `return`
    ReturnKw,
    /// `revert`
    RevertKw,
    /// `self`
    SelfKw,
    /// `struct`
    StructKw,
    /// `enum`
    EnumKw,
    /// `trait`
    TraitKw,
    /// `impl`
    ImplKw,
    /// `type`
    TypeKw,
    /// `let`
    LetKw,
    /// `mut`
    MutKw,
    /// `use`
    UseKw,
    /// `extern`
    ExternKw,

    // Expressions. These are non-leaf nodes.
    /// `x + 1`
    BinExpr,
    /// `!x`
    UnExpr,
    /// `foo(x, y)`
    CallExpr,
    /// `(x, y)`
    CallArgList,
    /// `<i32, u256>`
    CallTypeArgList,
    /// `FOO::Bar`
    PathExpr,
    /// `foo.bar(x, y)`
    MethodCallExpr,
    /// `foo.bar`
    FieldExpr,
    /// `foo[1]`
    IndexExpr,
    /// `(x ,y)`
    TupleExpr,
    /// `[x; 1]`
    ArrayExpr,
    /// `1`
    LiteralExpr,
    /// `if x { 1 } else { 2 }`
    IfExpr,
    /// `match x { pat => { .. } }`
    MatchExpr,

    // Statements. These are non-leaf nodes.
    /// `let x = 1`
    LetStmt,
    /// `for x in y {..}`
    ForStmt,
    /// `assert x == 2`
    AssertStmt,
    /// `return 1`
    ReturnStmt,
    /// `1`
    ExprStmt,
    StmtList,

    // Patterns. These are non-leaf nodes.
    /// `_`
    WildCardPat,
    /// `..`
    RestPat,
    /// `x`
    LiteralPat,
    /// `(x, y)`
    TuplePat,
    /// `Enum::Variant`
    PathPat,
    /// `Enum::Variant(x, y)`
    PathTuplePat,

    // MatchArms.
    // `pat => { stmtlist }`
    MatchArm,
    MatchArmList,

    // Items. These are non-leaf nodes.
    /// `fn foo(x: i32) -> i32 { .. }`
    Fn,
    /// `struct Foo { .. }`
    Struct,
    /// `x: i32`
    FieldDef,
    FieldDefList,
    /// `contract Foo { .. }`
    ContractDef,
    /// `(i32, u32)`
    Tuple,
    /// `enum Foo { .. }`
    Enum,
    VariantDef,
    VariantDefList,
    /// `type Foo = i32`
    TypeAlias,
    /// `impl Foo { .. }`
    Impl,
    /// `trait Foo { .. }`
    Trait,
    /// `impl Trait for Foo { .. }`
    TraitImpl,
    /// `T`
    /// `T: Trait`
    TypeBound,
    /// `<T: Trait, U>`
    GenericParamList,
    /// `const FOO: i32 = 1`
    Const,
    /// `use foo::bar`
    Use,
    /// `extern { .. }`
    Extern,
    ItemList,

    // Paths. These are non-leaf nodes.
    /// `Segment1::Segment2`
    Path,
    /// `Segment1`
    PathSegment,

    /// `#attr`
    Attr,
    AttrList,

    /// `pub`
    Visibility,
}
