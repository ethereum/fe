//! This module contains the definition of the `SyntaxKind`.

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
    /// `1` or `0b1010` or `0o77` or `0xff`
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
    UnderScore,
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
    /// `/`
    #[token("/")]
    Slash,
    /// `%`
    #[token("%")]
    Percent,
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
    /// `<<`
    #[token("<<")]
    Lt2,
    /// `<=`
    #[token("<=")]
    LtEq,
    /// `<<=`
    #[token("<<=")]
    Lt2Eq,
    /// `>`
    #[token(">")]
    Gt,
    /// `>>`
    #[token(">>")]
    Gt2,
    /// `>=`
    #[token(">=")]
    GtEq,
    /// `>>=`
    #[token(">>=")]
    Gt2Eq,
    /// `=`
    #[token("=")]
    Eq,
    /// `==`
    #[token("==")]
    Eq2,
    /// `!=`
    #[token("!=")]
    NonEq,

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
    /// `contract Foo { .. }`
    ContractDef,
    /// `enum Foo { .. }`
    Enum,
    /// `type Foo = i32`
    TypeAlias,
    /// `impl Foo { .. }`
    Impl,
    /// `trait Foo { .. }`
    Trait,
    /// `impl Trait for Foo { .. }`
    TraitImpl,
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
    /// `(key1: value1, key2: value2)`
    AttrParamList,
    /// `key: value`
    AttrParam,
    /// `/// Comment`
    DocCommentAttr,
    AttrList,

    /// `pub`
    Visibility,

    /// `x: i32`
    StructFieldDef,
    StructFieldDefList,

    /// `(i32, u32)`
    TupleDef,

    VariantDef,
    VariantDefList,

    /// `T`
    /// `T: Trait`
    TypeBound,
    /// `<T: Trait, U>`
    GenericParamList,

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
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}
