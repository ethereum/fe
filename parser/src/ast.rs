pub type Name = String;
pub type Type = String;

#[derive(Debug, PartialEq)]
pub struct Module {
    pub body: Vec<ModuleStmt>,
}

#[derive(Debug, PartialEq)]
pub enum ModuleStmt {
    EventDef { name: Name, fields: Vec<EventField> },
    //InterfaceDef {
    //    name: Name,
    //    fields: Vec<InterfaceField>,
    //    methods: Vec<InterfaceMethodDef>,
    //},
    //ContractDef {
    //    name: Name,
    //    fields: Vec<ContractField>,
    //    methods: Vec<ContractMethodDef>,
    //},
}

#[derive(Debug, PartialEq)]
pub struct EventField {
    pub name: Name,
    pub typ: Type,
    //pub indexed: bool,
}

//pub struct InterfaceField {
//    pub name: Name,
//    pub typ: Type,
//}

//pub struct ContractField {
//    pub name: Name,
//    pub typ: Type,
//    pub public: bool,
//}

//pub struct InterfaceMethodDef {
//    pub name: Name,
//    pub props: Vec<MethodProps>,
//    pub args: Vec<Arg>,
//    pub return_type: Option<Type>,
//}

//pub struct ContractMethodDef {
//    pub name: Name,
//    pub props: Vec<MethodProps>,
//    pub args: Vec<Arg>,
//    pub return_type: Option<Type>,
//    pub body: Option<Stmt>,
//}

//pub struct Arg {
//    pub name: Name,
//    pub typ: Type,
//    pub default: Option<Expr>,
//}

//pub enum MethodProps {
//    Constant,
//    Modifying,
//    Public,
//    Private,
//}

//pub enum Stmt {
//    Return {
//        value: Option<Expr>,
//    }
//    Assign {
//        target: Vec<Expr>,
//        typ: Type,
//        value: Expr,
//    },
//    AugAssign {
//        target: Expr,
//        op: Operator,
//    },
//    EmitEvent {
//        event_name: Name,
//        args: Vec<Expr>,
//    },
//}

//pub enum Operator {
//    Add,
//    Sub,
//    Mult,
//    Div,
//    Mod,
//    Pow,
//    LShift,
//    RShift,
//    BitOr,
//    BitXor,
//    BitAnd,
//}

//pub enum Expr {
//    Name {
//        id: Name,
//    },
//}
