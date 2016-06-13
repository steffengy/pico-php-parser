use std::borrow::Cow;

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    // arith
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    // logical
    Or,
    And,
    // unary
    Not,
    // pre/post
    PreInc,
    PreDec,
    PostInc,
    PostDec,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Visibility {
    Public,
    Private,
    Protected
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    None,
    True,
    False,
    Null,
    Identifier(Cow<'a, str>),
    String(String),
    Int(i64),
    Variable(Cow<'a, str>),
    Echo(Box<Expr<'a>>),
    Return(Box<Expr<'a>>),

    ArrayIdx(Box<Expr<'a>>, Vec<Expr<'a>>),
    ObjProperty(Box<Expr<'a>>, Vec<Expr<'a>>),
    StaticProperty(Box<Expr<'a>>, Vec<Expr<'a>>),
    Call(Box<Expr<'a>>, Vec<Expr<'a>>),
    UnaryOp(Op, Box<Expr<'a>>),
    BinaryOp(Op, Box<Expr<'a>>, Box<Expr<'a>>),
    Function(FunctionDecl<'a>),
    // statements
    Assign(Box<Expr<'a>>, Box<Expr<'a>>),
    /// If (condition.0) { Block.1 } else Else_Expr.2
    If(Box<Expr<'a>>, Vec<Expr<'a>>, Option<Vec<Expr<'a>>>),
    While(Box<Expr<'a>>, Vec<Expr<'a>>),
    DoWhile(Vec<Expr<'a>>, Box<Expr<'a>>),
    ForEach(Box<Expr<'a>>, Option<Cow<'a, str>>, Option<Cow<'a, str>>, Vec<Expr<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDecl<'a> {
    pub params: Vec<Expr<'a>>,
    pub body: Vec<Expr<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl<'a> {
    /// A set of statements
    Block(Vec<Expr<'a>>),
    GlobalFunction(Cow<'a, str>, FunctionDecl<'a>)
}
