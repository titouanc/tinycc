pub type Program = Vec<Declaration>;

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Func(String, Type, Vec<(Type, String)>, Vec<Statement>),
    Var(String, Type),
}

#[derive(Debug, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Eql, NotEql,
    Lt, Lte,
    Gt, Gte,
}

#[derive(Debug, PartialEq)]
pub enum LValue {
    Identifier(String),
    ArrayItem(Box<LValue>, Box<Expr>),
}


#[derive(Debug, PartialEq)]
pub enum Statement {
    LocalDecl(String, Type),
    RValue(Box<Expr>),
    Condition(Box<Expr>, Vec<Statement>, Vec<Statement>),
    Loop(Box<Expr>, Vec<Statement>),
    Assign(Box<LValue>, Box<Expr>),
    Return(Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Lit(i32),
    LValue(Box<LValue>),
    InfixOp(Op, Box<Expr>, Box<Expr>),
    Funcall(String, Vec<Expr>),
    ArrayLen(Box<LValue>),
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Char,
    ArrayOf(Box<Type>, usize)
}
