use lexer::Span;

pub type Program = Vec<Declaration>;

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Func(String, Type, Vec<(Type, String)>),
    Var(String, Type),
}

pub enum Statement {
    Rvalue(Box<Expr>),
    Condition(Box<Expr>, Box<Statement>, Box<Option<Statement>>),
    Loop(Box<Expr>, Box<Statement>),
    Assign(Box<LValue>, Box<Expr>)
}

pub enum LValue {
    Identifier(String),
    ArrayItem(Box<LValue>, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Lit(i64),
    Sum(Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Char,
    ArrayOf(Box<Type>)
}
