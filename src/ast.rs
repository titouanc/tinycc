pub type Program = Vec<Declaration>;

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Func(String, Type, Vec<(String, Type)>, Vec<Statement>),
    Var(String, Type),
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eql, NotEql,
    Lt, Lte,
    Gt, Gte,
}

#[derive(Debug, PartialEq)]
pub enum LValue {
    Identifier(String),
    ArrayItem(Box<LValue>, Expression),
}


#[derive(Debug, PartialEq)]
pub enum Statement {
    LocalDecl(String, Type),
    RValue(Expression),
    Condition(Expression, Vec<Statement>, Vec<Statement>),
    Loop(Expression, Vec<Statement>),
    Assign(LValue, Box<Expression>),
    Return(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Lit(i32),
    LValue(Box<LValue>),
    InfixOp(Operator, Box<Expression>, Box<Expression>),
    Funcall(String, Vec<Expression>),
    ArrayLen(Box<LValue>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Char,
    ArrayOf(Box<Type>, usize)
}
