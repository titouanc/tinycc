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
    Assign(LValue, Expression),
    Return(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Lit(i32), // Litteral value
    LValue(Box<LValue>), // variable (optional indexing)
    Funcall(String, Vec<Expression>), // f(args)
    ArrayLen(Box<LValue>), // length array

    UnaryOp(Operator, Box<Expression>),
    InfixOp(Operator, Box<Expression>, Box<Expression>), // left op right
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>), // a ? b : c
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Char,
    ArrayOf(Box<Type>, usize)
}
