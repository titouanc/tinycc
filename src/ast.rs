use std::fmt;

fn print_block(body: &Vec<Statement>, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{{");
    for st in body {
        write!(f, "{}", st);
    }
    write!(f, "}}")
}

pub type Program = Vec<Declaration>;

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Func(String, Type, Vec<(String, Type)>, Vec<Statement>),
    Var(String, Type),
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
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

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Mul => write!(f, "*"),
            Div => write!(f, "+"),
            Mod => write!(f, "%"),
            Eql => write!(f, "=="),
            NotEql => write!(f, "!="),
            Lt => write!(f, "<"),
            Lte => write!(f, "<="),
            Gt => write!(f, ">"),
            Gte => write!(f, ">="),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum LValue {
    Identifier(String),
    ArrayItem(Box<LValue>, Expression),
}

impl fmt::Display for LValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LValue::Identifier(ref name) => write!(f, "{}", name),
            LValue::ArrayItem(ref left, ref idx) => write!(f, "{}[{}]", *left, idx),
        }
    }
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

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::LocalDecl(ref name, ref typ) => write!(f, "{} {};", typ, name),
            Statement::RValue(ref expr) => write!(f, "{};", expr),
            Statement::Condition(ref cond, ref cons, ref alt) => {
                write!(f, "if ({})", cond);
                print_block(cons, f);
                write!(f, " else ");
                print_block(alt, f)
            },
            Statement::Loop(ref cond, ref body) => {
                write!(f, "while ({})", cond);
                print_block(body, f)
            },
            Statement::Assign(ref left, ref val) => write!(f, "{} = {};", left, val),
            Statement::Return(ref expr) => write!(f, "return {};", expr),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Lit(i32), // Litteral value
    LValue(Box<LValue>), // variable (optional indexing)
    Funcall(String, Vec<Expression>), // f(args)
    ArrayLen(Box<LValue>), // length array

    InfixOp(Operator, Box<Expression>, Box<Expression>), // left op right
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>), // a ? b : c
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Lit(x) => write!(f, "{}", x),
            Expression::LValue(ref left) => write!(f, "{}", left),
            Expression::Funcall(ref name, ref args) => {
                write!(f, "{}(", name);
                for arg in args {
                    write!(f, "{}, ", arg);
                }
                write!(f, ")")
            },
            Expression::ArrayLen(ref lval) => write!(f, "lengthOf({})", lval),

            Expression::InfixOp(ref op, ref l, ref r) => write!(f, "({} {} {})", l, op, r),
            Expression::Ternary(ref cond, ref cons, ref  alt) => write!(f, "({}) ? ({}) : ({})", cond, cons, alt),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Char,
    ArrayOf(Box<Type>, usize)
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Int => write!(f, "int"),
            Type::Char => write!(f, "char"),
            Type::ArrayOf(ref typ, size) => write!(f, "{}[{}]", typ, size),
        }
    }
}
