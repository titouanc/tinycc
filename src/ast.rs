use std::fmt;

pub type Program = Vec<Declaration>;

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Func(String, Type, Vec<(String, Type)>, Vec<Statement>),
    Var(String, Type),
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum LValue {
    Identifier(String),
    ArrayItem(Box<LValue>, Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    LocalDecl(String, Type),
    RValue(Expression),
    Condition(Expression, Vec<Statement>, Vec<Statement>),
    Loop(Expression, Vec<Statement>),
    Assign(LValue, Expression),
    Return(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Lit(i32), // Litteral value
    LValue(Box<LValue>), // variable (optional indexing)
    Funcall(String, Vec<Expression>), // f(args)
    ArrayLen(Box<LValue>), // length array

    InfixOp(Operator, Box<Expression>, Box<Expression>), // left op right
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>), // a ? b : c
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Char,
    ArrayOf(Box<Type>, usize)
}

impl Type {
    pub fn size(&self) -> usize {
        return match *self {
            Type::Char => 1,
            Type::Int  => 4,
            Type::ArrayOf(ref t, n) => n * t.size(),
        }
    }
}

////////////// Formatting //////////////
fn print_block(body: &Vec<Statement>, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{{").unwrap();
    for st in body {
        write!(f, "{}", st).unwrap();
    }
    write!(f, "}}")
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Declaration::Var(ref name, ref typ) => write!(f, "{} {}", typ, name),
            Declaration::Func(ref name, ref typ, ref args, ref body) => {
                write!(f, "{} {}(", typ, name).unwrap();
                for (i, &(ref n, ref t)) in args.iter().enumerate() {
                    if i > 0 {write!(f, ", ").unwrap();}
                    write!(f, "{} {}", t, n).unwrap();
                }
                write!(f, ")").unwrap();
                print_block(body, f)
            },
        }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Operator::Add => write!(f, "+"),
            &Operator::Sub => write!(f, "-"),
            &Operator::Mul => write!(f, "*"),
            &Operator::Div => write!(f, "/"),
            &Operator::Mod => write!(f, "%"),
            &Operator::Eql => write!(f, "=="),
            &Operator::NotEql => write!(f, "!="),
            &Operator::Lt => write!(f, "<"),
            &Operator::Lte => write!(f, "<="),
            &Operator::Gt => write!(f, ">"),
            &Operator::Gte => write!(f, ">="),
        }
    }
}

impl fmt::Display for LValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LValue::Identifier(ref name) => write!(f, "{}", name),
            LValue::ArrayItem(ref left, ref idx) => write!(f, "{}[{}]", *left, idx),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::LocalDecl(ref name, ref typ) => write!(f, "{} {};", typ, name),
            Statement::RValue(ref expr) => write!(f, "{};", expr),
            Statement::Condition(ref cond, ref cons, ref alt) => {
                write!(f, "if ({})", cond).unwrap();
                let res = print_block(cons, f);
                if alt.len() > 0 {
                    res.unwrap();
                    write!(f, " else ").unwrap();
                    print_block(alt, f)
                } else {
                    res
                }
            },
            Statement::Loop(ref cond, ref body) => {
                write!(f, "while ({})", cond).unwrap();
                print_block(body, f)
            },
            Statement::Assign(ref left, ref val) => write!(f, "{} = {};", left, val),
            Statement::Return(ref expr) => write!(f, "return {};", expr),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Lit(x) => write!(f, "{}", x),
            Expression::LValue(ref left) => write!(f, "{}", left),
            Expression::Funcall(ref name, ref args) => {
                write!(f, "{}(", name).unwrap();
                for (i, &ref arg) in args.iter().enumerate() {
                    if i > 0 {write!(f, ", ").unwrap();}
                    write!(f, "{}", arg).unwrap();
                }
                write!(f, ")")
            },
            Expression::ArrayLen(ref lval) => write!(f, "lengthOf({})", lval),

            Expression::InfixOp(ref op, ref l, ref r) => write!(f, "({} {} {})", l, op, r),
            Expression::Ternary(ref cond, ref cons, ref  alt) => write!(f, "({}) ? ({}) : ({})", cond, cons, alt),
        }
    }
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
