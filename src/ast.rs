use std::fmt;

pub trait AST {
    fn const_fold(&self) -> Self where Self: Sized+Clone {
        self.clone()
    }
}


pub type Program = Vec<Declaration>;

impl AST for Program {
    fn const_fold(&self) -> Program {
        self.iter().map(|ref x| x.const_fold()).collect()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration {
    Func(String, Type, Vec<(String, Type)>, Vec<Statement>),
    Var(String, Type),
}

impl AST for Declaration {
    fn const_fold(&self) -> Declaration {
        match self {
            &Declaration::Func(ref n, ref t, ref args, ref body) =>
                Declaration::Func(n.to_string(), t.clone(), args.clone(),
                                  body.iter().map(|ref x| x.const_fold()).collect()),
            _ => self.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eql, NotEql,
    Lt, Lte,
    Gt, Gte,
    Or, And,
    BitOr, BitAnd, BitXor,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LValue {
    Identifier(String),
    ArrayItem(Box<LValue>, Expression),
}

impl LValue {
    pub fn name(&self) -> &String {
        match self {
            &LValue::Identifier(ref n) => n,
            &LValue::ArrayItem(ref l, _) => l.name(),
        }
    }

    pub fn dim(&self) -> usize {
        match self {
            &LValue::Identifier(ref n) => 0,
            &LValue::ArrayItem(ref l, _) => 1 + l.dim(),
        }
    }

    pub fn with_type(&self, typ: &Type) -> Type {
        match self {
            &LValue::ArrayItem(ref l, _) => typ.inner(),
            _ => typ.clone(),
        }
    }
}

impl AST for LValue {
    fn const_fold(&self) -> LValue {
        match self {
            &LValue::ArrayItem(ref l, ref expr) =>
                LValue::ArrayItem(l.clone(), expr.const_fold()),
            _ => self.clone(),
        }
    }
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

impl AST for Statement {
    fn const_fold(&self) -> Statement {
        match self {
            &Statement::RValue(ref e) => Statement::RValue(e.const_fold()),
            &Statement::Condition(ref cond, ref cons, ref alt) =>
                Statement::Condition(cond.const_fold(),
                    cons.iter().map(|ref x| x.const_fold()).collect(),
                    alt.iter().map(|ref x| x.const_fold()).collect()),
            &Statement::Loop(ref cond, ref body) =>
                Statement::Loop(cond.const_fold(),
                    body.iter().map(|ref x| x.const_fold()).collect()),
            &Statement::Assign(ref l, ref e) =>
                Statement::Assign(l.const_fold(), e.const_fold()),
            &Statement::Return(ref r) => Statement::Return(r.const_fold()),
            _ => self.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Lit(i32),                         // Litteral value
    CharLit(char),                    // Litteral char
    LValue(Box<LValue>),              // variable (optional indexing)
    Funcall(String, Vec<Expression>), // f(args)
    ArrayLen(Box<LValue>),            // length array

    InfixOp(Operator, Box<Expression>, Box<Expression>),        // left op right
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>), // a ? b : c
}

impl AST for Expression {
    // Constant folding implementation at the AST level
    fn const_fold(&self) -> Expression {
        match self {
            &Expression::CharLit(x) => Expression::Lit(x as i32),
            &Expression::InfixOp(ref op, ref l, ref r) => {
                let lf = l.const_fold();
                let rf = r.const_fold();
                if let (&Expression::Lit(ref lfl), &Expression::Lit(ref rfl)) = (&lf, &rf) {
                    match op {
                        &Operator::Add => return Expression::Lit(lfl + rfl),
                        &Operator::Sub => return Expression::Lit(lfl - rfl),
                        &Operator::Mul => return Expression::Lit(lfl * rfl),
                        &Operator::Div => return Expression::Lit(lfl / rfl),
                        _ => {}
                    }
                }
                return Expression::InfixOp(op.clone(), Box::new(lf), Box::new(rf));
            },
            &Expression::Funcall(ref s, ref exprs) =>
                Expression::Funcall(s.to_string(), exprs.iter()
                                                        .map(|ref x| x.const_fold())
                                                        .collect()),
            _ => self.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Char,
    ArrayOf(Box<Type>, usize)
}

impl Type {
    pub fn size(&self) -> usize {
        match *self {
            Type::Char => 1,
            Type::Int  => 4,
            Type::ArrayOf(ref t, n) => n * t.size(),
        }
    }

    pub fn length(&self) -> usize {
        match self {
            &Type::ArrayOf(_, n) => n,
            _ => 0,
        }
    }

    pub fn base(&self) -> Type {
        match *self {
            Type::ArrayOf(ref t, _) => t.base(),
            _ => self.clone(),
        }
    }

    pub fn inner(&self) -> Type {
        match self {
            &Type::ArrayOf(ref t, _) => (**t).clone(),
            _ => panic!("Not a wrapped type")
        }
    }

    pub fn shape(&self) -> Vec<usize> {
        match self {
            &Type::ArrayOf(ref l, n) => {
                let mut res = vec![n];
                res.append(&mut l.shape());
                res
            },
            _ => vec![],
        }
    }

    pub fn accepts(&self, other: &Type) -> bool {
        match (self, other) {
            // Two arrays, check inner type
            (&Type::ArrayOf(ref l, _), &Type::ArrayOf(ref r, _)) => l.accepts(&*r),
            // Array and non-array: NO
            (&Type::ArrayOf(_, _), _) => false,
            // non-array and array: NO
            (_, &Type::ArrayOf(_, _)) => false,
            _ => true,
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
            &Operator::And => write!(f, "&&"),
            &Operator::Or => write!(f, "||"),
            &Operator::BitAnd => write!(f, "&"),
            &Operator::BitOr => write!(f, "|"),
            &Operator::BitXor => write!(f, "^"),
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
            Expression::CharLit(x) => write!(f, "'{}'", x),
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
