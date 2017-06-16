/* ITL: Intermediate Tiny Language */

use std::collections::HashMap;
use std::fmt;

use ::ast;
use ::ast::Type;
use ::ast::Operator;

#[derive(Debug,PartialEq,Clone)]
pub enum Direct {
    Immediate(i32),
    Variable(String),
}

impl fmt::Display for Direct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Direct::*;

        match *self {
            Immediate(ref val) => write!(f, "{}", val),
            Variable(ref name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug,PartialEq,Clone)]
pub enum RVal {
    Immediate(i32),
    Variable(String),
    Indirect(String, Direct),
}

impl RVal {
    pub fn to_direct(&self) -> Direct {
        match self {
            &RVal::Immediate(n) => Direct::Immediate(n),
            &RVal::Variable(ref n) => Direct::Variable(n.to_string()),
            _ => panic!("Unable to convert RVal `{:?}` to Direct"),
        }
    }
}

impl fmt::Display for RVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::RVal::*;

        match *self {
            Immediate(ref val) => write!(f, "{}", val),
            Variable(ref name) => write!(f, "{}", name),
            Indirect(ref name, ref off) => write!(f, "{}[{}]", name, off),
        }
    }
}

#[derive(Debug,PartialEq,Clone)]
pub enum LVal {
    Discard,
    Variable(String),
    Indirect(String, Direct),
}

impl fmt::Display for LVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::LVal::*;

        match *self {
            Discard => write!(f, ""),
            _ => write!(f, "{} = ", self.to_rval()),
        }
    }
}

impl LVal {
    pub fn to_rval(&self) -> RVal {
        match self {
            &LVal::Variable(ref name) =>
                RVal::Variable(name.to_string()),
            &LVal::Indirect(ref name, ref offset) =>
                RVal::Indirect(name.to_string(), offset.clone()),
            _ => panic!("Impossible to convert {:?} to an RValue", self),
        }
    }

    pub fn to_direct(&self) -> Direct {
        match self {
            &LVal::Variable(ref name) =>
                Direct::Variable(name.to_string()),
            _ => panic!("Impossible to convert {:?} to a Direct value", self),
        }
    }
}

#[derive(Debug,PartialEq,Clone)]
pub enum OpCode {
    NOP,
    Assign(LVal, RVal),
    BinOp(LVal, Operator, RVal, RVal),
    Goto(usize),
    If(RVal, usize),
    Call(LVal, String, Vec<RVal>),
    Return(RVal),
}

impl OpCode {
    pub fn reloc(&self, offset: usize) -> OpCode {
        use self::OpCode::*;

        match self {
            &Goto(ref jmp) => Goto(jmp + offset),
            &If(ref cond, ref jmp) => If(cond.clone(), jmp + offset),
            _ => self.clone(),
        }
    }
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::OpCode::*;

        match *self {
            NOP => write!(f, "\x1b[31mNOP\x1b[0m"),
            Assign(ref l, ref r) => write!(f, "{}{}", l, r),
            BinOp(ref dest, ref op, ref l, ref r) => write!(f, "{}{} {} {}", dest, l, op, r),
            Goto(ref pos) => write!(f, "\x1b[1;36mGOTO\x1b[0m \x1b[33m{}\x1b[0m", pos),
            If(ref cond, ref pos) => write!(f, "\x1b[1;36mIF\x1b[0m {} ELSE \x1b[1;36mGOTO\x1b[0m \x1b[33m{}\x1b[0m", cond, pos),
            Call(ref dest, ref name, ref args) => write!(f, "{}\x1b[1;32mCALL\x1b[0m {} {:?}", dest, name, args),
            Return(ref val) => write!(f, "\x1b[1;32mRETURN\x1b[0m {}", val),
        }
    }
}

#[derive(Debug,PartialEq,Clone,Copy)]
pub enum AddressSpace {Local, Param, Global}

#[derive(Debug,PartialEq,Clone)]
pub struct Block {
    params: Vec<(String,Type)>,
    frame: HashMap<String,(AddressSpace,Type)>,
    code: Vec<OpCode>,
    ret: Type,
    intermediate: usize,
    max_intermediate: usize,
}

impl Block {
    fn type_for_name(&self, name: &String) -> Type {
        if let Some(&(_, ref t)) = self.frame.get(name) {
            t.clone()
        } else {
            panic!("Variable `{}` not found", name);
        }
    }

    fn tmp_var(&mut self) -> LVal {
        let res = format!("$_{}", self.intermediate);
        self.intermediate += 1;
        self.max_intermediate = if self.intermediate > self.max_intermediate {
            self.frame.insert(res.to_string(), (AddressSpace::Local, Type::Int));
            self.intermediate
        } else {
            self.max_intermediate
        };
        LVal::Variable(res)
    }

    fn sub(&self) -> Block {
        Block {
            params: self.params.clone(),
            frame: self.frame.clone(),
            code: vec![],
            ret: Type::Int,
            intermediate: self.intermediate,
            max_intermediate: self.max_intermediate
        }
    }

    fn multiply(&mut self, rval: &RVal, cons: i32) -> Direct {
        use self::RVal::*;

        match rval {
            &Immediate(ref n) => Direct::Immediate(n * cons),
            _ => {
                let dest = self.tmp_var();
                self.code.push(OpCode::BinOp(dest.clone(), Operator::Mul,
                                             rval.clone(), Immediate(cons)));
                dest.to_direct()
            }
        }
    }

    fn add(&mut self, rval: &RVal, cons: i32) -> Direct {
        use self::RVal::*;

        match rval {
            &Immediate(ref n) => Direct::Immediate(n + cons),
            _ => {
                let dest = self.tmp_var();
                self.code.push(OpCode::BinOp(dest.clone(), Operator::Add,
                                             rval.clone(), Immediate(cons)));
                dest.to_direct()
            }
        }
    }

    fn internalize_lvalue(&mut self, lval: &ast::LValue) -> LVal {
        use self::LVal::*;

        match lval {
            &ast::LValue::Identifier(ref name) => Variable(name.to_string()),
            &ast::LValue::ArrayItem(ref l, ref expr) => {
                let off = self.internalize_expression(expr);
                let t = self.type_for_name(l.name()).inner();
                let stride = t.size() / t.base().size();
                let offset = self.multiply(&off, stride as i32);
                Indirect(l.name().to_string(), offset)
            }
        }
    }

    fn internalize_expression(&mut self, expr: &ast::Expression) -> RVal {
        use self::RVal::*;
        use ast::Expression::*;

        match expr {
            &Lit(ref n) => {Immediate(*n)},
            &CharLit(ref n) => {Immediate(*n as i32)},
            &LValue(ref lval) => {self.internalize_lvalue(lval).to_rval()},
            &Funcall(ref name, ref args_expr) => {
                let args = args_expr.iter()
                                    .map(|ref arg| self.internalize_expression(arg))
                                    .collect();
                let dest = self.tmp_var();
                let op = OpCode::Call(dest.clone(), name.to_string(), args);
                self.code.push(op);
                dest.to_rval()
            },
            &ArrayLen(ref lval) => {
                let t = self.type_for_name(lval.name());
                let tt = lval.with_type(&t);
                Immediate(tt.length() as i32)
            },
            &InfixOp(ref op, ref left, ref right) => {
                let dest = self.tmp_var();
                let l = self.internalize_expression(left);
                let r = self.internalize_expression(right);
                let op = OpCode::BinOp(dest.clone(), *op, l, r);
                self.code.push(op);
                dest.to_rval()
            },
            &Ternary(ref cond, ref if_true, ref if_false) => {
                Immediate(42)
            }
        }
    }

    fn merge(&mut self, other: &Block) {
        let base = self.code.len();
        for instr in other.code.iter() {
            self.code.push(instr.reloc(base));
        }
        for (name, qual) in other.frame.iter() {
            if ! self.frame.contains_key(name) {
                self.frame.insert(name.to_string(), qual.clone());
            }
        }
    }

    fn internalize_condition(&mut self, if_cond: &ast::Expression,
                                        if_true: &Vec<ast::Statement>,
                                        if_false: &Vec<ast::Statement>)
    {
        let cond = self.internalize_expression(if_cond);
        let sub_true = self.sub()._internalize(if_true);
        let sub_false = self.sub()._internalize(if_false);
        let after_true_block = 2 + self.code.len() + sub_true.code.len();
        self.code.push(OpCode::If(cond, after_true_block));
        self.merge(& sub_true);
        let after_else_block = 1 + self.code.len() + sub_false.code.len();
        self.code.push(OpCode::Goto(after_else_block));
        self.merge(& sub_false);
    }

    fn internalize_loop(&mut self, loop_cond: &ast::Expression,
                                   body: &Vec<ast::Statement>)
    {
        let cond = self.internalize_expression(loop_cond);
        let sub_body = self.sub()._internalize(body);
        let before_cond = self.code.len();
        let after_body = 2 + before_cond + sub_body.code.len();
        self.code.push(OpCode::If(cond, after_body));
        self.merge(& sub_body);
        self.code.push(OpCode::Goto(before_cond));
    }

    fn internalize_statement(&mut self, st: &ast::Statement) {
        use ast::Statement::*;

        match st {
            &LocalDecl(ref name, ref typ) => {
                self.frame.insert(name.to_string(), (AddressSpace::Local, typ.clone()));
            },
            &RValue(ref expr) => {
                self.internalize_expression(expr);
            }
            &Condition(ref cond, ref if_true, ref if_false) => {
                self.internalize_condition(cond, if_true, if_false);
            }
            &Loop(ref cond, ref body) => {
                self.internalize_loop(cond, body);
            }
            &Assign(ref left, ref expr) => {
                let lval = self.internalize_lvalue(left);
                let rval = self.internalize_expression(expr);
                self.code.push(OpCode::Assign(lval, rval));
            }
            &Return(ref expr) => {
                let ret = self.internalize_expression(expr);
                self.code.push(OpCode::Return(ret));
            }
        }
    }

    fn _internalize(mut self, body: &Vec<ast::Statement>) -> Block {
        for st in body.iter() {
            self.intermediate = 0;
            self.internalize_statement(st);
        }
        self
    }

    pub fn internalize(ret: &Type, args: &Vec<(String, Type)>, body: &Vec<ast::Statement>) -> Block {
        let mut res = Block {
            params: args.clone(),
            code: vec![],
            ret: ret.clone(),
            frame: HashMap::new(),
            intermediate: 0,
            max_intermediate: 0
        };
        for &(ref name, ref typ) in args.iter() {
            res.frame.insert(name.to_string(), (AddressSpace::Param, typ.clone()));
        }
        res._internalize(body)
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(");
        for (i, &(ref name, ref typ)) in self.params.iter().enumerate() {
            if i > 0 { write!(f, ", "); }
            write!(f, "{}: {}", name, typ);
        }
        write!(f, "): {}\n", self.ret);
        for (i, opcode) in self.code.iter().enumerate() {
            write!(f, "  \x1b[33m{:3}\x1b[0m \x1b[30m|\x1b[0m {}\n", i, opcode);
        }
        write!(f, "\n")
    }
}


pub struct Program {
    functions: HashMap<String,Block>,
    globals: HashMap<String,Type>,
}

impl Program {
    pub fn internalize(prog: &ast::Program) -> Program {
        use ast::Declaration::*;

        let mut res = Program {functions: HashMap::new(), globals: HashMap::new()};

        for decl in prog.iter() {
            match decl {
                &Func(ref name, ref ret, ref args, ref body) => {
                    let blk = Block::internalize(ret, args, body);
                    res.functions.insert(name.to_string(), blk);
                },
                &Var(ref name, ref typ) => {
                    res.globals.insert(name.to_string(), typ.clone());
                }
            }
        }

        return res;
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (name, typ) in self.globals.iter() {
            write!(f, "GLOBAL {}: {}\n", name, typ);
        }
        for (name, blk) in self.functions.iter() {
            write!(f, "FUNC {} {}", name, blk);
        }
        write!(f, "\n")
    }
}
