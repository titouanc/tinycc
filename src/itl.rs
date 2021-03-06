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

impl Direct {
    pub fn to_rval(&self) -> RVal {
        match *self {
            Direct::Immediate(ref val) => RVal::Immediate(*val),
            Direct::Variable(ref val) => RVal::Variable(val.to_string()),
        }
    }
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

    pub fn lval(&self) -> LVal {
        use self::OpCode::*;
        match self {
            &Assign(ref l, _) => l.clone(),
            &BinOp(ref l, _, _, _) => l.clone(),
            &Call(ref l, _, _) => l.clone(),
            _ => LVal::Discard,
        }
    }

    pub fn with_lval(&self, lval: LVal) -> OpCode {
        use self::OpCode::*;
        match self {
            &Assign(_, ref x) => Assign(lval, x.clone()),
            &BinOp(_, ref op, ref l, ref r) =>
                BinOp(lval, op.clone(), l.clone(), r.clone()),
            &Call(_, ref name, ref args) =>
                Call(lval, name.to_string(), args.clone()),
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
            If(ref cond, ref pos) => write!(f, "\x1b[1;36mIF\x1b[0m {} \x1b[1;36mELSE GOTO\x1b[0m \x1b[33m{}\x1b[0m", cond, pos),
            Call(ref dest, ref name, ref args) => write!(f, "{}\x1b[1;32mCALL\x1b[0m {} {:?}", dest, name, args),
            Return(ref val) => write!(f, "\x1b[1;32mRETURN\x1b[0m {}", val),
        }
    }
}

#[derive(Debug,PartialEq,Clone,Copy)]
pub enum AddressSpace {Local, Param, Global}

#[derive(Debug,PartialEq,Clone)]
pub struct Block {
    pub params: Vec<(String,Type)>,
    pub frame: HashMap<String,(AddressSpace,Type)>,
    pub code: Vec<OpCode>,
    ret: Type,
    intermediate: usize,
    max_intermediate: usize,
}

impl Block {
    fn type_for_name(&self, name: &String) -> Type {
        self.lookup(name).1
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

    fn multiply(&mut self, r1: &RVal, r2: &RVal) -> Direct {
        use self::RVal::*;

        if &Immediate(0) == r1 || &Immediate(0) == r2 {
            return Direct::Immediate(0);
        }
        if &Immediate(1) == r1 {
            return r2.to_direct();
        }
        if &Immediate(1) == r2 {
            return r1.to_direct();
        }

        match (r1, r2) {
            (&Immediate(ref l), &Immediate(ref r)) => Direct::Immediate(l * r),
            _ => {
                let dest = self.tmp_var();
                self.code.push(OpCode::BinOp(dest.clone(), Operator::Mul,
                                             r1.clone(), r2.clone()));
                dest.to_direct()
            }
        }
    }

    fn add(&mut self, r1: &RVal, r2: &RVal) -> Direct {
        use self::RVal::*;

        if &Immediate(0) == r1 {
            return r2.to_direct();
        }
        if &Immediate(0) == r2 {
            return r1.to_direct();
        }

        match (r1, r2) {
            (&Immediate(ref l), &Immediate(ref r)) => Direct::Immediate(l + r),
            _ => {
                let dest = self.tmp_var();
                self.code.push(OpCode::BinOp(dest.clone(), Operator::Add,
                                             r1.clone(), r2.clone()));
                dest.to_direct()
            }
        }
    }

    fn internalize_array_offset(&mut self, lval: &ast::LValue, typ: &Type) -> Direct {
        match lval {
            &ast::LValue::Identifier(_) => Direct::Immediate(0),
            &ast::LValue::ArrayItem(ref l, ref expr) => {
                let t = typ.inner();
                let innerpart = self.internalize_array_offset(l, &t);
                let off = self.internalize_expression(expr);
                let stride = t.size() / t.base().size();
                let outerpart = self.multiply(&off, &RVal::Immediate(stride as i32));
                self.add(&innerpart.to_rval(), &outerpart.to_rval())
            }
        }
    }

    fn internalize_lvalue(&mut self, lval: &ast::LValue) -> LVal {
        use self::LVal::*;

        match lval {
            &ast::LValue::Identifier(ref name) => Variable(name.to_string()),
            &ast::LValue::ArrayItem(ref l, ref expr) => {
                let t = self.type_for_name(l.name());
                Indirect(l.name().to_string(), self.internalize_array_offset(lval, &t))
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
                if *op == Operator::And {
                    // Rewrite (a && b) as (a ? (b ? 1 : 0) : 0)
                    self.internalize_ternary(left, 
                                             &Ternary(Box::new((**right).clone()),
                                                     Box::new(Lit(1)),
                                                     Box::new(Lit(0))),
                                             &Lit(0))
                } else if *op == Operator::Or {
                    // Rewrite (a || b) as (a ? 1 : (b ? 1 : 0))
                    self.internalize_ternary(left,
                                             &Lit(1),
                                             &Ternary(Box::new((**right).clone()),
                                                     Box::new(Lit(1)),
                                                     Box::new(Lit(0))))
                } else {
                    let l = self.internalize_expression(left);
                    let r = self.internalize_expression(right);
                    let opcode = OpCode::BinOp(dest.clone(), *op, l, r);
                    self.code.push(opcode);
                    dest.to_rval()
                }
            },
            &Ternary(ref cond, ref if_true, ref if_false) => {
                self.internalize_ternary(cond, if_true, if_false)
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

    fn internalize_ternary(&mut self, if_cond: &ast::Expression,
                                      if_true: &ast::Expression,
                                      if_false: &ast::Expression) -> RVal
    {
        let res = self.tmp_var();
        let cond = self.internalize_expression(if_cond);
        
        let mut sub_true = self.sub();
        let expr_true = sub_true.internalize_expression(if_true);
        sub_true.code.push(OpCode::Assign(res.clone(), expr_true));

        let mut sub_false = self.sub();
        let expr_false = sub_false.internalize_expression(if_false);
        sub_false.code.push(OpCode::Assign(res.clone(), expr_false));
        
        let after_true = 2 + self.code.len() + sub_true.code.len();
        self.code.push(OpCode::If(cond, after_true));
        self.merge(&sub_true);

        let after_false = 1 + self.code.len() + sub_false.code.len();
        self.code.push(OpCode::Goto(after_false));
        self.merge(&sub_false);

        res.to_rval()
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
        let before_cond = self.code.len();
        let cond = self.internalize_expression(loop_cond);
        let after_cond = self.code.len();
        let sub_body = self.sub()._internalize(body);
        let after_body = 2 + after_cond + sub_body.code.len();
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

    pub fn internalize(ret: &Type, args: &Vec<(String, Type)>,
                       body: &Vec<ast::Statement>,
                       globals: &HashMap<String,Type>) -> Block
    {
        let mut res = Block {
            params: args.clone(),
            code: vec![],
            ret: ret.clone(),
            frame: HashMap::new(),
            intermediate: 0,
            max_intermediate: 0
        };
        for (name, typ) in globals.iter() {
            res.frame.insert(name.to_string(), (AddressSpace::Global, typ.clone()));
        }
        for &(ref name, ref typ) in args.iter() {
            res.frame.insert(name.to_string(), (AddressSpace::Param, typ.clone()));
        }
        res._internalize(body).simplify()
    }

    fn eliminate_dead_branchs(&mut self) {
        use self::RVal::*;
        use self::OpCode::*;

        for step in 0.. {
            let mut found = false;
            let mut dead_branch_range = 0..0;
            let mut condition_pos = 0;

            for (i, op) in self.code.iter().enumerate() {
                if let &If(Immediate(ref cond), ref jmp) = op {
                    // NOP constant condition
                    condition_pos = i;

                    // Determine dead branch start and end
                    dead_branch_range = if *cond == 0 {
                        i..*jmp
                    } else {
                        if let Goto(ref else_end) = self.code[*jmp - 1] {
                            *jmp..*else_end
                        } else {
                            panic!("Expected GOTO at end of branch");
                        }
                    };

                    found = true;
                    break;
                }
            }

            if ! found {
                break
            } else {
                self.code[condition_pos] = NOP;
                for i in dead_branch_range {
                    self.code[i] = NOP;
                }
                // self.code[dead_branch_range] = NOP;
            }
        }
    }

    fn eliminate_double_copy(&mut self) {
        use self::OpCode::*;

        let mut prev_op = NOP;
        let mut new_code = vec![];
        let len = self.code.len();

        for i in 0..len {
            let op = self.code[i].clone();
            if let Assign(ref l, ref r) = op {
                let prev_lval = prev_op.lval();
                if prev_lval != LVal::Discard && &prev_lval.to_rval() == r {
                    new_code.push(prev_op.with_lval(l.clone()));
                    prev_op = NOP;
                    continue;
                }
            }
            if i > 0 {
                new_code.push(prev_op);
            }
            prev_op = op;
        }

        if len > 0 {
            new_code.push(prev_op);
        }
        self.code = new_code;
    }

    fn eliminate_goto_next(&mut self) {
        use self::OpCode::*;
        let mut new_code = vec![];

        for (i, instr) in self.code.iter().enumerate() {
            if let &Goto(ref jmp) = instr {
                if *jmp == i + 1 {
                    new_code.push(NOP);
                    continue;
                }
            }
            new_code.push(instr.clone());
        }
        self.code = new_code;
    }

    pub fn simplify(&self) -> Block {
        let mut res = self.clone();
        res.eliminate_dead_branchs();
        res.eliminate_double_copy();
        res.eliminate_goto_next();
        return res;
    }

    pub fn lookup(&self, name: &String) -> (AddressSpace,Type) {
        if let Some(x) = self.frame.get(name) {
            x.clone()
        } else {
            panic!("Variable `{}` not found", name)
        }
    }

    pub fn get_labels(&self) -> Vec<usize> {
        let mut res = vec![];

        for op in self.code.iter() {
            match op {
                &OpCode::Goto(ref jmp) => {res.push(*jmp);}
                &OpCode::If(_, ref jmp) => {res.push(*jmp);}
                _ => {}
            }
        }

        return res;
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
            write!(f, "  \x1b[33m{:3}\x1b[0m  {}\n", i, opcode);
        }
        write!(f, "\n")
    }
}


pub struct Program {
    pub functions: HashMap<String,Block>,
    pub globals: HashMap<String,Type>,
}

impl Program {
    pub fn internalize(prog: &ast::Program) -> Program {
        use ast::Declaration::*;

        let mut res = Program {functions: HashMap::new(), globals: HashMap::new()};

        for decl in prog.iter() {
            match decl {
                &Func(ref name, ref ret, ref args, ref body) => {
                    let blk = Block::internalize(ret, args, body, &res.globals);
                    res.functions.insert(name.to_string(), blk.simplify());
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
