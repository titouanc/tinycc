/* ITL: Intermediate Tiny Language */

use std::fmt;
use std::collections::HashMap;
use ::scope::{Variable, Scope};
use ::ast;
use ::ast::Type;

type Op = ast::Operator;

#[derive(Debug,Clone,PartialEq)]
pub enum OpCode {
    NOP,
    BinOp(String, Op, String, String), // Var = Var Op Var
    Immediate(String, i32),            // Var = immediate value
    Assign(String, String),            // Var = Var
    Goto(usize),                       // Goto <index>
    If(String, usize),                 // If Var otherwise jump <index>
    Call(String, String, Vec<String>), // Var = Func(Vars...)
    Load(String, String, String),      // Var = Var[Var]
    Store(String, String, String),     // Var[Var] = Var
    Return(String),                    // return Var
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::OpCode::*;

        match *self {
            NOP => write!(f, "NOP"),
            BinOp(ref name, ref op, ref l, ref r) =>
                write!(f, "{} = {} {} {}", name, l, op, r),
            Immediate(ref name, ref val) =>
                write!(f, "{} = {}", name, val),
            Call(ref name, ref func, ref args) =>
                write!(f, "{} = {}({:?})", name, func, args),
            Load(ref l, ref r, ref off) =>
                write!(f, "{} = {}[{}]", l, r, off),
            Store(ref l, ref off, ref r) =>
                write!(f, "{}[{}] = {}", l, off, r),
            Assign(ref l, ref r) =>
                write!(f, "{} = {}", l, r),
            If(ref cond, ref jmp) =>
                write!(f, "IF {} OR JMP {}", cond, jmp),
            Return(ref v) =>
                write!(f, "RET {}", v),
            _ => write!(f, "<OpCode>"),
        }
    }
}

impl OpCode {
    pub fn reloc_code(&self, offset: usize) -> OpCode {
        use self::OpCode::*;
        match self {
            &Goto(ref i) => Goto(i + offset),
            &If(ref cond, ref i) => If(cond.to_string(), i + offset),
            other => other.clone(),
        }
    }
}

#[derive(Debug,Clone,PartialEq)]
pub struct Block {
    inputs: HashMap<String, Type>,
    locals: HashMap<String, Type>,
    renames: HashMap<String, String>,
    code: Vec<OpCode>,
}

impl Block {
    pub fn fresh_name(&mut self) -> String {
        let res = format!("${}", self.locals.len() + self.inputs.len());
        self.locals.insert(res.to_string(), Type::Int);
        res
    }

    pub fn latest_name(&self, name: &String) -> String {
        if self.locals.contains_key(name) || self.inputs.contains_key(name) {
            if let Some(rename) = self.renames.get(name) {
                self.latest_name(rename)
            } else {
                name.to_string()
            }
        } else {
            panic!(format!("Unable to find latest name for `{}`", name))
        }
    }

    fn sub(&self, body: &Vec<ast::Statement>) -> Block {
        let mut res = Block {
            inputs: self.locals.clone(),
            locals: HashMap::new(),
            renames: HashMap::new(),
            code: vec![],
        };

        for (n, t) in self.inputs.iter() {
            res.inputs.insert(n.to_string(), t.clone());
        }

        for st in body.iter() {
            res.internalize_statement(st);
        }
        return res;
    }

    fn outputs(&self) -> HashMap<String, String> {
        let mut res = HashMap::new();
        for k in self.inputs.keys() {
            res.insert(k.to_string(), self.latest_name(k));
        }
        return res;
    }

    fn sum_locals(&mut self, x: &String, y: &String) -> String {
        use ast::Operator::*;

        let left = self.fresh_name();
        let op = OpCode::BinOp(left.to_string(), Add,
                               x.to_string(), y.to_string());
        self.code.push(op);
        left
    }

    fn array_offset(&mut self, arr: &ast::LValue, expr: &ast::Expression) -> (String, String) {
        use ast::LValue::*;

        let this_offset = self.internalize_expression(expr);

        match arr {
            &Identifier(ref name) => (name.to_string(), this_offset),
            &ArrayItem(ref l, ref e) => {
                let (name, offset) = self.array_offset(l, e);
                (name, self.sum_locals(&offset, &this_offset))
            }
        }
    }

    fn internalize_expression(&mut self, expr: &ast::Expression) -> String {
        use ast::Expression::*;
        use self::OpCode::*;
        
        let left = self.fresh_name();
        let op = match expr {
            &Lit(ref x) => Immediate(left.to_string(), *x),
            &CharLit(ref x) => Immediate(left.to_string(), *x as i32),
            &LValue(ref lval) => {
                match **lval {
                    // Resolve an identifier: just return its latest name and
                    // do nothing
                    ast::LValue::Identifier(ref name) => {
                        return self.latest_name(name);
                    },
                    ast::LValue::ArrayItem(ref l, ref e) => {
                        let (name, offset) = self.array_offset(l, e);
                        Load(left.to_string(), name, offset)
                    }
                }
            },
            &InfixOp(ref op, ref l_expr, ref r_expr) => {
                let l = self.internalize_expression(l_expr);
                let r = self.internalize_expression(r_expr);
                BinOp(left.to_string(), op.clone(), l, r)
            },
            &Funcall(ref name, ref exprs) => {
                let mut args = vec![];
                for e in exprs.iter() {
                    args.push(self.internalize_expression(e));
                }
                Call(left.to_string(), name.to_string(), args)
            },
            _ => NOP
        };
        self.code.push(op);

        left
    }

    fn internalize_assign(&mut self, lval: &ast::LValue, rval: String) {
        use ast::LValue::*;

        match lval {
            &Identifier(ref name) => {
                let actual_name = self.latest_name(name);
                let new_name = self.fresh_name();
                self.renames.insert(actual_name.to_string(), new_name.to_string());
                let op = OpCode::Assign(new_name.to_string(), rval);
                self.code.push(op);
            },
            &ArrayItem(ref l, ref expr) => {
                let (name, offset) = self.array_offset(l, expr);
                let op = OpCode::Store(name, offset, rval);
                self.code.push(op);
            }
        }
    }

    fn internalize_statement(&mut self, st: &ast::Statement) {
        use ast::Statement::*;

        match st {
            &LocalDecl(ref name, ref typ) => {
                self.locals.insert(name.to_string(), typ.clone());
            },
            &RValue(ref expr) => {
                self.internalize_expression(expr);
            },
            &Assign(ref lval, ref expr) => {
                let rval = self.internalize_expression(expr);
                self.internalize_assign(lval, rval);
            },
            &Condition(ref expr, ref cons, ref alt) => {
                let cond = self.internalize_expression(expr);
                let mut block_cons = self.sub(cons);
                for (old, new) in block_cons.outputs() {
                    self.renames.insert(old.to_string(), new.to_string());
                }

                let dest_offset = 1 + self.code.len() + block_cons.code.len();
                self.code.push(OpCode::If(cond, dest_offset));
                self.code.append(&mut block_cons.code);
            },
            &Loop(ref cond, ref body) => {

            },
            &Return(ref expr) => {
                let r = self.internalize_expression(expr);
                self.code.push(OpCode::Return(r));
            },
            _ => {}
        }
    }

    pub fn internalize(args: &Vec<(String,Type)>, body: &Vec<ast::Statement>) -> Block {
        let mut res = Block {
            inputs: HashMap::new(),
            locals: HashMap::new(),
            renames: HashMap::new(),
            code: vec![]
        };

        for &(ref name, ref typ) in args.iter() {
            res.inputs.insert(name.to_string(), typ.clone());
        }
        for st in body.iter() {
            res.internalize_statement(st);
        }

        return res;
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({:?})", self.inputs);
        let mut i = 0;
        for op in self.code.iter() {
            write!(f, "\n  \x1b[33m{:3}\x1b[0m {}", i, op);
            i = i+1;
        }
        Ok(())
    }
}

pub struct Program {
    globals: HashMap<String, Type>,
    functions: HashMap<String, Block>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\x1b[1mGLOBALS\x1b[0m {:?}\n", self.globals);
        for (name, block) in self.functions.iter() {
            write!(f, "  \x1b[4m{}\x1b[0m: {}\n", name, block);
        }
        Ok(())
    }
}

impl Program {
    fn internalize_declaration(&mut self, decl: &ast::Declaration) {
        use ast::Declaration::*;

        match decl {
            &Var(ref name, ref typ) => {
                self.globals.insert(name.to_string(), typ.clone());
            },
            &Func(ref name, ref typ, ref args, ref body) => {
                let block = Block::internalize(args, body);
                self.functions.insert(name.to_string(), block);
            },
        }
    }

    pub fn internalize(prog: &ast::Program) -> Program {
        let mut res = Program {
            globals: HashMap::new(),
            functions: HashMap::new()
        };

        for decl in prog.iter() {
            res.internalize_declaration(decl);
        }

        return res;
    }
}
