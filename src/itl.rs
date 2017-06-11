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
    BinOp(String, Op, String, String),   // Var = Var Op Var
    Mad(String, String, usize, String), // Var = Var * Const + Var
    Immediate(String, i32),              // Var = immediate value
    Assign(String, String),              // Var = Var
    Goto(usize),                         // Goto <index>
    If(String, usize),                   // If Var otherwise jump <index>
    Call(String, String, Vec<String>),   // Var = Func(Vars...)
    Load(String, String, String),        // Var = Var[Var]
    Store(String, String, String),       // Var[Var] = Var
    Return(String),                      // return Var
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::OpCode::*;

        match *self {
            NOP => write!(f, "NOP"),
            BinOp(ref name, ref op, ref l, ref r) =>
                write!(f, "{} = ({} {} {})", name, l, op, r),
            Mad(ref name, ref a, ref b, ref c) =>
                write!(f, "{} = ({} * {} + {})", name, a, b, c),
            Immediate(ref name, ref val) =>
                write!(f, "{} = {}", name, val),
            Call(ref name, ref func, ref args) =>
                write!(f, "{} = \x1b[36;1mCALL\x1b[0m {} {:?}", name, func, args),
            Load(ref l, ref r, ref off) =>
                write!(f, "{} = \x1b[32;1mLOAD\x1b[0m {} [+{}]", l, r, off),
            Store(ref l, ref off, ref r) =>
                write!(f, "\x1b[32;1mSTORE\x1b[0m {} [+{}] = {}", l, off, r),
            Assign(ref l, ref r) =>
                write!(f, "{} = {}", l, r),
            If(ref cond, ref loc) =>
                write!(f, "\x1b[34;1mIF\x1b[0m {} \x1b[34;1mOR GOTO\x1b[0m \x1b[33m{}\x1b[0m", cond, loc),
            Goto(ref loc) =>
                write!(f, "\x1b[34;1mGOTO\x1b[0m \x1b[33m{}\x1b[0m", loc),
            Return(ref v) =>
                write!(f, "\x1b[36;1mRET\x1b[0m {}", v),
            _ => write!(f, "<OpCode>"),
        }
    }
}

impl OpCode {
    pub fn reloc(&self, offset: usize) -> OpCode {
        use self::OpCode::*;
        match self {
            &Goto(ref i) => Goto(i + offset),
            &If(ref cond, ref i) => If(cond.to_string(), i + offset),
            other => other.clone(),
        }
    }
}

#[derive(Debug,Clone,PartialEq)]
pub enum StackOffset {
    Param(usize, Type),
    Local(usize, Type),
}

#[derive(Debug,Clone,PartialEq)]
pub struct Stack {
    variables: HashMap<String,StackOffset>
}

impl Stack {
    pub fn new(block: &Block) -> Stack {
        use self::StackOffset::*;

        let mut vars = HashMap::new();
        let mut local_offset = 0;
        for (name, typ) in block.locals.iter() {
            vars.insert(name.to_string(), Local(local_offset, typ.clone()));
            local_offset += typ.size();
        }

        let mut param_offset = 0;
        for name in block.inputs_order.iter() {
            if let Some(typ) = block.inputs.get(name){
                vars.insert(name.to_string(), Param(param_offset, typ.clone()));
                param_offset += 4;
            }
        }
        Stack {variables: vars}
    }

    pub fn stackframe_size(&self) -> usize {
        use self::StackOffset::*;

        let mut res = 0;
        for (k, v) in self.variables.iter() {
            if let &Local(_, ref typ) = v {
                res += typ.size();
            }
        }
        return res;
    }

    pub fn offset_of(&self, varname: &String) -> StackOffset {
        if let Some(res) = self.variables.get(varname) {
            return res.clone();
        }
        panic!("Variable {} not in stackframe !!!", varname);
    }
}

#[derive(Debug,Clone,PartialEq)]
pub struct Block {
    inputs_order: Vec<String>,
    inputs: HashMap<String, Type>,
    locals: HashMap<String, Type>,
    renames: HashMap<String, String>,
    pub code: Vec<OpCode>,
}

impl Block {
    pub fn fresh_name(&mut self) -> String {
        let res = format!("_$_{}", self.locals.len() + self.inputs.len());
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

    pub fn lookup_type(&self, lval: &ast::LValue) -> Type {
        match lval {
            &ast::LValue::Identifier(ref name) => {
                if let Some(t) = self.inputs.get(name) {
                    return t.clone();
                }

                if let Some(t) = self.locals.get(name) {
                    return t.clone();
                }
                panic!("Unable to find type of `{}`", name);
            },
            &ast::LValue::ArrayItem(ref l, _) => {
                self.lookup_type(l).inner()
            }
        }
    }

    pub fn get_stack(&self) -> Stack {
        Stack::new(self)
    }

    pub fn get_labels(&self) -> Vec<usize> {
        let mut res = vec![];
        for op in self.code.iter() {
            match op {
                &OpCode::Goto(ref pos) => {res.push(*pos);},
                &OpCode::If(_, ref pos) => {res.push(*pos);},
                _ => {}
            }
        }
        return res;
    }

    fn sub(&self, body: &Vec<ast::Statement>) -> Block {
        let mut res = Block {
            inputs: self.locals.clone(),
            inputs_order: vec![],
            locals: HashMap::new(),
            renames: HashMap::new(),
            code: vec![],
        };

        for (k, v) in self.inputs.iter(){
            res.inputs.insert(k.to_string(), v.clone());
            res.inputs_order.push(k.to_string());
        }

        for st in body.iter() {
            res.internalize_statement(st);
        }
        
        for (old, new) in res.outputs() {
            res.code.push(OpCode::Assign(self.latest_name(&old), new.to_string()));
        }

        return res;
    }

    fn outputs(&self) -> HashMap<String, String> {
        let mut res = HashMap::new();
        for k in self.inputs.keys() {
            let v = self.latest_name(k);
            if k != &v {
                res.insert(k.to_string(), v);
            }
        }
        return res;
    }

    fn internalize_addr(&mut self, base: &String, idx: &String, size: usize) -> String {
        use ast::Operator::*;

        let left = self.fresh_name();
        let op = OpCode::Mad(left.to_string(), idx.to_string(),
                             size as usize, base.to_string());
        self.code.push(op);
        left
    }

    fn array_offset(&mut self, arr: &ast::LValue, expr: &ast::Expression) -> String {
        use ast::LValue::*;

        let idx = self.internalize_expression(expr);
        let t = self.lookup_type(arr);

        match arr {
            &Identifier(ref name) => {
                let base = self.fresh_name();
                self.code.push(OpCode::Immediate(base.to_string(), 0));
                self.internalize_addr(&base, &idx, t.size())
            },
            &ArrayItem(ref l, ref e) => {
                let base = self.array_offset(l, e);
                let stride = t.size() / t.shape().first().unwrap();
                self.internalize_addr(&base, &idx, stride)
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
                        let offset = self.array_offset(l, e);
                        let name = l.name();
                        Load(left.to_string(), name.to_string(), offset)
                    }
                }
            },
            &InfixOp(ref op, ref l_expr, ref r_expr) => {
                let l = self.internalize_expression(l_expr);
                let r = self.internalize_expression(r_expr);
                BinOp(left.to_string(), op.clone(), l, r)
            },
            &Ternary(ref cond_expr, ref true_expr, ref false_expr) => {
                println!("\x1b[31;1mWAAAARNING: Ternary expression not internalized\x1b[0m");
                NOP
            },
            &Funcall(ref name, ref exprs) => {
                let mut args = vec![];
                for e in exprs.iter() {
                    args.push(self.internalize_expression(e));
                }
                Call(left.to_string(), name.to_string(), args)
            },
            &ArrayLen(ref lval) => {
                let t = self.lookup_type(lval);
                if let Some(s) = t.shape().last() {
                    Immediate(left.to_string(), *s as i32)
                } else {
                    panic!("Unplausible shape for {}", lval);
                }
            },
            &Ternary(_, _, _) => NOP,
        };
        self.code.push(op);

        left
    }

    fn internalize_assign(&mut self, lval: &ast::LValue, rval: String) {
        use ast::LValue::*;

        match lval {
            &Identifier(ref name) => {
                let actual_name = self.latest_name(name);
                // let new_name = self.fresh_name();
                // self.renames.insert(actual_name.to_string(), new_name.to_string());
                // let op = OpCode::Assign(new_name.to_string(), rval);
                // self.code.push(op);
                self.renames.insert(actual_name.to_string(), rval.to_string());

            },
            &ArrayItem(ref l, ref expr) => {
                let name = l.name();
                let offset = self.array_offset(l, expr);
                let op = OpCode::Store(name.to_string(), offset, rval);
                self.code.push(op);
            }
        }
    }

    fn internalize_condition(&mut self, expr: &ast::Expression,
                                        cons: &Vec<ast::Statement>,
                                         alt: &Vec<ast::Statement>)
    {
        let cond = self.internalize_expression(expr);

        let mut block_cons = self.sub(cons);
        self.locals.extend(block_cons.locals);

        let mut block_alt = self.sub(alt);
        self.locals.extend(block_alt.locals);

        let ctrlop = if block_alt.code.len() > 0 { 2 } else { 1 };
        let dest_offset = ctrlop + self.code.len() + block_cons.code.len();
        self.code.push(OpCode::If(cond, dest_offset));
        
        let after_if = self.code.len();
        self.code.extend(block_cons.code.iter().map(|ref x| x.reloc(after_if)));

        if block_alt.code.len() > 0 {
            self.code.push(OpCode::Goto(dest_offset + block_alt.code.len()));
        }

        let after_else = self.code.len();
        self.code.extend(block_alt.code.iter().map(|ref x| x.reloc(after_else)));
    }

    fn internalize_loop(&mut self, expr: &ast::Expression,
                                        body: &Vec<ast::Statement>)
    {
        let cond_offset = self.code.len();
        let cond = self.internalize_expression(expr);

        let mut block_body = self.sub(body);
        self.locals.extend(block_body.locals);

        let dest_offset = 2 + self.code.len() + block_body.code.len();
        self.code.push(OpCode::If(cond, dest_offset));

        let after_if = self.code.len();
        self.code.extend(block_body.code.iter().map(|ref x| x.reloc(after_if)));
        self.code.push(OpCode::Goto(cond_offset));
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
            &Condition(ref cond, ref cons, ref alt) => {
                self.internalize_condition(cond, cons, alt);
            },
            &Loop(ref cond, ref body) => {
                self.internalize_loop(cond, body);
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
            inputs_order: vec![],
            inputs: HashMap::new(),
            locals: HashMap::new(),
            renames: HashMap::new(),
            code: vec![]
        };

        for &(ref name, ref typ) in args.iter() {
            res.inputs.insert(name.to_string(), typ.clone());
            res.inputs_order.push(name.to_string());
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
    pub functions: HashMap<String, Block>,
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
