use ::itl::*;
use ::itl::OpCode::*;
use ::ast::Type;
use ::ast::Operator;
use std::collections::HashMap;

pub struct Assembler {
    code: Vec<String>,
    locals: HashMap<String,usize>,
    params: HashMap<String,usize>,
    types: HashMap<String,Type>,
    stacksize: usize,
}

impl Assembler {
    fn build_stackframe(&mut self, blk: &Block) {
        let mut param_off = 0;
        for &(ref name, ref typ) in blk.params.iter() {
            self.params.insert(name.to_string(), param_off);
            param_off += 4;
        }

        let mut local_off = 0;
        for (name, &(ref addr, ref typ)) in blk.frame.iter() {
            if addr == &AddressSpace::Local {
                self.locals.insert(name.to_string(), local_off);
                local_off += typ.size();
            }
            self.types.insert(name.to_string(), typ.clone());
        }
        self.stacksize = local_off;
    }

    fn lookup_variable(&self, name: &String) -> String {
        if let Some(x) = self.params.get(name) {
            format!("{}(%ebp)", x + 8)
        } else if let Some(x) = self.locals.get(name) {
            let off = (*x as i32) - (self.stacksize as i32);
            format!("{}(%ebp)", off)
        } else {
            name.to_string()
        }
    }

    fn lookup_indirect(&mut self, name: &String, off: &Direct) -> String {
        use itl::Direct::*;

        let var_ref = self.lookup_variable(name);
        let typ = self.types.get(name).unwrap();
        let size = typ.base().size() as i32;
        if self.params.contains_key(name){
            self.code.push(format!("movl {}, %esi", var_ref));
        } else {
            self.code.push(format!("leal {}, %esi", var_ref));
        }

        match off {
            &Immediate(ref idx) => format!("{}(%esi)", idx * size),
            &Variable(ref name) => {
                let idx = self.lookup_variable(name);
                self.code.push(format!("movl {}, %ecx", idx));
                format!("(%esi,%ecx,{})", size)
            }
        }
    }

    fn lookup_rval(&mut self, rval: &RVal, need_register: bool) -> String {
        use itl::RVal::*;

        let res = match rval {
            &Immediate(ref x) => format!("${}", x),
            &Indirect(ref name, ref off) => self.lookup_indirect(name, off),
            &Variable(ref name) => {
                let r = self.lookup_variable(name);
                match self.types.get(name).unwrap() {
                    &Type::ArrayOf(_, _) => {
                        self.code.push(format!("leal {}, %eax", r));
                        "%eax".to_string()
                    },
                    &Type::Char => {
                        self.code.push(format!("movzbl {}, %eax", r));
                        "%eax".to_string()
                    }
                    _ => r
                }
            }
        };

        if need_register && &res[0..1] != "%" {
            self.code.push(format!("movl {}, %eax", res));
            "%eax".to_string()
        } else {
            res
        }
    }

    fn lookup_lval(&mut self, lval: &LVal) -> String {
        use itl::LVal::*;

        match lval {
            &Variable(ref name) => {
                self.lookup_variable(name)
            },
            &Indirect(ref name, ref off) => {
                self.lookup_indirect(name, off)
            }
            _ => panic!("Cannot lookup Discard lvalue")
        }
    }

    fn comparison_op(&mut self, res_op: &str, left: String, right: String) -> String {
        self.code.push(format!("cmpl {}, {}", right, left));
        self.code.push(format!("movl $0, %eax"));
        self.code.push(format!("{} %al", res_op));
        "%eax".to_string()
    }

    fn rval_is_byte(&self, rval: &RVal) -> bool {
        use itl::RVal::*;

        match rval {
            &Variable(ref name) | &Indirect(ref name, _) =>
                self.types.get(name).unwrap() == &Type::Char,
            _ => false
        }
    }

    fn lval_is_byte(&self, lval: &LVal) -> bool {
        use itl::LVal::*;

        match lval {
            &Variable(ref name) | &Indirect(ref name, _) =>
                self.types.get(name).unwrap() == &Type::Char,
            _ => false
        }
    }

    fn assemble_binop(&mut self, dest: &LVal, op: &Operator, l: &RVal, r: &RVal) {
        use ast::Operator::*;
        let left = self.lookup_rval(l, true);
        let right = self.lookup_rval(r, false);
        let res_reg = match *op {
            Add => {
                if right == "$1" {
                    self.code.push(format!("incl {}", left));
                } else if right != "$0" {
                    self.code.push(format!("addl {}, {}", right, left));
                }
                left
            },
            Sub => {
                if right == "$1" {
                    self.code.push(format!("decl {}", left));
                } else if right != "$0" {
                    self.code.push(format!("subl {}, {}", right, left));
                }
                left
            },
            Mul => {
                if let &RVal::Immediate(ref x) = r {
                    self.code.push(format!("movl {}, %ecx", right));
                    self.code.push(format!("imull %ecx"));
                } else {
                    self.code.push(format!("imull {}", right));
                }
                left
            },
            Div => {
                self.code.push(format!("movl $0, %edx"));
                if let &RVal::Immediate(_) = r {
                    self.code.push(format!("movl {}, %ecx", right));
                    self.code.push(format!("idivl %ecx"));
                } else {
                    self.code.push(format!("idivl {}", right));
                }
                left
            },
            Mod => {
                self.code.push(format!("movl $0, %edx"));
                if let &RVal::Immediate(_) = r {
                    self.code.push(format!("movl {}, %ecx", right));
                    self.code.push(format!("idivl %ecx"));
                } else {
                    self.code.push(format!("idivl {}", right));
                }
                "%edx".to_string()
            },
            Eql => self.comparison_op("sete", left, right),
            NotEql => self.comparison_op("setne", left, right),
            Lt => self.comparison_op("setb", left, right),
            Lte => self.comparison_op("setbe", left, right),
            Gt => self.comparison_op("seta", left, right),
            Gte => self.comparison_op("setae", left, right),
            BitAnd | And => {
                self.code.push(format!("andl {}, {}", right, left));
                left
            },
            BitOr | Or => {
                self.code.push(format!("orl {}, {}", right, left));
                left
            },
            BitXor => {
                self.code.push(format!("xorl {}, {}", right, left));
                left
            }
        };
        if dest != &LVal::Discard {
            let dest_val = self.lookup_lval(dest);
            if self.lval_is_byte(dest){
                self.code.push(format!("movb {}, {}", res_reg, dest_val));
            } else {
                self.code.push(format!("movl {}, {}", res_reg, dest_val));
            }
        }
    }

    fn assemble_opcode(&mut self, func_name: &String, op: &OpCode) {
        match op {
            &Assign(ref dest, ref val) => {
                let r = self.lookup_rval(val, true);
                let l = self.lookup_lval(dest);
                if self.lval_is_byte(dest){
                    self.code.push(format!("movb {}, {}", r, l));
                } else {
                    self.code.push(format!("movl {}, {}", r, l));
                }
            },
            &BinOp(ref dest, ref oper, ref l, ref r) => {
                self.assemble_binop(dest, oper, l, r);
            },
            &Goto(ref jmp) => {
                self.code.push(format!("jmp __{}_{}", func_name, jmp));
            },
            &If(ref cond, ref jmp) => {
                let cond_val = self.lookup_rval(cond, false);
                self.code.push(format!("testl $1, {}", cond_val));
                self.code.push(format!("jz __{}_{}", func_name, jmp));
            },
            &Call(ref dest, ref func, ref args) => {
                for arg in args.iter().rev() {
                    let val = self.lookup_rval(arg, false);
                    let to_push = if self.rval_is_byte(arg) && &val[0..1] != "%" {
                        self.code.push(format!("movzbl {}, %ecx", val));
                        "%ecx".to_string()
                    } else {
                        val
                    };
                    self.code.push(format!("pushl {}", to_push));
                }
                self.code.push(format!("call {}", func));
                if dest != &LVal::Discard {
                    let l = self.lookup_lval(dest);
                    self.code.push(format!("movl %eax, {}", l));
                }
                if args.len() > 0 {
                    self.code.push(format!("addl ${}, %esp", 4 * args.len()));
                }
            },
            &Return(ref val) => {
                let retval = self.lookup_rval(val, false);
                self.code.push(format!("mov {}, %eax", retval));
                self.code.push(format!("leave"));
                self.code.push(format!("ret"));
            },
            &NOP => {},
        }
    }

    fn assemble_block(&mut self, name: &String, blk: &Block) {
        let labels = blk.get_labels();
        if name == "tiny" {
            self.code.push(format!(".global tiny"));
        }
        self.code.push(format!("{}:", name));
        self.code.push(format!("enter ${}, $0", self.stacksize));

        for (i, op) in blk.code.iter().enumerate() {
            if labels.contains(& i) {
                self.code.push(format!("__{}_{}:", name, i));
            }
            self.assemble_opcode(name, op);
        }

        self.code.push(format!("__{}_{}:", name, blk.code.len()));
        self.code.push(format!("leave"));
        self.code.push(format!("ret"));
        self.code.push(format!(".type {}, @function", name));
    }

    pub fn new() -> Assembler {
        Assembler {
            code: vec![],
            locals: HashMap::new(),
            params: HashMap::new(),
            types: HashMap::new(),
            stacksize: 0
        }
    }

    pub fn assemble(prog: &Program) -> String {
        let mut res = vec![];

        res.push(".data".to_string());
        for (name, typ) in prog.globals.iter() {
            res.push(format!("{}:", name));
            let t = match typ.base() {
                Type::Int => "long",
                Type::Char => "byte",
                _ => panic!("Unexpected type base `{}`", typ),
            };
            let value = match typ {
                &Type::ArrayOf(_, _) => {
                    let n_elements = typ.size() / typ.base().size();
                    let zeros: Vec<String> = (0..n_elements).map(|_| "0".to_string())
                                                            .collect();
                    zeros.join(",")
                },
                _ => "0".to_string(),
            };
            res.push(format!(".{} {}", t, value));
            res.push("".to_string());
        }

        res.push(".text".to_string());
        for (name, blk) in prog.functions.iter() {
            let mut block_ass = Self::new();
            block_ass.build_stackframe(blk);
            block_ass.assemble_block(name, blk);
            res.append(&mut block_ass.code);
            res.push("".to_string());
        }
        res.push(".ident \"TinyCC\"".to_string());

        return res.join("\n");
    }
}
