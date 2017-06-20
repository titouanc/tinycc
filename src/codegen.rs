use ::itl::*;
use ::itl::OpCode::*;
use ::ast::Type;
use std::fmt;
use std::collections::HashMap;

struct Addressing {
    stacksize: usize,
    params: HashMap<String,usize>,
    locals: HashMap<String,usize>,
    types: HashMap<String,Type>,
}

impl Addressing {
    pub fn new(frame: &HashMap<String,(AddressSpace,Type)>) -> Addressing {
        let mut params = HashMap::new();
        let mut locals = HashMap::new();
        let mut types = HashMap::new();

        let mut local_off = 0;
        let mut param_off = 0;

        for (name, &(ref addr, ref typ)) in frame.iter() {
            match addr {
                &AddressSpace::Local => {
                    locals.insert(name.to_string(), local_off);
                    local_off += typ.size();
                },
                &AddressSpace::Param => {
                    params.insert(name.to_string(), param_off);
                    param_off += typ.size();
                }
                _ => {},
            }
            types.insert(name.to_string(), typ.clone());
        }

        Addressing {
            params: params,
            locals: locals,
            types: types,
            stacksize: local_off,
        }
    }

    fn lookup_name(&self, name: &String) -> String {
        if let Some(x) = self.params.get(name) {
            return format!("{}(%ebp)", x + 8);
        } else if let Some(x) = self.locals.get(name) {
            let off = (*x as i32) - (self.stacksize as i32);
            return format!("{}(%ebp)", off);
        }
        return format!("{}", name);
    }

    fn lookup_type(&self, name: &String) -> Type {
        self.types.get(name).unwrap().clone()
    }

    pub fn resolve_lval(&self, lval: &LVal) -> String {
        use itl::LVal::*;

        match lval {
            &Discard => format!(""),
            _ => self.resolve_rval(&lval.to_rval())
        }
    }

    pub fn resolve_rval(&self, rval: &RVal) -> String {
        use itl::RVal::*;

        match rval {
            &Immediate(ref x) => format!("${}", x),
            &Variable(ref name) => self.lookup_name(name),
            &Indirect(ref name, ref off) =>
                format!("{}({})", self.resolve_direct(off), self.lookup_name(name)),
        }
    }

    pub fn resolve_direct(&self, direct: &Direct) -> String {
        self.resolve_rval(&direct.to_rval())
    }
}

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
            param_off += if let &Type::ArrayOf(_, _) = typ {
                4 // Pointer size
            } else {
                typ.size() // Value size
            }
        }

        let mut local_off = 0;
        for (name, &(ref addr, ref typ)) in blk.frame.iter() {
            if addr == &AddressSpace::Local {
                self.locals.insert(name.to_string(), local_off);
                local_off += typ.size();
            }
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

    fn lookup_indirect(&mut self, name: &String, off: &Direct, need_register: bool) -> String {
        use itl::Direct::*;

        let var_ref = self.lookup_variable(name);
        self.code.push(format!("movl {}, %esi", var_ref));

        let mem = match off {
            &Immediate(ref idx) => format!("{}(%esi)", idx),
            &Variable(ref name) => {
                let idx = self.lookup_variable(name);
                self.code.push(format!("movl {}, %ecx", idx));
                format!("(%esi,%ecx)")
            }
        };
        if need_register {
            format!("movl {}, %eax", mem)
        } else {
            mem
        }
    }

    fn lookup_rval(&mut self, rval: &RVal, need_register: bool) -> String {
        use itl::RVal::*;

        match rval {
            &Immediate(ref x) => format!("${}", x),
            &Variable(ref name) => {
                let original = self.lookup_variable(name);
                if need_register {
                    self.code.push(format!("movl {}, %eax", original));
                    format!("%eax")
                } else {
                    original
                }
            },
            &Indirect(ref name, ref off) => {
                self.lookup_indirect(name, off, need_register)
            }
        }
    }

    fn lookup_lval(&mut self, lval: &LVal) -> String {
        use itl::LVal::*;

        match lval {
            &Variable(ref name) => {
                self.lookup_variable(name)
            },
            &Indirect(ref name, ref off) => {
                self.lookup_indirect(name, off, false)
            }
            _ => panic!("Cannot lookup Discard lvalue")
        }
    }

    fn assemble_opcode(&mut self, func_name: &String, op: &OpCode) {
        match op {
            &Assign(ref dest, ref val) => {
                let r = self.lookup_rval(val, true);
                let l = self.lookup_lval(dest);
                self.code.push(format!("movl {}, {}", r, l));
            },
            &BinOp(ref dest, ref op, ref l, ref r) => {},
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
                    self.code.push(format!("pushl {}", val));
                }
                self.code.push(format!("call {}", func));
                if dest != &LVal::Discard {
                    let l = self.lookup_lval(dest);
                    self.code.push(format!("movl %eax, {}", l));
                }
                self.code.push(format!("addl ${}, %esp", 4 * args.len()));
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
        self.code.push(format!(".global {}", name));
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
        return res.join("\n");
    }
}
