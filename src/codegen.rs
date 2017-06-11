use ::itl::*;
use ::itl::OpCode::*;
use ::ast;

pub trait ToAssembly {
    fn _to_asm(&self, label_prefix: &String) -> Vec<String>;

    fn to_asm(&self) -> Vec<String> {
        self._to_asm(&"".to_string())
    }
}



fn var_addr(stack: &Stack, name: &String) -> String {
    use self::StackOffset::*;

    match stack.offset_of(name) {
        Param(pos, _) => format!("{}(%ebp)", pos + 8),
        Local(pos, _) => format!("{}(%esp)", pos),
    }
}

fn comparison(res: &mut Vec<String>, opcode: &str, operand: &String) {
    res.push(format!("cmpl {}, %eax", operand));
    res.push(format!("movl $0, %eax"));
    res.push(format!("{} %al", opcode));
}

fn binop(res: &mut Vec<String>, opcode: &ast::Operator, operand: &String) {
    use ast::Operator::*;
    match opcode {
        &Add => {
            res.push(format!("addl {}, %eax", operand));
        }
        &Sub => {
            res.push(format!("subl {}, %eax", operand));
        }
        &Mul => {
            res.push(format!("imull {}", operand));
        }
        &Div => {
            res.push(format!("movl $0, %edx"));
            res.push(format!("idivl {}", operand));
        }
        &Mod => {
            res.push(format!("movl $0, %edx"));
            res.push(format!("divl {}, %eax", operand));
            res.push(format!("movl %edx, %eax"));
            return;
        }
        &Eql => {comparison(res, "sete", operand);}
        &NotEql => {comparison(res, "setne", operand);}
        &Lt => {comparison(res, "setb", operand);}
        &Lte => {comparison(res, "setbe", operand);}
        &Gt => {comparison(res, "seta", operand);}
        &Gte => {comparison(res, "setae", operand);}
        &And => {}
        &Or => {}
        &BitAnd => {
            res.push(format!("andl {}, %eax", operand));
        }
        &BitOr => {
            res.push(format!(" orl {}, %eax", operand));
        }
        &BitXor => {
            res.push(format!("xorl {}, %eax", operand));
        }
    }
}

// pub enum OpCode {
//     NOP,
//     BinOp(String, Op, String, String),   // Var = Var Op Var
//     Mad(String, String, String, String), // Var = Var * Var + Var
//     Immediate(String, i32),              // Var = immediate value
//     Assign(String, String),              // Var = Var
//     Goto(usize),                         // Goto <index>
//     If(String, usize),                   // If Var otherwise jump <index>
//     Call(String, String, Vec<String>),   // Var = Func(Vars...)
//     Load(String, String, String),        // Var = Var[Var]
//     Store(String, String, String),       // Var[Var] = Var
//     Return(String),                      // return Var
// }

impl ToAssembly for Block {
    fn _to_asm(&self, label_prefix: &String) -> Vec<String> {
        let mut res = vec![];
        let stack = self.get_stack();
        let labels = self.get_labels();
        res.push(format!("enter ${}, $0", stack.stackframe_size()));

        let mut i = 0;
        for op in self.code.iter() {
            if labels.contains(&i){
                res.push(format!("_{}_{}:", label_prefix, i));
            }
            match op {
                &BinOp(ref dest, ref op, ref l, ref r) => {
                    res.push(format!("movl {}, %eax", var_addr(&stack, &l)));
                    binop(&mut res, &op, &var_addr(&stack, &r));
                    res.push(format!("movl %eax, {}", var_addr(&stack, &dest)));
                },
                &Mad(ref dest, ref x, ref a, ref b) => {
                    res.push(format!("movl ${}, %eax", a));
                    res.push(format!("mull {}", var_addr(&stack, &x)));
                    res.push(format!("addl {}, %eax", var_addr(&stack, &b)));
                    res.push(format!("movl %eax, {}", var_addr(&stack, &dest)));
                },
                &Immediate(ref dest, ref val) => {
                    res.push(format!("movl ${}, {}", val, var_addr(&stack, dest)));
                },
                &Assign(ref dest, ref var) => {
                    res.push(format!("movl {}, %eax", var_addr(&stack, &var)));
                    res.push(format!("movl %eax, {}", var_addr(&stack, &dest)));
                },
                &Call(ref dest, ref func, ref args) => {
                    for arg in args.iter() {
                        res.push(format!("pushl {}", var_addr(&stack, arg)));
                    }
                    res.push(format!("call {}", func));
                    res.push(format!("addl ${}, %esp", 4*args.len()));
                    res.push(format!("movl %eax, {}", var_addr(&stack, dest)));
                },
                &Return(ref retval) => {
                    res.push(format!("mov {}, %eax", var_addr(&stack, retval)));
                    res.push(format!("leave"));
                    res.push(format!("ret"));
                },
                &Goto(ref dest) => {
                    res.push(format!("jmp _{}_{}", label_prefix, dest));
                },
                &If(ref cond, ref jmp) => {
                    res.push(format!("cmpl $1, {}", var_addr(&stack, cond)));
                    res.push(format!("jne _{}_{}", label_prefix, jmp));
                },
                _ => {}
            }
            i += 1;
        }

        res.push(format!("_{}_{}:", label_prefix, i));

        res.push(format!("leave"));
        res.push(format!("ret"));
        return res;
    }
}

impl ToAssembly for Program {
    fn _to_asm(&self, label_prefix: &String) -> Vec<String> {
        let mut res = vec![];
        for (name, block) in self.functions.iter() {
            res.push(format!(""));
            res.push(format!(".global {}", name));
            res.push(format!("{}:", name));
            res.append(&mut block._to_asm(name));
        }
        return res
    }
}
