use ::itl::*;
use ::itl::OpCode::*;
use std::fmt;

fn assemble_block(name: &String, blk: &Block) -> Vec<String> {
    let mut res = vec![
        format!(".global {}", name),
        format!("{}:", name),
        format!("enter"),
        format!("leave"),
        format!("ret")
    ];
    return res;
}

pub fn assemble(prog: &Program) -> String {
    let mut res = vec![];
    for (name, blk) in prog.functions.iter() {
        let mut assembled = assemble_block(name, blk);
        res.append(&mut assembled);
    }
    return res.join("\n");
}
