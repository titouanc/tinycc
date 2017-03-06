use ::ast;

#[derive(Debug, PartialEq)]
pub struct Variable {
    name: String,
    typ: ast::Type,
}

impl Variable {
    pub fn new(name: &String, typ: &ast::Type) -> Variable {
        Variable {name: name.to_string(), typ: typ.clone()}
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    name: String,
    typ: ast::Type,
    args: Vec<Variable>,
    locals: Vec<Variable>,
}

#[derive(Debug, PartialEq)]
pub struct Program {
    vars: Vec<Variable>,
    funcs: Vec<Function>,
}

// pub enum Declaration {
//     Func(String, Type, Vec<(String, Type)>, Vec<Statement>),
//     Var(String, Type),
// }

fn find_locals(body: &Vec<ast::Statement>) -> Vec<Variable> {
    let mut res = vec![];
    for s in body {
        match s {
            &ast::Statement::LocalDecl(ref name, ref typ) =>
                res.push(Variable::new(name, typ)),
            _ => (),
        }
    }
    return res;
}

impl Program {
    pub fn new(prog: ast::Program) -> Program {
        let mut res = Program {vars: vec![], funcs: vec![]};
        for decl in prog {
            match &decl {
                &ast::Declaration::Var(ref name, ref typ) => {
                    res.vars.push(Variable {
                        name: name.to_string(),
                        typ: typ.clone()
                    });
                },
                &ast::Declaration::Func(ref name, ref typ, ref args, ref st) => {
                    res.funcs.push(Function {
                        name: name.to_string(),
                        typ: typ.clone(),
                        args: args.iter().map(|&(ref name, ref typ)| Variable {
                            name: name.to_string(),
                            typ: typ.clone(),
                        }).collect(),
                        locals: find_locals(st),
                    });
                },
            }
        }
        return res;
    }
}

// Cannot assign array
// Length of only arrays
// IndeX only arrays
// tiny() in program
// Call args
