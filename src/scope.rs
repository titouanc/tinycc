use ::ast;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Scope {
    vars: HashMap<String, Variable>,
    funcs: HashMap<String, Function>,
    sts: Vec<ast::Statement>,
}

pub enum Lookup<'a> {
    NotFound,
    Var(& 'a Variable),
    Func(& 'a Function)
}

impl Scope {
    pub fn new() -> Scope {
        Scope {vars: HashMap::new(), funcs: HashMap::new(), sts: vec![]}
    }

    pub fn add_variable(&mut self, name: &String, typ: &ast::Type) {
        let res = Variable {name: name.to_string(), typ: typ.clone()};
        self.vars.insert(name.to_string(), res);
    }

    pub fn add_function(&mut self, name: &String, typ: &ast::Type,
                                   args: &Vec<(String, ast::Type)>,
                                   body: &Vec<ast::Statement>) {
        let res = Function {
            name: name.to_string(),
            typ: typ.clone(),
            args: args.iter().map(|ref arg|
                Variable {name: arg.0.to_string(), typ: arg.1.clone()}
            ).collect(),
            inner_scope: Scope::new(),
            body: vec![]
        };
        self.funcs.insert(name.to_string(), res);
    }

    pub fn lookup<'a>(& 'a self, name: &String) -> Lookup<'a> {
        match self.vars.get(name) {
            Some(x) => return Lookup::Var(x),
            None => match self.funcs.get(name) {
                Some(x) => return Lookup::Func(x),
                None => Lookup::NotFound
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    name: String,
    typ: ast::Type,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    name: String,
    typ: ast::Type,
    args: Vec<Variable>,
    inner_scope: Scope,
    body: Vec<ast::Statement>,
}

fn err<T>(msg: &str) -> Result<T,String> {
    Err(msg.to_string())
}

fn check_entry_point(global: &Scope) -> Result<bool,String> {
    match global.lookup(& "tiny".to_string()) {
        Lookup::Func(f) => {
            if f.typ != ast::Type::Int {
                return err("tiny() return type is not int")
            }
            if f.args.len() > 0 {
                return err("tiny() function takes no argument")
            }
        },
        _ => return err("Function tiny() not found")
    }

    Ok(true)
}

pub fn analyze(prog: &ast::Program) -> Result<Scope,String> {
    let mut global = Scope::new();

    for decl in prog {
        match decl {
            &ast::Declaration::Var(ref name, ref typ) => {
                global.add_variable(name, typ);
            },
            &ast::Declaration::Func(ref name, ref typ, ref args, ref body) => {
                global.add_function(name, typ, args, body)
            }
        }
    }

    if let Err(x) = check_entry_point(&global) {
        return Err(x)
    }

    Ok(global)
}
