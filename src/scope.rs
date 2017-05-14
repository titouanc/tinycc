use ::ast;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Scope<'a> {
    parent: Option<& 'a Scope<'a>>,
    pub vars: HashMap<String,Variable>,
    pub funcs: HashMap<String,Function>,
}

impl <'a> Scope<'a> {
    pub fn new() -> Scope<'a> {
        Scope {vars: HashMap::new(), funcs: HashMap::new(), parent: None}
    }

    pub fn sub(&self) -> Scope {
        Scope {vars: HashMap::new(), funcs: HashMap::new(), parent: Some(self)}
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
            body: body.clone()
        };
        self.funcs.insert(name.to_string(), res);
    }

    pub fn lookup_var(&self, name: &String) -> Option<Variable> {
        if let Some(v) = self.vars.get(name) {
            Some(v.clone())
        } else if let Some(ref p) = self.parent {
            p.lookup_var(name)
        } else {
            None
        }
    }

    pub fn lookup_func(&self, name: &String) -> Option<Function> {
        if let Some(f) = self.funcs.get(name) {
            Some(f.clone())
        } else if let Some(ref p) = self.parent {
            p.lookup_func(name)
        } else {
            None
        }
    }

    fn check_entry_point(&self) -> Result<(),String> {
        if let Some(f) = self.funcs.get(& "tiny".to_string()) {
            if f.typ != ast::Type::Int {
                return err("tiny() return type is not int")
            }
            if f.args.len() > 0 {
                return err("tiny() function takes no argument")
            }
        } else {
            return err("Function tiny() not found");
        }

        Ok(())
    }

    fn check_lvalue(&self, lval: &ast::LValue) -> Result<(), String> {
        match lval {
            &ast::LValue::Identifier(ref name) => {
                if let None = self.lookup_var(name) {
                    return Err(format!("Variable `{}` not found", name));
                }
            },
            &ast::LValue::ArrayItem(ref lval, ref expr) => {
                try!(self.check_lvalue(lval));
                try!(self.check_expr(expr));
            }
        }
        Ok(())
    }

    fn check_expr(&self, expr: &ast::Expression) -> Result<(), String> {
        match expr {
            &ast::Expression::LValue(ref x) => self.check_lvalue(x),
            &ast::Expression::Funcall(ref name, ref args) => {
                if let None = self.lookup_func(name) {
                    return Err(format!("Function `{}` not found", name));
                }
                for arg in args {
                    try!(self.check_expr(arg));
                }
                Ok(())
            },
            _ => Ok(())
        }
    }

    fn check_statement(&mut self, s: &ast::Statement) -> Result<(),String> {
        match s {
            &ast::Statement::LocalDecl(ref n, ref t) => {
                self.add_variable(&n, &t)
            },
            &ast::Statement::Return(ref expr) => {
                try!(self.check_expr(expr));
            },
            &ast::Statement::Assign(ref lval, ref expr) => {
                try!(self.check_lvalue(lval));
                try!(self.check_expr(expr));
            },
            _ => {}
        }
        Ok(())
    }

    fn check_functions(&self) -> Result<(),String> {
        for (_, ref func) in self.funcs.iter() {
            let mut sub = self.sub();
            for ref v in func.args.iter() {
                sub.add_variable(& v.name, & v.typ);
            }
            for s in func.body.iter() {
                try!(sub.check_statement(s));
            }
            println!("{:?}", sub.vars);
        }
        Ok(())
    }

    pub fn static_check(&self) -> Result<(),String> {
        try!(self.check_entry_point());
        try!(self.check_functions());
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
    name: String,
    typ: ast::Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    name: String,
    typ: ast::Type,
    args: Vec<Variable>,
    body: Vec<ast::Statement>,
}

fn err<T>(msg: &str) -> Result<T,String> {
    Err(msg.to_string())
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

    try!(global.static_check());
    Ok(global)
}
