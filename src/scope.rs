use ::ast;
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Scope<'a> {
    parent: Option<& 'a Scope<'a>>,
    pub vars: HashMap<String,Variable>,
    pub funcs: HashMap<String,Function>
}

fn err<T>(msg: &str) -> Result<T,String> {
    Err(msg.to_string())
}

fn format_args(args: &Vec<ast::Expression>) -> String {
    let args_s: Vec<String> = args.iter().map(|e| format!("{}", e)).collect();
    args_s.join(", ")
}

impl <'a> Scope<'a> {
    pub fn new() -> Scope<'a> {
        Scope {vars: HashMap::new(), funcs: HashMap::new(), parent: None}
    }

    pub fn sub(&self) -> Scope {
        Scope {vars: HashMap::new(), funcs: HashMap::new(), parent: Some(self)}
    }

    pub fn add_variable(&mut self, name: &String, typ: &ast::Type, global: bool) {
        let res = Variable {name: name.to_string(), typ: typ.clone(), global: global};
        self.vars.insert(name.to_string(), res);
    }

    pub fn add_function(&mut self, name: &String, typ: &ast::Type,
                                   args: &Vec<(String, ast::Type)>,
                                   body: &Vec<ast::Statement>) {
        let res = Function {
            name: name.to_string(),
            typ: typ.clone(),
            args: args.iter().map(|ref arg|
                Variable {name: arg.0.to_string(), typ: arg.1.clone(), global: false}
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

    fn get_lval_type(&self, lval: &ast::LValue) -> Result<ast::Type,String> {
        match lval {
            &ast::LValue::Identifier(ref name) => {
                if let Some(v) = self.lookup_var(name) {
                    Ok(v.typ.clone())
                } else {
                    Err(format!("Variable `{}` not found", name))
                }
            },
            &ast::LValue::ArrayItem(ref l, ref expr) => {
                let rest = self.get_lval_type(&*l);
                if let Ok(t) = rest {
                    match t {
                        ast::Type::ArrayOf(x, _) => Ok(*x.clone()),
                        _ => Err(format!("Attempt to subscript scalar in `{}`", lval))
                    }
                } else {
                    rest
                }
            }
        }
    }

    fn get_expr_type(&self, expr: &ast::Expression) -> Result<ast::Type,String> {
        match expr {
            &ast::Expression::Lit(_) => Ok(ast::Type::Int),
            &ast::Expression::CharLit(_) => Ok(ast::Type::Char),
            &ast::Expression::LValue(ref l) => self.get_lval_type(&*l),
            &ast::Expression::Funcall(ref name, _) => {
                if let Some(f) = self.lookup_func(name) {
                    Ok(f.typ.clone())
                } else {
                    Err(format!("Function {} not found", name))
                }
            },
            &ast::Expression::ArrayLen(_) => Ok(ast::Type::Int),

            &ast::Expression::InfixOp(_, ref l, ref r) => {
                let tl = try!(self.get_expr_type(l));
                let tr = try!(self.get_expr_type(r));

                if ! tl.accepts(&tr) {
                    Err(format!("Incompatible type in {} (got {} and {})",
                                expr, tl, tr))
                } else {
                    Ok(tl)
                }
            },
            &ast::Expression::Ternary(ref cond, ref l, ref r) => {
                let tcond = try!(self.get_expr_type(cond));
                if ! ast::Type::Int.accepts(&tcond) {
                    return Err(format!("{} is not a valid condition", cond));
                }

                let tl = try!(self.get_expr_type(l));
                let tr = try!(self.get_expr_type(r));
                if ! tl.accepts(&tr) {
                    Err(format!("Incompatible type in {} (got {} and {})",
                                expr, tl, tr))
                } else {
                    Ok(tl)
                }
            },
            _ => err("Not implemented")
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

    fn check_funcall(&self, func: &Function, given_args: &Vec<ast::Expression>) -> Result<(),String> {
        if func.args.len() != given_args.len() {
            Err(format!("Wrong number of arguments in call {}({}): expecting {} arguments, got {}",
                        func.name, format_args(given_args), func.args.len(), given_args.len()))
        } else {
            for i in 0..func.args.len() {
                let ref formal = func.args[i];
                let ref given = given_args[i];
                try!(self.check_expr(&given));
                if let Ok(t) = self.get_expr_type(&given) {
                    if ! formal.typ.accepts(&t) {
                        let e = format!("Argument {} has wrong type in call {}({}): expecting {}, got {}",
                                        i+1, func.name, format_args(given_args), t, formal.typ);
                        return Err(e)
                    }
                }
            }
            Ok(())
        }
    }

    fn check_expr(&self, expr: &ast::Expression) -> Result<(), String> {
        match expr {
            &ast::Expression::LValue(ref x) => self.check_lvalue(x),
            &ast::Expression::ArrayLen(ref x) => {
                try!(self.check_lvalue(&*x));
                if let Ok(t) = self.get_lval_type(&*x) {
                    match t {
                        ast::Type::ArrayOf(_, _) => return Ok(()),
                        _ => return Err(format!("Error in expression {}: {} is not an array", expr, x))
                    }
                } else {
                    Err(format!("Could not determine type of {}", x))
                }
            },
            &ast::Expression::Funcall(ref name, ref args) => {
                let f = self.lookup_func(name);
                match f {
                    None => Err(format!("Function `{}` not found", name)),
                    Some(ref f) => self.check_funcall(f, args)
                }
            },
            _ => {
                try!(self.get_expr_type(expr));
                Ok(())
            }
        }
    }

    fn check_block(&self, sts: &Vec<ast::Statement>, ret_type: &ast::Type) -> Result<(),String> {
        let mut sub = self.sub();
        let u: Vec<String> = sts.iter().map(|e| format!("{}", e)).collect();
        // println!("Analyze {}", u.join("\n        "));
        for s in sts.iter(){
            try!(sub.check_statement(s, ret_type));
        }
        Ok(())
    }

    fn check_statement(&mut self, s: &ast::Statement, ret_type: &ast::Type) -> Result<(),String> {
        match s {
            &ast::Statement::LocalDecl(ref n, ref t) => {
                self.add_variable(&n, &t, false)
            },
            &ast::Statement::RValue(ref expr) => {
                try!(self.check_expr(expr));
            }
            &ast::Statement::Return(ref expr) => {
                try!(self.check_expr(expr));
                let t = try!(self.get_expr_type(expr));
                if ! ret_type.accepts(&t) {
                    return Err(format!("Type error in `return {}`: expecting {}, got {}",
                                       expr, ret_type, t))
                }
            },
            &ast::Statement::Assign(ref lval, ref expr) => {
                try!(self.check_lvalue(lval));
                try!(self.check_expr(expr));
                let t = try!(self.get_lval_type(lval));
                let tv = try!(self.get_expr_type(expr));
                if ! t.accepts(&tv) {
                    return Err(format!("Type error in assignment {} = {}: expecting {}, got {}",
                                       lval, expr, t, tv));
                }
            },
            &ast::Statement::Condition(ref cond, ref cons, ref alt) => {
                try!(self.check_expr(cond));
                let tcond = try!(self.get_expr_type(cond));
                if ! ast::Type::Int.accepts(&tcond) {
                    return Err(format!("Not a valid condition: {}\nin {}",
                                       cond, s));
                }
                try!(self.check_block(&cons, ret_type));
                try!(self.check_block(&alt, ret_type));
            },
            &ast::Statement::Loop(ref cond, ref body) => {
                try!(self.check_expr(cond));
                let tcond = try!(self.get_expr_type(cond));
                if ! ast::Type::Int.accepts(&tcond) {
                    return Err(format!("Not a valid condition: {}\nin {}",
                                       cond, s));
                }
                try!(self.check_block(&body, ret_type));
            },
            _ => {return Err(format!("Not implemented {}", s));}
        }
        Ok(())
    }

    fn check_functions(&self) -> Result<(),String> {
        let print_func = Function {
            name: "print".to_string(),
            typ: ast::Type::Int,
            args: vec![Variable {name: "something".to_string(), typ: ast::Type::Int, global:false}],
            body: vec![]
        };
        let read_func = Function {
            name: "read".to_string(),
            typ: ast::Type::Char,
            args: vec![],
            body: vec![]
        };
        for (_, ref func) in self.funcs.iter() {
            let mut sub = self.sub();
            for ref v in func.args.iter() {
                sub.add_variable(& v.name, & v.typ, false);
            }
            sub.funcs.insert("print".to_string(), print_func.clone());
            sub.funcs.insert("read".to_string(), read_func.clone());
            try!(sub.check_block(&func.body, &func.typ));
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
    global: bool
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    name: String,
    typ: ast::Type,
    args: Vec<Variable>,
    body: Vec<ast::Statement>,
}

pub fn analyze(prog: &ast::Program) -> Result<Scope,String> {
    let mut global = Scope::new();

    for decl in prog {
        match decl {
            &ast::Declaration::Var(ref name, ref typ) => {
                global.add_variable(name, typ, true);
            },
            &ast::Declaration::Func(ref name, ref typ, ref args, ref body) => {
                global.add_function(name, typ, args, body)
            }
        }
    }

    try!(global.static_check());
    Ok(global)
}
