use ::lexer::*;
use ::tokens::*;
use ::tokens::Token::*;
use ::ast::*;

pub type Parsed = Result<Program, (Option<(Token, Span)>, &'static str)>;

parser! {
    fn parse_(Token, Span);

    // Combine 2 spans
    (a, b) {Span {lo: a.lo, hi: b.hi,}}


    program: Program {
        => vec![],
        program[mut prog] vardecl[d] Semicol => {
            prog.push(d);
            prog
        }
        program[mut prog] fundecl[d] => {
            prog.push(d);
            prog
        }
    }

    vardecl: Declaration {
        typ[rettype] Name(name) => Declaration::Var(name, rettype),
    }

    fundecl: Declaration {
        typ[rettype] Name(name) LParen formal_params[params] RParen block => {
            Declaration::Func(name, rettype, params)
        }
    }

    formal_params: Vec<(Type, String)> {
        => vec![],
        typ[t] Name(name) => vec![(t, name)],
        formal_params[mut left] Comma typ[t] Name(name) => {
            left.push((t, name));
            left
        }
    }

    formal_arg: (Type, String) {
        typ[t] Name(name) => (t, name),
    }

    block: Vec<Statement> {
        LBrace statements[s] RBrace => s,
    }

    statements: Vec<Statement> {
        => vec![],
        statements[mut prev] Semicol statement[s] => {
            prev.push(s);
            prev
        }
    }

    statement: Statement {
        If LParen expr[cond] RParen statement[cons] => {
            Statement::Condition(Box::new(cond), Box::new(cons), Box::new(None))
        },
        // If LParen expr[cond] RParen statement[cons] Else statement[alt] => {
        //     Statement::Condition(Box::new(cond), Box::new(cons), Box::new(Some(alt)))
        // },
        While LParen expr[cond] RParen statement[body] => {
            Statement::Loop(Box::new(cond), Box::new(body))
        }
        lvalue[lhs] Assign expr[rhs] => {
            Statement::Assign(Box::new(lhs), Box::new(rhs))
        }
    }

    lvalue: LValue {
        Name(name) => LValue::Identifier(name),
        lvalue[lhs] LBrack expr[idx] RBrack => {
            LValue::ArrayItem(Box::new(lhs), Box::new(idx))
        }
    }

    expr: Expr {
        => Expr::Lit(0)
    }

    typ: Type {
        Int => Type::Int,
        Char => Type::Char,
    }
}

pub fn parse_string(input: &str, print_err: bool) -> Program {
    match parse_(Lexer::new(input)) {
        Ok(res) => return res,
        Err((Some((_, loc)), err)) => {
            if print_err {
                println!("{} \x1b[31m>>\x1b[0m {}\x1b[31;1m{}\x1b[0m{}",
                         err,
                         input[..loc.lo].to_string(),
                         input[loc.lo..loc.hi].to_string(),
                         input[loc.hi..].to_string());
            }
            panic!("Parse error: {} in ``{}''", err, input[loc.lo..loc.hi].to_string())
        },
        _ => panic!("Unknown parse error"),
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn test_str_decl(input: &str, decl: Declaration) {
        let res = parse_string(input, true);
        assert_eq!(res[0], decl);
    }

    #[test]
    fn var_decl(){
        test_str_decl("int i;",
                      Declaration::Var("i".to_owned(), Type::Int));
        test_str_decl("char c;",
                      Declaration::Var("c".to_owned(), Type::Char));
    }

    #[test]
    fn fundecl_noargs_nobody(){
        test_str_decl("int f(){}",
                      Declaration::Func("f".to_owned(), Type::Int, vec![]));
    }

    #[test]
    fn fundecl_1arg_nobody(){
        test_str_decl("int f(int x){}",
                      Declaration::Func("f".to_owned(), Type::Int,
                                                        vec![(Type::Int, "x".to_owned())]));
    }

    #[test]
    fn fundecl_2args_nobody(){
        test_str_decl("int f(int x, char y){}",
                      Declaration::Func("f".to_owned(), Type::Int,
                                                        vec![(Type::Int, "x".to_owned()),
                                                             (Type::Char, "y".to_owned())]));
    }
}
