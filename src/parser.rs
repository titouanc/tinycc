use ::ast::*;
use ::lexer::*;
use ::tokens::*;
use ::tokens::Token::*;

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
        typ[rettype] Name(name) LParen formal_params[params] RParen block[body] => {
            Declaration::Func(name, rettype, params, body)
        }
    }

    formal_params: Vec<(Type, String)> {
        => vec![],
        formal_arg[arg] => vec![arg],
        formal_params[mut left] Comma formal_arg[arg] => {
            left.push(arg);
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
        statement[s] => vec![s],
        statements[mut prev] Semicol statement[s] => {
            prev.push(s);
            prev
        }
    }

    statement: Statement {
        typ[t] Name(name) Semicol => Statement::LocalDecl(name, t),
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

    fn var(name: &str, typ: Type) -> Declaration {
        Declaration::Var(name.to_string(), typ)
    }

    fn func(name: &str, ret: Type, args: Vec<(Type, String)>, body: Vec<Statement>) -> Declaration {
        Declaration::Func(name.to_string(), ret, args, body)
    }

    fn int_f(args: Vec<(Type, String)>, body: Vec<Statement>) -> Declaration {
        func("f", Type::Int, args, body)
    }

    #[test]
    fn var_decl(){
        test_str_decl("int i;", var("i", Type::Int));
        test_str_decl("char c;", var("c", Type::Char));
    }

    #[test]
    fn fundecl_noargs(){
        test_str_decl("int f(){}", int_f(vec![], vec![]));
    }

    #[test]
    fn fundecl_1arg(){
        test_str_decl("int f(int x){}",
                      int_f(vec![(Type::Int, "x".to_string())], vec![]));
    }

    #[test]
    fn fundecl_2args(){
        test_str_decl("int f(int x, char y){}",
                      int_f(vec![(Type::Int, "x".to_string()),
                                 (Type::Char, "y".to_string())],
                            vec![]));
    }

    #[test]
    fn fundecl_4args(){
        test_str_decl("int f(int x, char y, char z, char w){}",
                      int_f(vec![(Type::Int, "x".to_string()),
                                 (Type::Char, "y".to_string()),
                                 (Type::Char, "z".to_string()),
                                 (Type::Char, "w".to_string())],
                            vec![]));
    }

    #[test]
    fn fundecl_body(){
        test_str_decl("int f(){int x;}",
                      int_f(vec![],
                            vec![Statement::LocalDecl("x".to_string(), Type::Int)]));
    }
}
