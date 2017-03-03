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
        program[mut prog] declaration[d] => {
            prog.push(d);
            prog
        }
    }

    declaration: Declaration {
        vardecl[d] Semicol => d,
        fundecl[d] => d,
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
        statement[s] => vec![s],
        LBrace statements[s] RBrace => s,
    }

    statements: Vec<Statement> {
        => vec![],
        statements[mut prev] statement[s] => {
            prev.push(s);
            prev
        }
    }

    statement: Statement {
        typ[t] Name(name) Semicol => Statement::LocalDecl(name, t),
        expr[e] Semicol => Statement::RValue(Box::new(e)),
        lvalue[l] Assign expr[r] Semicol => Statement::Assign(Box::new(l), Box::new(r)),
        Return expr[r] Semicol => Statement::Return(Box::new(r)),
        While LParen expr[cond] RParen block[body] => Statement::Loop(Box::new(cond), body),
        #[no_reduce(Else)]
        If LParen expr[cond] RParen block[cons] => Statement::Condition(Box::new(cond), cons, vec![]),
        If LParen expr[cond] RParen block[cons] Else block[alt] => Statement::Condition(Box::new(cond), cons, alt),
    }

    lvalue: LValue {
        Name(name) => LValue::Identifier(name),
        lvalue[lhs] LBrack expr[idx] RBrack => {
            LValue::ArrayItem(Box::new(lhs), Box::new(idx))
        }
    }

    cmp: Expr {
        term[x] => x,
        term[l] Equal term[r] => Expr::InfixOp(Op::Eql, Box::new(l), Box::new(r)),
        term[l] NotEqual term[r] => Expr::InfixOp(Op::NotEql, Box::new(l), Box::new(r)),
        term[l] Lt term[r] => Expr::InfixOp(Op::Lt, Box::new(l), Box::new(r)),
        term[l] Lte term[r] => Expr::InfixOp(Op::Lte, Box::new(l), Box::new(r)),
        term[l] Gt term[r] => Expr::InfixOp(Op::Gt, Box::new(l), Box::new(r)),
        term[l] Gte term[r] => Expr::InfixOp(Op::Gte, Box::new(l), Box::new(r)),
    }

    term: Expr {
        fact[x] => x,
        term[l] Plus fact[r] => Expr::InfixOp(Op::Add, Box::new(l), Box::new(r)),
        term[l] Minus fact[r] => Expr::InfixOp(Op::Sub, Box::new(l), Box::new(r)),
    }

    fact: Expr {
        atom[x] => x,
        fact[l] Times atom[r] => Expr::InfixOp(Op::Mul, Box::new(l), Box::new(r)),
        fact[l] Divide atom[r] => Expr::InfixOp(Op::Div, Box::new(l), Box::new(r)),
    }

    atom: Expr {
        Integer(i) => Expr::Lit(i),
        Qchar(c) => Expr::Lit(c as i32),
        lvalue[l] => Expr::LValue(Box::new(l)),
        LParen expr[e] RParen => e,
    }

    expr: Expr {
        cmp[t] => t
    }

    typ: Type {
        Int => Type::Int,
        Char => Type::Char,
        typ[t] LBrack Integer(size) RBrack => {
            Type::ArrayOf(Box::new(t), size as usize)
        },
    }
}

pub fn parse_string(input: &str, print_err: bool) -> Program {
    match parse_(Lexer::new(input)) {
        Ok(res) => return res,
        Err((Some((_, loc)), err)) => {
            if print_err {
                println!("\x1b[31;1m!\x1b[0m {}\n\x1b[34m>>\x1b[0m {}\x1b[31;1m{}\x1b[0m{}",
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
    fn var_decl_array(){
        test_str_decl("int[10] i;",
                      var("i", Type::ArrayOf(Box::new(Type::Int), 10)));
        test_str_decl("int[10][10] i;",
                      var("i", Type::ArrayOf(Box::new(Type::ArrayOf(Box::new(Type::Int), 10)), 10)));
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
    fn fundecl_return_lit(){
        test_str_decl("int f(){return 3;}",
                      int_f(vec![],
                            vec![Statement::Return(Box::new(Expr::Lit(3)))]))
    }

    #[test]
    fn fundecl_declare_local(){
        test_str_decl("int f(){int x;}",
                      int_f(vec![],
                            vec![Statement::LocalDecl("x".to_string(), Type::Int)]));
    }

    #[test]
    fn fundecl_return_local_constant(){
        parse_string("int f(){int x; x = 3; return x;}", true);
    }

    #[test]
    fn fundecl_return_local_constant_paren(){
        parse_string("int f(){int x; x = (3); return (x);}", true);
    }

    #[test]
    fn fundecl_return_sum_args(){
        parse_string("int f(int x, int y){return x + y;}", true);
    }

    #[test]
    fn fundecl_return_sum_constants(){
        parse_string("int f(){return 3 + 2;}", true);
    }

    #[test]
    fn fundecl_assign_sum_to_local(){
        parse_string("int f(int x, int y){int z; z = x + y;}", true);
    }

    #[test]
    fn fundecl_assign_sum_to_array(){
        parse_string("int f(int x, int y){int[10] z; z[0] = x + y;}", true);
    }

    #[test]
    fn fundecl_3sum(){
        parse_string("int f(){return 3 + 4 + 5;}", true);
    }

    #[test]
    fn fundecl_natural_binop_priority(){
        parse_string("int f(){return 3 + 4 * 5;}", true);
    }

    #[test]
    fn fundecl_enforce_binop_priority(){
        parse_string("int f(){return 3 + (4 * 5);}", true);
        parse_string("int f(){return (3+4) * 5;}", true);
    }

    #[test]
    fn fundecl_if(){
        parse_string("int f(){if (1){return 42;}}", true);
        parse_string("int f(){if (1){return 42;} else {return 25;}}", true);
    }

    #[test]
    fn fundecl_if_nobraces(){
        parse_string("int f(){if (1) return 42;}", true);
        parse_string("int f(){if (1) return 42; else return 25;}", true);
    }

    #[test]
    fn fundecl_while(){
        parse_string("int f(){int x; x=0; while (x < 10){x = x+1;}}", true);
    }
}
