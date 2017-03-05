pub mod ast;
pub mod grammar; // synthesized by LALRPOP

extern crate lalrpop_util;
use lalrpop_util::ParseError::*;

pub fn compile(src: &str) -> ast::Program {
    match grammar::parse_Program(src) {
        Ok(prog) => return prog,
        Err(err) => {
            match err {
                InvalidToken {location: loc} =>
                    println!("\x1b[34mInvalid token \x1b[31m {:?}\x1b[0m", loc),
                UnrecognizedToken {token: Some((lo, (_, t), hi)), expected: exp} => {
                    println!("\x1b[34mUnexpected token \x1b[31m{:?}\x1b[0m in", t);
                    println!("{}\x1b[41m{}\x1b[0m{}",
                             src[..lo].to_string(),
                             src[lo..hi].to_string(),
                             src[hi..].to_string());
                    println!("Expecting one of: {}", exp.join(", "));
                },
                _ => println!("{:?}", err),
            };
            panic!("Could not continue");
        },
    };
}

#[cfg(test)]
mod test_parser {
    use super::*;
    use ast::Expression::*;
    use ast::Operator as Op;

    #[test]
    fn var_decl() {
        assert!(grammar::parse_Declaration("int x;").is_ok());
        assert!(grammar::parse_Declaration("char x;").is_ok());
    }

    #[test]
    fn array_decl() {
        assert!(grammar::parse_Declaration("int[10] x;").is_ok());
        assert!(grammar::parse_Declaration("char[10][42] x;").is_ok());
    }

    #[test]
    fn func_decl_without_args() {
        assert!(grammar::parse_Declaration("int f(){}").is_ok());
    }

    #[test]
    fn func_decl_without_body() {
        assert!(grammar::parse_Declaration("int f(int x, int y){}").is_ok());
    }

    #[test]
    fn func_decl() {
        assert!(grammar::parse_Declaration("int f(int x, int y){return x + y;}").is_ok());
    }

    #[test]
    fn binary_operators() {
        assert!(grammar::parse_Expr("3 + 4").is_ok());
        assert!(grammar::parse_Expr("3 + 4 * 5").is_ok());
        assert!(grammar::parse_Expr("3 + 4 * 5 % 3").is_ok());
    }

    #[test]
    fn op_precedence() {
        let expr = grammar::parse_Expr("3 + 4 * 5 % 3").unwrap();
        let expected = InfixOp(Op::Add,
                               Box::new(Lit(3)),
                               Box::new(InfixOp(Op::Mod,
                                                Box::new(InfixOp(Op::Mul,
                                                                 Box::new(Lit(4)),
                                                                 Box::new(Lit(5)))),
                                                Box::new(Lit(3)))));
        assert_eq!(expr, expected);
    }

    #[test]
    fn expr_symbolic() {
        assert!(grammar::parse_Expr("x + 4").is_ok());
        assert!(grammar::parse_Expr("3 + x * 5").is_ok());
        assert!(grammar::parse_Expr("3 + x * 5 % y").is_ok());
    }

    #[test]
    fn array_access() {
        assert!(grammar::parse_Expr("x[14][12]").is_ok());
    }

    #[test]
    fn function_call(){
        assert!(grammar::parse_Expr("f()").is_ok());
        assert!(grammar::parse_Expr("f(x)").is_ok());
        assert!(grammar::parse_Expr("f(x, y, z)").is_ok());
    }

    #[test]
    fn if_expr(){
        assert!(grammar::parse_Expr("(3 < 4) ? 1 : 2").is_ok());
    }

    #[test]
    fn if_statement(){
        assert!(grammar::parse_Statement("if (x == 0){return 3;}").is_ok());
        assert!(grammar::parse_Statement("if (x == 0) return 3;").is_ok());
    }

    #[test]
    fn if_else_statement(){
        assert!(grammar::parse_Statement("if (x == 0){return 3;} else {return 4;}").is_ok());
        assert!(grammar::parse_Statement("if (x == 0) return 3; else return 4;").is_ok());
    }

    #[test]
    fn while_statement(){
        assert!(grammar::parse_Statement("while (x == 0){return 3;}").is_ok());
        assert!(grammar::parse_Statement("while (x == 0) return 3;").is_ok());
    }

    fn test_with_repr(in_: &str, out: &str) {
        let prog = compile(in_);
        let repr = format!("{}", prog[0]);
        println!("\x1b[33mComparison:\x1b[0m");
        println!("     Got: {}", repr);
        println!("Expected: {}", out);
        assert_eq!(repr, out);
    }

    #[test]
    fn nested_if(){

        test_with_repr("int f(){if (x) if (y) return 1; else return 2;}",
                       "int f(){if (x){if (y){return 1;} else {return 2;}}}");
    }

    #[test]
    fn nested_else(){
        test_with_repr("int f(){if (x) return 1; else if (y) return 2; else return 3;}",
                       "int f(){if (x){return 1;} else {if (y){return 2;} else {return 3;}}}");
    }

    #[test]
    fn nested_if_else(){
        test_with_repr("int f(){if (x) if (y) return 1; else return 2; else return 3;}",
                       "int f(){if (x){if (y){return 1;} else {return 2;}} else {return 3;}}");
    }
}
