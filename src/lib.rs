pub mod ast;
pub mod scope;
pub mod itl;
pub mod grammar; // synthesized by LALRPOP

extern crate lalrpop_util;
use lalrpop_util::ParseError::*;

pub fn parse(src: &str) -> ast::Program {
    match grammar::parse_Program(src) {
        Ok(prog) => return prog,
        Err(err) => {
            match err {
                InvalidToken {location: loc} =>
                    println!("\x1b[34mInvalid token \x1b[31m {:?}\x1b[0m",
                             src[loc..loc+1].to_string()),
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

pub fn compile(src: &str) {
    use ast::AST;

    let tree = parse(src).const_fold();
    match scope::analyze(&tree){
        Ok(_) => {},
        Err(msg) => {
            for decl in tree.iter() {
                println!("{}", decl);
            }
            panic!("\x1b[31;1mStatic analysis error:\x1b[0m {}", msg);
        }
    }

    let sequential = itl::Program::internalize(&tree);
    println!("{}", sequential);
}

#[cfg(test)]
mod test_parser {
    use super::*;
    
    fn test_with_repr(in_: &str, out: &str) {
        let prog = parse(in_);
        let repr = format!("{}", prog[0]);
        println!("\x1b[33mComparison:\x1b[0m");
        println!("     Got: {}", repr);
        println!("Expected: {}", out);
        assert_eq!(repr, out);
    }

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
        let repr = format!("{}", expr);
        assert_eq!(repr, "(3 + ((4 * 5) % 3))");
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

    #[test]
    fn accepts_scalar(){
        let t1 = ast::Type::Int;
        let t2 = ast::Type::Int;
        let t3 = ast::Type::Char;
        let t4 = ast::Type::ArrayOf(Box::new(ast::Type::Int), 42);
        assert!(t1.accepts(&t2));
        assert!(t2.accepts(&t1));
        assert!(t1.accepts(&t3));
        assert!(t3.accepts(&t1));
        assert!(! t1.accepts(&t4));
        assert!(! t4.accepts(&t1));
    }

    #[test]
    fn accepts_1d(){
        let t1 = ast::Type::ArrayOf(Box::new(ast::Type::Int), 42);
        let t2 = ast::Type::ArrayOf(Box::new(ast::Type::Int), 10);
        let t3 = ast::Type::ArrayOf(Box::new(t2.clone()), 42);
        assert!(t1.accepts(&t2));
        assert!(t2.accepts(&t1));
        assert!(! t1.accepts(&t3));
        assert!(! t3.accepts(&t1));
    }
}
