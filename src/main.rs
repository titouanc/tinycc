#![feature(plugin)]
#![plugin(plex)]

pub mod tokens;
pub mod lexer;
pub mod ast;
pub mod parser;

fn parse(input: &str) -> parser::Parsed {
    let lex = lexer::Lexer::new(input);
    return parser::parse(lex);
}

fn demo_parse(input: &str) {
    if let Ok(_) = parse(input) {
        println!("ok");
    } else {
        println!("Parse error");
    }
}

fn main() {
    demo_parse(" 3 + 9 - 4");
    println!("Goodbye !");
}
