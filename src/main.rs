#![feature(plugin)]
#![plugin(plex)]

pub mod lexer;
pub mod ast;
pub mod parser;

fn main() {
    let lex = lexer::Lexer::new("42 + 13");
    parser::parse(lex).unwrap();
}
