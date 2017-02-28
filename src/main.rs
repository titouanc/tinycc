#![feature(plugin)]
#![plugin(plex)]

pub mod tokens;
pub mod lexer;
pub mod ast;
pub mod parser;

fn main() {
    parser::parse_string("char lol; int f(int x, int y){return x + y;}", true);
}
