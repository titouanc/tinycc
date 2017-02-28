#![feature(plugin)]
#![plugin(plex)]

pub mod tokens;
pub mod lexer;
pub mod ast;
pub mod parser;

fn main() {
    parser::parse_string(" 3 + 9 - 4", true);
}
