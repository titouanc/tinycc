#![feature(plugin)]
#![plugin(plex)]

pub mod tokens;
pub mod lexer;
pub mod ast;
pub mod parser;
pub mod staticanalysis;

use std::io::{self, Read};

fn main() {
    let mut buffer = String::new();
    if let Ok(_) = io::stdin().read_to_string(&mut buffer) {
        let res = parser::parse_string(buffer.as_str(), true);
        println!("{:?}", res);
    }    
}
