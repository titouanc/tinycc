#![feature(plugin)]
#![plugin(plex)]

pub mod tokens;
pub mod lexer;
pub mod ast;
pub mod parser;

use std::io::{self, Read};

fn main() {
    let mut buffer = String::new();
    if let Ok(_) = io::stdin().read_to_string(&mut buffer) {
        let tokens = lexer::Lexer::new(buffer.as_str());
        for (t, _) in tokens {
            print!("{:?} ", t);
        }
        println!("");
        parser::parse_string(buffer.as_str(), true);
    }    
}
