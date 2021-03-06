extern crate tinycc;

use std::io::{self, Read};

fn main() {
    let mut buffer = String::new();
    if let Ok(_) = io::stdin().read_to_string(&mut buffer) {
        tinycc::compile(buffer.as_str());
    }
}
