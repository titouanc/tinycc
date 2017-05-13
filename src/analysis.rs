use ::ast;
use std::collections::HashMap;
use ::scope::{Scope, Variable, Function};

type PResult = Result<(),String>;

fn error(txt: &str) -> PResult {
    Err(txt.to_string())
}
