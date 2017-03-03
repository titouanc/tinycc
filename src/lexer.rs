use ::tokens::*;
use ::tokens::Token::*;

lexer! {
    fn next_token(text: 'a) -> (Token, &'a str);

    r#"[ \n\t]+"# => (Whitespace, text),
    r#"\+"# => (Plus, text),
    r#"\*"# => (Times, text),
    "-" => (Minus, text),
    "/" => (Divide, text),
    "," => (Comma, text),
    ";" => (Semicol, text),
    "==" => (Equal, text),
    "!=" => (NotEqual, text),
    "<" => (Lt, text),
    "<=" => (Lte, text),
    ">" => (Gt, text),
    ">=" => (Gte, text),
    "=" => (Assign, text),

    r#"\("# => (LParen, text),
    r#"\)"# => (RParen, text),
    r#"\["# => (LBrack, text),
    r#"\]"# => (RBrack, text),
    r#"\{"# => (LBrace, text),
    r#"\}"# => (RBrace, text),

    "while" => (While, text),
    "if" => (If, text),
    "else" => (Else, text),
    "return" => (Return, text),

    "int" => (Int, text),
    "char" => (Char, text),
    "length" => (Length, text),

    r#"'.'"# => (Qchar(text.as_bytes()[1] as char), text),
    r#"[a-zA-Z_][a-zA-Z0-9_]*"# => (Name(text.to_owned()), text),    
    r#"[0-9]+"# => {
        (if let Ok(i) = text.parse(){
            Integer(i)
        } else {
            panic!("Integer {} is out of range", text);
        }, text)
    },
}


pub struct Lexer<'a> {
    original: &'a str,
    remaining: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Lexer<'a> {
        Lexer {original: s, remaining: s}
    }
}


impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, Span);

    fn next(&mut self) -> Option<(Token, Span)> {
        loop {
            let tok = if let Some(tok) = next_token(&mut self.remaining){
                tok
            } else {
                return None
            };

            match tok {
                (Whitespace, _) => {
                    continue;
                }
                (tok, sp) => {
                    return Some((tok, span_in(sp, self.original)));
                }
            }
        }
    }
}

/// Source location information througout the parsing process
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub lo: usize,
    pub hi: usize
}

fn span_in(s: &str, t: &str) -> Span {
    let lo = s.as_ptr() as usize - t.as_ptr() as usize;
    Span {lo: lo, hi: lo+s.len(),}
}



#[cfg(test)]
mod tests {
    use super::*;

    fn test_str(input: &str, tok: Token) {
        let mut lex = Lexer::new(input);
        assert_eq!(lex.next(), Some((tok, Span {lo: 0, hi: input.len()})));
    }

    #[test]
    fn int() {
        test_str("42", Integer(42));
    }

    #[test]
    fn name() {
        test_str("Hello", Name("Hello".to_owned()));
        test_str("hello", Name("hello".to_owned()));
        test_str("_", Name("_".to_owned()));
        test_str("H3ll0", Name("H3ll0".to_owned()));
    }

    #[test]
    fn qchar() {
        test_str("'a'", Qchar('a'));    
    }

    #[test]
    fn operators() {
        test_str("=", Assign);
        test_str("==", Equal);
        test_str("+", Plus);
        test_str("-", Minus);
        test_str("*", Times);
        test_str("/", Divide);
    }

    #[test]
    fn paired(){
        test_str("(", LParen);
        test_str(")", RParen);
        test_str("[", LBrack);
        test_str("]", RBrack);
        test_str("{", LBrace);
        test_str("}", RBrace);
    }

    #[test]
    fn separators(){
        test_str(",", Comma);
        test_str(";", Semicol);
    }

    #[test]
    fn keywords() {
        test_str("int", Int);
        test_str("char", Char);
        test_str("while", While);
        test_str("if", If);
        test_str("else", Else);
        test_str("return", Return);
    }
}
