#[derive(Debug, Clone)]
pub enum Token {
    Integer(i64),
    Plus,
    Whitespace,
}

lexer! {
    fn next_token(text: 'a) -> (Token, &'a str);

    r#"\+"# => (Token::Plus, text),
    r#"[0-9]+"# => {
        (if let Ok(i) = text.parse(){
            Token::Integer(i)
        } else {
            panic!("Integer {} is out of range", text);
        }, text)
    },
    r#" "# => (Token::Whitespace, text),
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
                (Token::Whitespace, _) => {
                    continue;
                }
                (tok, sp) => {
                    return Some((tok, span_in(sp, self.original)));
                }
            }
        }
    }
}


#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub lo: usize,
    pub hi: usize
}

fn span_in(s: &str, t: &str) -> Span {
    let lo = s.as_ptr() as usize - t.as_ptr() as usize;
    Span {lo: lo, hi: lo+s.len(),}
}