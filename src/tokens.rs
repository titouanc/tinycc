#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Identifiers
    Name(String),

    // Literals
    Integer(i32),
    Qchar(char),

    // Types
    Int,
    Char,
    Length,

    // Control flow
    If,
    Else,
    While,
    Return,

    // Operators
    Assign,
    Plus,
    Minus,
    Times,
    Divide,
    Equal,
    Not,

    // Paired
    LParen,   RParen,
    LBrace,   RBrace,
    LBrack,   RBrack,

    // Separators
    Comma,
    Semicol,
    Whitespace,
}

impl ToString for Token {
    fn to_string(&self) -> String {
        let s = match self.to_owned() {
            Token::Name(s) => return s,
            Token::Integer(i) => return format!("{}", i),
            Token::Qchar(c) => return format!("'{}'", c),
            Token::Int => "int",
            Token::Char => "char",
            Token::Length => "len",
            Token::If => "if",
            Token::Else => "else",
            Token::While => "while",
            Token::Return => "return",
            Token::Assign => "=",
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Times => "*",
            Token::Divide => "/",
            Token::Equal => "==",
            Token::Not => "!",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::LBrack => "[",
            Token::RBrack => "]",
            Token::Comma => ",",
            Token::Semicol => ";",
            Token::Whitespace => " ",
        };
        s.to_string()
    }
}
