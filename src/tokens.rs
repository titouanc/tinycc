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
    NotEqual,
    Lt,
    Lte,
    Gt,
    Gte,
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
