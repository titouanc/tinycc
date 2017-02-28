#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Identifiers
    Name(String),

    // Literals
    Integer(i64),
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
    Lpar  ,   Rpar  ,
    Lbrace,   Rbrace,
    Lbrack,   RBrack,

    // Separators
    Comma,
    Semicol,
    Whitespace,
}
