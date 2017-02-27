use lexer::Span;

#[derive(Debug)]
pub struct Expr {
    pub span: Span,
    pub node: Node,
}

#[derive(Debug)]
pub enum Node {
    Lit(i64),
    Sum(Box<Expr>, Box<Expr>),
}