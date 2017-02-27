use ::lexer::*;
use ::lexer::Token::*;
use ::ast::*;

parser! {
    fn parse_(Token, Span);

    // Combine 2 spans
    (a, b) {Span {lo: a.lo, hi: b.hi,}}

    term: Expr {
        atom[l] Plus atom[r] => Expr {
            span: span!(),
            node: Node::Sum(Box::new(l), Box::new(r)),
        }
    }

    atom: Expr {
        Integer(i) => Expr {span: span!(), node: Node::Lit(i),}
    }
}

pub fn parse<I: Iterator<Item=(Token, Span)>>(i: I) ->
             Result<Expr, (Option<(Token, Span)>, &'static str)> {
    parse_(i)
}