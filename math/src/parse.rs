use super::tok::*;

// TODO switch to arena allocator

struct TokenStream<'t> {
    toks: &'t [Token],
    idx: usize,
}

fn infix_info(c: char) -> Option<(u8, u8, fn(Box<MathTree>, Box<MathTree>) -> MathTree)> {
    Some(match c {
        '+' => (50, 60, MathTree::Plus),
        '-' => (50, 60, MathTree::Sub),
        '*' => (70, 80, MathTree::Mul),
        _ => return None,
    })
}

macro_rules! matchxtract {
    ($val:expr, $p:pat => $e:expr) => {
        match $val {
            $p => Some($e),
            _ => None,
        }
    };
}

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ParseError {
    ExpectingPrimary,
}

impl<'t> TokenStream<'t> {
    fn peek(&self) -> Option<&'t Token> {
        self.toks.get(self.idx)
    }

    fn advance(&mut self) -> Option<&Token> {
        self.idx += 1;
        self.toks.get(self.idx - 1)
    }

    fn sniff<F, Ret>(&mut self, cond: F) -> Option<Ret>
    where
        F: FnOnce(&'t Token) -> Option<Ret>,
    {
        self.peek().and_then(cond)
    }

    fn eat<F, Ret>(&mut self, cond: F) -> Option<Ret>
    where
        F: FnOnce(&'t Token) -> Option<Ret>,
    {
        self.sniff(cond).inspect(|_| {
            self.advance();
        })
    }

    fn prim(&mut self) -> Option<Result<MathTree>> {
        if let Some(res) = self.eat(|t| {
            Some(match t {
                Token::Frac(top, bot) => parse(top)
                    .and_then(|top| parse(bot).map(|bot| (top, bot)))
                    .map(|(top, bot)| MathTree::Div(Box::new(top), Box::new(bot))),
                &Token::Glyph(c) if c.is_alphabetic() => Ok(MathTree::Var(c)),
                _ => return None,
            })
        }) {
            return Some(res);
        }

        let mut s = String::new();
        while let Some(c) = self.eat(|t| matchxtract!(t, &Token::Glyph(d @ ('0'..='9' | '.')) => d))
        {
            s.push(c);
        }
        s.parse::<f64>().ok().map(MathTree::Num).map(Ok)
    }

    fn sniff_op(&mut self) -> Option<char> {
        self.sniff(|t| matchxtract!(t, &Token::Glyph(c @ ('+' | '-')) => c))
    }

    fn expr_prec(&mut self, min_prec: u8) -> Result<MathTree> {
        // TODO prefix and postfix operators
        let mut lhs = self.prim().ok_or(ParseError::ExpectingPrimary)??;
        while self.peek().is_some() {
            if let Some(op) = self.sniff_op() {
                if let Some((lp, rp, cons)) = infix_info(op) {
                    if lp >= min_prec {
                        self.advance();
                        let rhs = self.expr_prec(rp)?;
                        lhs = cons(Box::new(lhs), Box::new(rhs));
                        continue;
                    }
                }
                break;
            } else {
                // (term) (sup) is power-of
                if let Some(k) = self.eat(|t| matchxtract!(t, Token::Sup(k) => k)) {
                    let rhs = parse(k)?;
                    lhs = MathTree::Pow(Box::new(lhs), Box::new(rhs));
                    continue;
                }
                // (term) (term) is multiplication
                let rhs = self.prim().ok_or(ParseError::ExpectingPrimary)??;
                lhs = MathTree::Mul(Box::new(lhs), Box::new(rhs));
                continue;
            }
        }
        Ok(lhs)
    }

    fn entry(&mut self) -> Result<MathTree> {
        self.expr_prec(0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MathTree {
    Plus(Box<MathTree>, Box<MathTree>),
    Mul(Box<MathTree>, Box<MathTree>),
    Sub(Box<MathTree>, Box<MathTree>),
    Div(Box<MathTree>, Box<MathTree>),
    Pow(Box<MathTree>, Box<MathTree>),
    Num(f64),
    Var(char),
}

pub fn parse(toks: &[Token]) -> Result<MathTree> {
    TokenStream { idx: 0, toks }.entry()
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::keysim;

    macro_rules! b {
        ($p:ident, $($e:expr),*) => {
            MathTree::$p($(Box::new($e)),*)
        }
    }

    #[test]
    fn basic() {
        use MathTree::*;
        use Token::*;
        assert_eq!(
            parse(&[Glyph('a'), Glyph('+'), Glyph('b')]).unwrap(),
            Plus(Box::new(Var('a')), Box::new(Var('b')))
        );
        assert_eq!(
            parse(&keysim!("a+bc")).unwrap(),
            Plus(
                Box::new(Var('a')),
                Box::new(Mul(Box::new(Var('b')), Box::new(Var('c'))))
            )
        );
        assert_eq!(
            parse(&keysim!("ab+c")).unwrap(),
            Plus(
                Box::new(Mul(Box::new(Var('a')), Box::new(Var('b')))),
                Box::new(Var('c'))
            )
        );
        assert_eq!(
            parse(&keysim!("3+a^b")).unwrap(),
            b!(Plus, Num(3.), b!(Pow, Var('a'), Var('b')))
        );
        assert_eq!(
            parse(&keysim!("18591+581")).unwrap(),
            b!(Plus, Num(18591.), Num(581.))
        );
    }
}
