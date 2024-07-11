use super::tok::*;

struct TokenStream<'t> {
    toks: &'t [Token],
    idx: usize,
}

fn infix_info(c: char) -> Option<(u8, u8, BinOp)> {
    use BinOp::*;
    Some(match c {
        '+' => (50, 60, Add),
        '-' => (50, 60, Sub),
        '*' => (70, 80, Mul),
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
    LeftoverGarbage,
}

impl<'t> TokenStream<'t> {
    fn new(toks: &'t [Token]) -> Self {
        Self { toks, idx: 0 }
    }

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

    fn prim(&mut self, out: &mut ExprTree) -> Option<Result<ExprNode>> {
        if let Some(res) = self.eat(|t| -> Option<Result<ExprNode>> {
            Some(match t {
                Token::Frac(top, bot) => {
                    let top = TokenStream::formula_from(top, out);
                    let top = match top {
                        Ok(r) => r,
                        Err(e) => return Some(Err(e)),
                    };
                    let bot = TokenStream::formula_from(bot, out);
                    let bot = match bot {
                        Ok(r) => r,
                        Err(e) => return Some(Err(e)),
                    };
                    Ok(ExprNode::Binary(BinOp::Div, top, bot))
                }
                &Token::Glyph(c) if c.is_alphabetic() => Ok(ExprNode::Var(c)),
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
        s.parse::<f64>().ok().map(ExprNode::Num).map(Ok)
    }

    fn sniff_op(&mut self) -> Option<char> {
        self.sniff(|t| matchxtract!(t, &Token::Glyph(c @ ('+' | '-')) => c))
    }

    fn expr_prec(&mut self, out: &mut ExprTree, min_prec: u8) -> Result<ExprNode> {
        // TODO prefix and postfix operators
        let mut lhs = self.prim(out).ok_or(ParseError::ExpectingPrimary)??;
        while self.peek().is_some() {
            if let Some(op) = self.sniff_op() {
                if let Some((lp, rp, binop)) = infix_info(op) {
                    if lp >= min_prec {
                        self.advance();
                        let rhs = self.expr_prec(out, rp)?;
                        lhs = ExprNode::Binary(binop, out.add(lhs), out.add(rhs));
                        continue;
                    }
                }
                break;
            } else {
                // (term) (sup) is power-of
                if let Some(k) = self.eat(|t| matchxtract!(t, Token::Sup(k) => k)) {
                    let rhs = TokenStream::formula_from(k, out)?;
                    lhs = ExprNode::Binary(BinOp::Pow, out.add(lhs), rhs);
                    continue;
                }
                // (term) (term) is multiplication
                let rhs = self.prim(out).ok_or(ParseError::ExpectingPrimary)??;
                lhs = ExprNode::Binary(BinOp::Mul, out.add(lhs), out.add(rhs));
                continue;
            }
        }
        Ok(lhs)
    }

    fn formula(&mut self, out: &mut ExprTree) -> Result<ExprRef> {
        let expr = self.expr_prec(out, 0)?;
        Ok(out.add(expr))
    }

    fn formula_from(toks: &'t [Token], tree: &mut ExprTree) -> Result<ExprRef> {
        let mut this = TokenStream::new(toks);
        this.formula(tree)
    }

    fn top_level(toks: &'t [Token]) -> Result<MathTopLevel> {
        let mut tree = ExprTree::new();
        let mut this = TokenStream::new(toks);
        let lhs = this.formula(&mut tree)?;
        match this.advance() {
            Some(t) => match t {
                Token::Glyph('=') => {
                    let rhs = this.formula(&mut tree)?;
                    Ok(MathTopLevel {
                        ty: EqType::Equality(lhs, rhs),
                        tree,
                    })
                }
                _ => Err(ParseError::LeftoverGarbage),
            },
            None => Ok(MathTopLevel {
                ty: EqType::Expr(lhs),
                tree,
            }),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Div,
    Mul,
    Pow,
}

#[derive(Debug, Copy, Clone)]
pub enum ExprNode {
    Binary(BinOp, ExprRef, ExprRef),
    Num(f64),
    Var(char),
}

// no mutable references, so Copy/Clone are allowed
#[derive(Debug, Copy, Clone)]
pub struct ExprRef(u32);

/// The storage container for the expression AST.
#[derive(Debug, Clone)]
pub struct ExprTree {
    /// Stores a list of flattened tree nodes.
    nodes: Vec<ExprNode>,
}

impl ExprTree {
    fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn get(&self, r: ExprRef) -> ExprNode {
        self.nodes[r.0 as usize]
    }

    fn add(&mut self, n: ExprNode) -> ExprRef {
        self.nodes.push(n);
        ExprRef(self.nodes.len() as u32 - 1)
    }

    pub fn eq(&self, lhs: ExprRef, rhs: ExprRef) -> bool {
        let lhs = self.get(lhs);
        let rhs = self.get(rhs);
        macro_rules! genmatch {
            {$(plain $w:ident),*; $($t:tt)*} => {
                match lhs {
                    $(ExprNode::$w(a) => if let ExprNode::$w(b) = rhs { a == b } else { false }),*
                    $($t)*
                }
            }
        }
        genmatch! {
            plain Num, plain Var;
            ExprNode::Binary(op1, a1, b1) => 
                if let ExprNode::Binary(op2, a2, b2) = rhs { 
                    op1 == op2 && self.eq(a1, a2) && self.eq(b1, b2) 
                } else { 
                    false 
                }
        }
    }

    pub fn dbg_fmt(&self, r: ExprRef) -> String {
        match self.get(r) {
            ExprNode::Binary(op, a, b) => {
                format!("({op:?} {} {})", self.dbg_fmt(a), self.dbg_fmt(b))
            }
            ExprNode::Num(n) => format!("{n}"),
            ExprNode::Var(v) => format!("{v}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MathTopLevel {
    pub ty: EqType,
    pub tree: ExprTree,
}

#[derive(Debug, Clone)]
pub enum EqType {
    Equality(ExprRef, ExprRef),
    Expr(ExprRef),
}

pub fn parse(toks: &[Token]) -> Result<MathTopLevel> {
    TokenStream::top_level(toks)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::keysim;

    macro_rules! base {
        {$tree:ident [$res:ident] <$p:ident> ($($t:tt)*) ($($s:tt)*) } => {
            base!($tree [r1] $($t)*);
            base!($tree [r2] $($s)*);
            let $res = $tree.add(ExprNode::Binary($p, r1, r2));
        };
        {$tree:ident [$res:ident] $p:ident $n:literal} => {
            let $res = $tree.add(ExprNode::$p($n));
        };
        ($tree:ident, $($t:tt)*) => {
            {
                base!($tree [res] $($t)*);
                res
            }
        }
    }

    macro_rules! tree_assert_eq {
        ($tree:expr, $lhs:expr, $rhs:expr) => {{
            let rhs = $rhs;
            let lhs = $lhs;
            assert!(
                $tree.eq(lhs, rhs),
                "Trees compare unequal.\n LHS: `{}`\nRHS: `{}`\n",
                $tree.dbg_fmt(lhs),
                $tree.dbg_fmt(rhs)
            );
        }};
    }

    fn formula(tree: &mut ExprTree, toks: &[Token]) -> Result<ExprRef> {
        TokenStream::formula_from(toks, tree)
    }

    #[test]
    fn basic() {
        use BinOp::*;
        use ExprNode::*;
        use Token::*;
        let mut t = ExprTree::new();
        let tree = &mut t;
        macro_rules! b { ($($t:tt)*) => { base!(tree,$($t)*) } }
        let b = tree.add(Var('b'));
        let a = tree.add(Var('a'));
        let aplusb = tree.add(Binary(Add, a, b));
        tree_assert_eq!(
            tree,
            formula(tree, &[Glyph('a'), Glyph('+'), Glyph('b')]).unwrap(),
            aplusb
        );
        let c = tree.add(Var('c'));
        let bmulc = tree.add(Binary(Mul, b, c));
        let aplus_bmulc = tree.add(Binary(Add, a, bmulc));
        tree_assert_eq!(tree, formula(tree, &keysim!("a+bc")).unwrap(), aplus_bmulc);
        let amulb = tree.add(Binary(Mul, a, b));
        let amulb_addc = tree.add(Binary(Add, amulb, c));
        tree_assert_eq!(tree, formula(tree, &keysim!("ab+c")).unwrap(), amulb_addc);
        tree_assert_eq!(
            tree,
            formula(tree, &keysim!("3+a^b")).unwrap(),
            b!(<Add> (Num 3.) (<Pow> (Var 'a') (Var 'b')))
        );
        tree_assert_eq!(
            tree,
            formula(tree, &keysim!("18591+581")).unwrap(),
            b!(<Add> (Num 18591.) (Num 581.))
        );
    }
}
