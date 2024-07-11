use super::tok::*;

fn serialize(toks: &[Token], out: &mut String) {
    for tok in toks {
        match tok {
            &Token::Glyph(r) => {
                if r == '\\' {
                    *out += "\\backslash";
                } else {
                    out.push(r);
                }
            }
            Token::Sup(kids) => {
                *out += "^{";
                serialize(kids, out);
                out.push('}');
            }
            Token::Frac(c1, c2) => {
                *out += "\\frac{";
                serialize(c1, out);
                *out += "}{";
                serialize(c2, out);
                *out += "}";
            },
            Token::Paren(c) => {
                *out += "\\left(";
                serialize(c, out);
                *out += "\\right)";
            }
        }
    }
}

pub fn to_tex(toks: &[Token]) -> String {
    let mut s = String::new();
    serialize(toks, &mut s);
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        use Token::*;
        let toks = vec![
            Glyph('-'),
            Glyph('r'),
            Glyph('e'),
            Sup(vec![
                Glyph('2'),
                Sup(vec![Glyph('2')]),
                Glyph('i'),
                Glyph('t'),
            ]),
            Glyph('+'),
            Glyph('c'),
        ];

        let res = to_tex(&toks);
        assert_eq!(res, "-re^{2^{2}it}+c");
    }
}
