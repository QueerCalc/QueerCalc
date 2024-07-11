//! Internal tokenized representation of math formulae. 1:1 with the visual representation.
//! Consists of a tree of tokens with glyphs as leaves.

use std::{ops::Range, ptr::NonNull};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    /// A single, visible glyph on screen, e.g. 'x', '2', or '+'.
    Glyph(char),
    /// A list of tokens that are superscripted, e.g. `[Glyph('x'), Sup([Glyph('2')]]` -> x^2.
    Sup(Vec<Token>),
    /// (Numerator, denominator).
    Frac(Vec<Token>, Vec<Token>),
    /// A parenthesized expression.
    /// This is required to be its own variant,
    /// since parens can't be imbalanced, and their size depends on the inner content.
    Paren(Vec<Token>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenType {
    Glyph,
    Sup,
    Frac,
    Paren,
}

impl From<&Token> for TokenType {
    fn from(value: &Token) -> Self {
        match value {
            Token::Glyph(_) => TokenType::Glyph,
            Token::Sup(_) => TokenType::Sup,
            Token::Frac(_, _) => TokenType::Frac,
            Token::Paren(_) => TokenType::Paren,
        }
    }
}

fn valid_var_name(c: char) -> bool {
    c.is_alphabetic()
}

#[derive(Debug)]
pub struct MathTokenTree {
    top: Box<Vec<Token>>,
    cursor: Cursor,
}

impl MathTokenTree {
    pub fn keypress(&mut self, key: Key) {
        match key {
            Key::Left => self.cursor.move_left(),
            Key::Right => self.cursor.move_right(),
            Key::Down => self.cursor.move_down(),
            Key::Text('^') => self.cursor.insert(Token::Sup(Vec::new())),
            Key::Text('/') => {
                let last_prim = self.cursor.reverse_steal_prim();
                let has_last_prim = !last_prim.is_empty();
                self.cursor.insert(Token::Frac(last_prim, vec![]));
                if has_last_prim {
                    // go to denominator of the fraction
                    self.cursor.move_down();
                }
            }
            Key::Text(c) => self.cursor.insert(Token::Glyph(c)),
        }
    }

    pub fn from_toks(toks: Vec<Token>) -> Self {
        let mut toks = Box::new(toks);
        // SAFETY: err, i think this should guarantee it is kept in place?
        let ptr = Box::as_mut(&mut toks);
        let ptr = NonNull::from(ptr);
        MathTokenTree {
            top: toks,
            cursor: Cursor {
                index: 0,
                top: ptr,
                path: Vec::new(),
            },
        }
    }

    pub fn toks(&self) -> &[Token] {
        &self.top
    }
}

#[derive(Clone, Debug)]
struct Cursor {
    /// A reference to the top-level list of nodes.
    top: NonNull<Vec<Token>>,
    /// Stack of tuples representing how far we are down the tree.
    /// Each element is a tuple of (parent index, parent type, children list).
    ///
    /// SAFETY: All mutability functions only affect the *current* region.
    /// This ensures the parents are not modified.
    path: Vec<(usize, NonNull<Vec<Token>>)>,
    /// Where the cursor is in the current node.
    /// This represents the index of the token to the *right* of the cursor.
    /// ```md
    /// [  Glyph('2'),  Glyph('a'),  Glyph('b')  ]
    ///  ^index 0     ^index 1     ^index 2    ^index 3
    /// ```
    index: usize,
}

impl Cursor {
    /// SAFETY: these pointers always point to valid `Vec`s
    fn current_region(&self) -> &Vec<Token> {
        if let Some(&(_, region)) = self.path.last() {
            unsafe { region.as_ref() }
        } else {
            unsafe { self.top.as_ref() }
        }
    }

    /// SAFETY: these pointers always point to valid `Vec`s
    fn current_region_mut(&mut self) -> &mut Vec<Token> {
        if let Some(&(_, mut region)) = self.path.last() {
            unsafe { region.as_mut() }
        } else {
            unsafe { self.top.as_mut() }
        }
    }

    fn parent(&self) -> Option<&Token> {
        if let Some(&(parent_idx, _)) = self.path.last() {
            let mut ptr = self.top;
            if self.path.len() >= 2 {
                if let Some(&(_, parent_region)) = self.path.get(self.path.len() - 2) {
                    ptr = parent_region;
                }
            }
            let parent_region = unsafe { ptr.as_ref() };
            Some(&parent_region[parent_idx])
        } else {
            None
        }
    }

    /// Jump to the left of the parent.
    fn jump_left_of_parent(&mut self) {
        if let Some(&(parent_idx, _)) = self.path.last() {
            // the parent should be on the right
            self.index = parent_idx;
            self.path.pop();
        }
    }
    /// Jump to the right of the parent.
    fn jump_right_of_parent(&mut self) {
        if let Some(&(parent_idx, _)) = self.path.last() {
            // the parent should be on the left, so `parent_idx+1` should be on the right
            self.index = parent_idx + 1;
            self.path.pop();
        }
    }

    /// Jump to the left bound of the current region.
    fn jump_left_bound(&mut self) {
        self.index = 0;
    }

    /// Jump to the right bound of the current region.
    fn jump_right_bound(&mut self) {
        // note: [last valid index] + 1 is fine here,
        // because that is the index of an element (that would be) to the *right* of the cursor
        self.index = self.current_region().len();
    }

    /// Enter a node, ending up at the *left* of that node's region.
    /// Returns `true` if a node was actually entered.
    fn enter_node(&mut self, idx: usize) -> bool {
        let region = self.current_region_mut();
        if idx >= region.len() {
            // already at the far-right of the region,
            // no node to enter
            return false;
        }
        let ptr;
        match &mut region[idx] {
            Token::Glyph(_) => return false,
            Token::Sup(children) | Token::Frac(children, _) | Token::Paren(children) => {
                ptr = NonNull::from(children);
            }
        }
        self.path.push((idx, ptr));
        self.index = 0;
        return true;
    }

    fn move_left(&mut self) {
        if self.index > 0 {
            let left_idx = self.index - 1;
            if self.enter_node(left_idx) {
                self.jump_right_bound();
            } else {
                // Could not enter node, skip it instead
                self.index = left_idx;
            }
        } else {
            // escape the current node (if any)
            self.jump_left_of_parent();
        }
    }

    fn move_right(&mut self) {
        if self.index < self.current_region().len() {
            if self.enter_node(self.index) {
                self.jump_left_bound();
            } else {
                // Could not enter node, skip it instead
                self.index += 1;
            }
        } else {
            // escape the current node (if any)
            self.jump_right_of_parent();
        }
    }

    fn move_down(&mut self) {
        // move to denominator of fraction, preserving relative index
        if matches!(self.parent(), Some(Token::Frac(_, _))) {
            let old_idx = self.index;
            self.jump_left_of_parent();
            let parent_idx = self.index;
            let Token::Frac(_, bot) = &mut self.current_region_mut()[parent_idx] else {
                unreachable!("earlier checked for Frac")
            };
            let bot_len = bot.len();
            let bot = NonNull::from(bot);
            self.path.push((parent_idx, bot));
            self.index = old_idx.min(bot_len);
        }
    }

    /// Delete node to the left of the cursor.
    /// If no such node exists, move the rest of the nodes in the current
    /// region up one level, then delete the parent.
    fn delete(&mut self) {
        todo!()
    }

    /// Parses a primary expression in reverse. Tries to find
    /// a primary expression *just before* the cursor.
    /// Returns the range of indices in the current region
    /// that contains the primary expression (or an empty range if none).
    ///
    /// (This is used for inputting fractions.)
    fn reverse_parse_prim(&self) -> Range<usize> {
        if self.index == 0 {
            return 0..0;
        }
        let region = self.current_region();
        // if we are consuming the glyphs of a number
        // instead of looking for variable primary expressions
        let mut consuming_number = false;
        let mut rev_idx = self.index - 1;
        loop {
            if consuming_number {
                if !matches!(region[rev_idx], Token::Glyph('0'..='9')) {
                    rev_idx += 1;
                    break;
                }
            } else {
                // TODO this is actually not the same as the MathQuill impl (it joins glyphs).
                // should be rectified
                match region[rev_idx] {
                    Token::Glyph(c) => {
                        if valid_var_name(c) {
                            break;
                        } else if c.is_ascii_digit() || c == '.' {
                            consuming_number = true;
                        }
                    }
                    Token::Paren(_) => break,
                    Token::Frac(_, _) => break,
                    Token::Sup(_) => { /* attached to a prim that is further behind */ }
                }
            }
            if rev_idx == 0 {
                break;
            }
            rev_idx -= 1;
        }
        rev_idx..self.index
    }

    /// Finds the primary expression directly before the cursor (if any),
    /// removes it, and returns it in the provided vector.
    fn reverse_steal_prim(&mut self) -> Vec<Token> {
        let range = self.reverse_parse_prim();
        self.index -= range.len();
        let res = self.current_region_mut().drain(range).collect();
        res
    }

    /// Insert a token at the current cursor position,
    /// moving the cursor to the right once afterward.
    fn insert(&mut self, tok: Token) {
        let index = self.index;
        let region = self.current_region_mut();
        region.insert(index, tok);
        self.move_right();
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Key {
    Left,
    Right,
    Down,
    /// Any 'text' key, i.e. ASCII printables.
    Text(char),
}

pub fn apply_keystroke_seq(t: &mut MathTokenTree, s: &str) {
    for c in s.chars() {
        t.keypress(Key::Text(c));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Token::*;

    #[macro_export]
    macro_rules! keysim {
        ($($e:tt),*) => {
            {
                let mut t = MathTokenTree::from_toks(vec![]);
                $(keysim!(# t $e);)*
                Vec::from(t.toks())
            }
        };
        (# $v:ident $e:literal) => {
            apply_keystroke_seq(&mut $v, $e)
        };
        (# $v:ident $e:ident) => {
            $v.keypress(Key::$e)
        }
    }

    #[test]
    fn frac() {
        assert_eq!(
            keysim!("/3", Down, "4"),
            vec![Frac(vec![Glyph('3')], vec![Glyph('4')])]
        );
        assert_eq!(
            keysim!("3/4"),
            vec![Frac(vec![Glyph('3')], vec![Glyph('4')])]
        );
        assert_eq!(
            keysim!("158^2", Right, "/4"),
            vec![Frac(
                vec![Glyph('1'), Glyph('5'), Glyph('8'), Sup(vec![Glyph('2')])],
                vec![Glyph('4')]
            )]
        );
        assert_eq!(
            keysim!("yx^2", Right, "/14"),
            vec![
                Glyph('y'),
                Frac(
                    vec![Glyph('x'), Sup(vec![Glyph('2')])],
                    vec![Glyph('1'), Glyph('4')]
                )
            ]
        );
        assert_eq!(
            keysim!("x^6/3"),
            vec![
                Glyph('x'),
                Sup(vec![Frac(vec![Glyph('6')], vec![Glyph('3')])])
            ]
        );
        assert_eq!(
            keysim!("3/4/5"),
            vec![Frac(
                vec![Glyph('3')],
                vec![Frac(vec![Glyph('4')], vec![Glyph('5')])]
            )]
        );
    }

    #[test]
    fn basic() {
        let toks = vec![Glyph('e'), Sup(vec![Glyph('i')])];
        let mut f = MathTokenTree::from_toks(toks);
        // |e^{i}
        f.keypress(Key::Text('r')); // r|e^{i}
        f.keypress(Key::Right); // re|^{i}
        f.keypress(Key::Right); // re^{|i}
        f.keypress(Key::Right); // re^{i|}
        f.keypress(Key::Text('t')); // re^{it|}
        assert_eq!(
            f.toks(),
            vec![Glyph('r'), Glyph('e'), Sup(vec![Glyph('i'), Glyph('t')])]
        );
        f.keypress(Key::Left); // re^{i|t}
        f.keypress(Key::Left); // re^{|it}
        f.keypress(Key::Text('2')); // re^{2|it}
        f.keypress(Key::Text('^')); // re^{2^{|}it}
        f.keypress(Key::Text('2')); // re^{2^{2|}it}
        assert_eq!(
            f.toks(),
            vec![
                Glyph('r'),
                Glyph('e'),
                Sup(vec![
                    Glyph('2'),
                    Sup(vec![Glyph('2')]),
                    Glyph('i'),
                    Glyph('t')
                ])
            ]
        );
        f.keypress(Key::Left); // re^{2^{|2}it}
        f.keypress(Key::Left); // re^{2|^{2}it}
        f.keypress(Key::Left); // re^{|2^{2}it}
        f.keypress(Key::Left); // re|^{2^{2}it}
        f.keypress(Key::Left); // r|e^{2^{2}it}
        f.keypress(Key::Left); // |re^{2^{2}it}
        f.keypress(Key::Text('-')); // -|re^{2^{2}it}
        assert_eq!(
            f.toks(),
            vec![
                Glyph('-'),
                Glyph('r'),
                Glyph('e'),
                Sup(vec![
                    Glyph('2'),
                    Sup(vec![Glyph('2')]),
                    Glyph('i'),
                    Glyph('t')
                ]),
            ]
        );
        f.keypress(Key::Right); // -r|e^{2^{2}it}
        f.keypress(Key::Right); // -re|^{2^{2}it}
        f.keypress(Key::Right); // -re^{|2^{2}it}
        f.keypress(Key::Right); // -re^{2|^{2}it}
        f.keypress(Key::Right); // -re^{2^{|2}it}
        f.keypress(Key::Right); // -re^{2^{2|}it}
        f.keypress(Key::Right); // -re^{2^{2}|it}
        f.keypress(Key::Right); // -re^{2^{2}i|t}
        f.keypress(Key::Right); // -re^{2^{2}it|}
        f.keypress(Key::Right); // -re^{2^{2}it}|
        f.keypress(Key::Text('+'));
        f.keypress(Key::Text('c'));
        assert_eq!(
            f.toks(),
            vec![
                Glyph('-'),
                Glyph('r'),
                Glyph('e'),
                Sup(vec![
                    Glyph('2'),
                    Sup(vec![Glyph('2')]),
                    Glyph('i'),
                    Glyph('t')
                ]),
                Glyph('+'),
                Glyph('c'),
            ]
        );
    }
}
