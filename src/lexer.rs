use std::convert::{TryFrom, TryInto};
use std::sync::Arc;
use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Located<T> {
    pub value: T,
    pub filename: Option<Arc<String>>,
    pub offset: usize
}

#[derive(Debug, Clone)]
pub struct Spanning<T> {
    pub value: T,
    pub filename: Option<Arc<String>>,
    pub span: Range<usize>
}

impl<T> Spanning<T> {
    pub fn here<U>(&self, value: U) -> Spanning<U> {
        Spanning {value, filename: self.filename.clone(), span: self.span.clone() }
    }
}

// ---

#[derive(Debug, Clone)]
pub enum Token<'a> {
    Assign,
    Plus, Minus, Star, Slash,
    Lt, Le, Eq, Ne, Gt, Ge,

    Identifier(&'a str),
    Integer(isize),
    True, False
}

#[derive(Debug, Clone)]
pub enum Error {
    UnexpectedChar(char)
}

pub type LexResult<'a> = Result<Spanning<Token<'a>>, Located<Error>>;

// ---

struct LookaheadlessLexer<'a> {
    chars: &'a str,
    index: usize,
    filename: Option<Arc<String>>
}

impl<'a> LookaheadlessLexer<'a> {
    fn new(chars: &'a str, filename: Option<Arc<String>>) -> Self {
        LookaheadlessLexer {chars, index: 0, filename}
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.get(self.index..)
            .and_then(|cs| cs.chars().next())
    }

    fn pop_char(&mut self) -> Option<char> {
        self.chars.get(self.index..)
            .and_then(|cs| {
                let mut cis = cs.char_indices();
                match cis.next() {
                    Some((_, c)) => {
                        self.index += match cis.next() {
                            Some((c_len, _)) => c_len,
                            None => 1
                        };
                        Some(c)
                    },
                    None => None
                }
            })
    }

    fn here<T>(&self, value: T) -> Located<T> {
        Located {value, offset: self.index, filename: self.filename.clone()}
    }

    fn spanning<T>(&self, value: T, span: Range<usize>) -> Spanning<T> {
        Spanning { value, span, filename: self.filename.clone() }
    }
}

impl<'a> Iterator for LookaheadlessLexer<'a> {
    type Item = LexResult<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.peek_char() {
                Some('=') => {
                    let start_index = self.index;
                    let _ = self.pop_char();
                    match self.peek_char() {
                        Some('=') => {
                            let _ = self.pop_char();
                            return Some(Ok(self.spanning(Token::Eq, start_index..self.index)));
                        },
                        _ =>
                            return Some(Ok(self.spanning(Token::Assign, start_index..self.index)))
                    }
                },

                Some(c @ '!') => {
                    let start_index = self.index;
                    let _ = self.pop_char();
                    match self.peek_char() {
                        Some('=') => {
                            let _ = self.pop_char();
                            return Some(Ok(self.spanning(Token::Ne, start_index..self.index)));
                        },
                        _ => return Some(Err(self.here(Error::UnexpectedChar(c))))
                    }
                },

                Some('<') => {
                    let start_index = self.index;
                    let _ = self.pop_char();
                    match self.peek_char() {
                        Some('=') => {
                            let _ = self.pop_char();
                            return Some(Ok(self.spanning(Token::Le, start_index..self.index)));
                        },
                        _ => 
                            return Some(Ok(self.spanning(Token::Lt, start_index..self.index)))
                    }
                },
                Some('>') => {
                    let start_index = self.index;
                    let _ = self.pop_char();
                    match self.peek_char() {
                        Some('=') => {
                            let _ = self.pop_char();
                            return Some(Ok(self.spanning(Token::Ge, start_index..self.index)));
                        },
                        _ => 
                            return Some(Ok(self.spanning(Token::Gt, start_index..self.index)))
                    }
                },

                Some('+') => {
                    let start_index = self.index;
                    let _ = self.pop_char();
                    return Some(Ok(self.spanning(Token::Plus, start_index..self.index)));
                },
                Some('-') => {
                    let start_index = self.index;
                    let _ = self.pop_char();
                    return Some(Ok(self.spanning(Token::Minus, start_index..self.index)));
                },
                Some('*') => {
                    let start_index = self.index;
                    let _ = self.pop_char();
                    return Some(Ok(self.spanning(Token::Star, start_index..self.index)));
                },
                Some('/') => {
                    let start_index = self.index;
                    let _ = self.pop_char();
                    return Some(Ok(self.spanning(Token::Slash, start_index..self.index)));
                },

                Some(c) if c.is_alphabetic() => { // [:alpha]+ = [:alpha:] [:alpha:]*
                    let start_index = self.index;

                    let _ = self.pop_char(); // [:alpha:]
                    loop { // [:alpha:]*
                        match self.peek_char() {
                            Some(c) if c.is_alphabetic() => { self.pop_char(); },
                            _ => break
                        }
                    }

                    let span = start_index..self.index;
                    let name = &self.chars[span.clone()];
                    match name {
                        "True" => return Some(Ok(self.spanning(Token::True, span))),
                        "False" => return Some(Ok(self.spanning(Token::False, span))),
                        _ => return Some(Ok(self.spanning(Token::Identifier(name), span)))
                    }
                },

                Some(c) if c.is_digit(10) => { // \d+ = \d \d*
                    let start_index = self.index;

                    let mut n: isize = self.pop_char().unwrap() // \d
                        .to_digit(10).unwrap()
                        .try_into().unwrap();
                    loop { // \d*
                        match self.peek_char() {
                            Some(c) => match c.to_digit(10) {
                                Some(d) => {
                                    let _ = self.pop_char();
                                    n = 10*n + isize::try_from(d).unwrap();
                                },
                                None => break
                            },
                            None => break
                        }
                    }

                    return Some(Ok(self.spanning(Token::Integer(n), start_index..self.index)));
                },

                Some(c) if c.is_whitespace() => { self.pop_char(); }, // skip \s (\s* with the outer loop)

                Some(c) => return Some(Err(self.here(Error::UnexpectedChar(c)))),

                None => return None // EOF
            }
        }
    }
}

// ---

pub struct KyyLexer<'a> {
    tokens: LookaheadlessLexer<'a>,
    lookahead: Option<LexResult<'a>>
}

impl<'a> KyyLexer<'a> {
    pub fn new(chars: &'a str, filename: Option<Arc<String>>) -> Self {
        KyyLexer {
            tokens: LookaheadlessLexer::new(chars, filename),
            lookahead: None
        }
    }

    pub fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        if self.lookahead.is_none() {
            self.lookahead = self.tokens.next();
        }
        self.lookahead.as_ref()
    }

    pub fn here<T>(&self, value: T) -> Located<T> {
        match self.lookahead {
            Some(Ok(ref tok)) => Located {
                value,
                filename: tok.filename.clone(),
                offset: tok.span.start
            },
            Some(Err(ref err)) => Located {
                value,
                filename: err.filename.clone(),
                offset: err.offset
            },
            None => self.tokens.here(value)
        }
    }

    pub fn spanning<T>(&self, value: T, span: Range<usize>) -> Spanning<T> {
        self.tokens.spanning(value, span)
    }
}

impl<'a> Iterator for KyyLexer<'a> {
    type Item = LexResult<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lookahead.take()
            .or_else(|| self.tokens.next())
    }
}

