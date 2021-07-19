use std::convert::{TryFrom, TryInto};
use std::ops::Range;

use super::orefs::Handle;
use super::string;

#[derive(Debug, Clone)]
pub struct Located<T> {
    pub value: T,
    pub filename: Handle<string::String>,
    pub offset: usize
}

#[derive(Debug, Clone)]
pub struct Spanning<T> {
    pub value: T,
    pub filename: Handle<string::String>,
    pub span: Range<usize>
}

impl<T> Spanning<T> {
    pub fn here<U>(&self, value: U) -> Spanning<U> {
        Spanning {value, filename: self.filename.clone(), span: self.span.clone() }
    }
}

// ---

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token<'a> {
    Newline, Indent, Dedent,

    If, Else, Colon,

    Assign,
    Plus, Minus, Star, Slash,
    Lt, Le, Eq, Ne, Gt, Ge,

    Identifier(&'a str),
    Integer(isize),
    True, False
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenTag {
    Newline, Indent, Dedent,

    If, Else, Colon,

    Assign,
    Plus, Minus, Star, Slash,
    Lt, Le, Eq, Ne, Gt, Ge,

    Identifier,
    Integer,
    True, False
}

impl<'a> Token<'a> {
    pub fn tag(&self) -> TokenTag {
        match *self {
            Token::Newline => TokenTag::Newline,
            Token::Indent => TokenTag::Indent,
            Token::Dedent => TokenTag::Dedent,

            Token::If => TokenTag::If,
            Token::Else => TokenTag::Else,
            Token::Colon => TokenTag::Colon,

            Token::Assign => TokenTag::Assign,
            Token::Plus => TokenTag::Plus,
            Token::Minus => TokenTag::Minus,
            Token::Star => TokenTag::Star,
            Token::Slash => TokenTag::Slash,
            Token::Lt => TokenTag::Lt,
            Token::Le => TokenTag::Le,
            Token::Eq => TokenTag::Eq,
            Token::Ne => TokenTag::Ne,
            Token::Gt => TokenTag::Gt,
            Token::Ge => TokenTag::Ge,

            Token::Identifier(_) => TokenTag::Identifier,
            Token::Integer(_) => TokenTag::Integer,
            Token::True => TokenTag::True,
            Token::False => TokenTag::False
        }
    }
}

#[derive(Debug, Clone)]
pub enum Error {
    UnexpectedChar(char),
    DedentDepth(usize)
}

pub type LexResult<'a> = Result<Spanning<Token<'a>>, Located<Error>>;

// ---

struct LookaheadlessLexer<'a> {
    chars: &'a str,
    index: usize,
    filename: Handle<string::String>
}

impl<'a> LookaheadlessLexer<'a> {
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
        while self.peek_char().map_or(false, |c| c.is_whitespace() && c != '\r' && c != '\n') {
            let _ = self.pop_char();
        }

        match self.peek_char() {
            Some('\n') => { // TODO: \r\n (Windows), \r (Ye Olde Mac), \\\n (escape)
                let start_index = self.index;
                let _ = self.pop_char();
                Some(Ok(self.spanning(Token::Newline, start_index..self.index)))
            },

            Some(':') => {
                let start_index = self.index;
                let _ = self.pop_char();
                Some(Ok(self.spanning(Token::Colon, start_index..self.index)))
            },

            Some('=') => {
                let start_index = self.index;
                let _ = self.pop_char();
                match self.peek_char() {
                    Some('=') => {
                        let _ = self.pop_char();
                        Some(Ok(self.spanning(Token::Eq, start_index..self.index)))
                    },
                    _ =>
                        Some(Ok(self.spanning(Token::Assign, start_index..self.index)))
                }
            },

            Some(c @ '!') => {
                let start_index = self.index;
                let _ = self.pop_char();
                match self.peek_char() {
                    Some('=') => {
                        let _ = self.pop_char();
                        Some(Ok(self.spanning(Token::Ne, start_index..self.index)))
                    },
                    _ => Some(Err(self.here(Error::UnexpectedChar(c))))
                }
            },

            Some('<') => {
                let start_index = self.index;
                let _ = self.pop_char();
                match self.peek_char() {
                    Some('=') => {
                        let _ = self.pop_char();
                        Some(Ok(self.spanning(Token::Le, start_index..self.index)))
                    },
                    _ => 
                        Some(Ok(self.spanning(Token::Lt, start_index..self.index)))
                }
            },
            Some('>') => {
                let start_index = self.index;
                let _ = self.pop_char();
                match self.peek_char() {
                    Some('=') => {
                        let _ = self.pop_char();
                        Some(Ok(self.spanning(Token::Ge, start_index..self.index)))
                    },
                    _ => 
                        Some(Ok(self.spanning(Token::Gt, start_index..self.index)))
                }
            },

            Some('+') => {
                let start_index = self.index;
                let _ = self.pop_char();
                Some(Ok(self.spanning(Token::Plus, start_index..self.index)))
            },
            Some('-') => {
                let start_index = self.index;
                let _ = self.pop_char();
                Some(Ok(self.spanning(Token::Minus, start_index..self.index)))
            },
            Some('*') => {
                let start_index = self.index;
                let _ = self.pop_char();
                Some(Ok(self.spanning(Token::Star, start_index..self.index)))
            },
            Some('/') => {
                let start_index = self.index;
                let _ = self.pop_char();
                Some(Ok(self.spanning(Token::Slash, start_index..self.index)))
            },

            Some(c) if c.is_alphabetic() => { // [:alpha]+ = [:alpha:] [:alpha:]*
                let start_index = self.index;

                let _ = self.pop_char(); // [:alpha:]
                while self.peek_char().map_or(false, char::is_alphabetic) { // [:alpha:]*
                    let _ = self.pop_char();
                }

                let span = start_index..self.index;
                let name = &self.chars[span.clone()];
                match name {
                    "if" => Some(Ok(self.spanning(Token::If, span))),
                    "else" => Some(Ok(self.spanning(Token::Else, span))),
                    "True" => Some(Ok(self.spanning(Token::True, span))),
                    "False" => Some(Ok(self.spanning(Token::False, span))),
                    _ => Some(Ok(self.spanning(Token::Identifier(name), span)))
                }
            },

            Some(c) if c.is_digit(10) => { // \d+ = \d \d*
                let start_index = self.index;

                let mut n: isize = self.pop_char().unwrap() // \d
                    .to_digit(10).unwrap()
                    .try_into().unwrap();
                while let Some(d) = self.peek_char().and_then(|c| c.to_digit(10)) { // \d*
                    let _ = self.pop_char();
                    n = 10*n + isize::try_from(d).unwrap();
                }

                Some(Ok(self.spanning(Token::Integer(n), start_index..self.index)))
            },

            Some(c) => Some(Err(self.here(Error::UnexpectedChar(c)))),

            None => None // EOF
        }
    }
}

// ---

enum State {
    Startline,
    Indenting(usize),
    Dedenting(usize),
    Intraline,
    Eof
}

struct IndentingLexer<'a> {
    tokens: LookaheadlessLexer<'a>,
    state: State,
    indent_levels: Vec<usize>
}

impl<'a> Iterator for IndentingLexer<'a> {
    type Item = LexResult<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        use std::cmp::Ordering::*;
        use State::*;

        'next_state: loop {
            match self.state {
                Startline => {
                    let mut indent = 0;
                    loop { // TODO: \r\n?, \\
                        match self.tokens.peek_char() {
                            Some('\n') => {
                                let _ = self.tokens.pop_char();
                                indent = 0;
                            },
                            Some(c) if c.is_whitespace() => {
                                let _ = self.tokens.pop_char();
                                indent += 1;
                            },
                            Some(_) => break,
                            None => {
                                self.state = Eof;
                                continue 'next_state;
                            }
                        }
                    }

                    self.state = match indent.cmp(self.indent_levels.last().unwrap_or(&0)) {
                        Greater => Indenting(indent),
                        Less => Dedenting(indent),
                        Equal => Intraline
                    };
                },

                Indenting(indent) =>
                    match indent.cmp(self.indent_levels.last().unwrap_or(&0)) {
                        Greater => {
                            self.indent_levels.push(indent);
                            let start_index = self.tokens.index - indent;
                            return Some(Ok(self.tokens.spanning(Token::Indent, start_index..self.tokens.index)));
                        },
                        Equal => self.state = Intraline,
                        Less => unreachable!()
                    },

                Dedenting(indent) =>
                    match indent.cmp(self.indent_levels.last().unwrap_or(&0)) {
                        Less => {
                            let _ = self.indent_levels.pop();
                            let start_index = self.tokens.index - indent;
                            return Some(Ok(self.tokens.spanning(Token::Dedent, start_index..self.tokens.index)));
                        },
                        Equal => self.state = Intraline,
                        Greater => return Some(Err(self.tokens.here(Error::DedentDepth(indent))))
                    },

                Intraline =>
                    match self.tokens.next() {
                        Some(Ok(tok)) if tok.value == Token::Newline => {
                            self.state = Startline;
                            return Some(Ok(tok));
                        },
                        res @ Some(_) => return res,
                        None => self.state = Eof
                    },

                Eof =>
                    return self.indent_levels.pop().map(|_|
                        Ok(self.tokens.spanning(Token::Dedent, self.tokens.index..self.tokens.index))
                    )
            }
        }
    }
}

// ---

pub struct KyyLexer<'a> {
    tokens: IndentingLexer<'a>,
    lookahead: Option<LexResult<'a>>
}

impl<'a> KyyLexer<'a> {
    pub fn new(chars: &'a str, filename: Handle<string::String>) -> Self {
        KyyLexer {
            tokens: IndentingLexer {
                tokens: LookaheadlessLexer {chars, index: 0, filename},
                state: State::Startline,
                indent_levels: Vec::new()
            },
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
            None => self.tokens.tokens.here(value)
        }
    }

    pub fn spanning<T>(&self, value: T, span: Range<usize>) -> Spanning<T> {
        self.tokens.tokens.spanning(value, span)
    }
}

impl<'a> Iterator for KyyLexer<'a> {
    type Item = LexResult<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lookahead.take()
            .or_else(|| self.tokens.next())
    }
}

