use std::sync::Arc;
use std::ops::Range;

#[derive(Debug)]
pub struct Located<T> {
    pub value: T,
    pub filename: Option<Arc<String>>,
    pub offset: usize
}

#[derive(Debug)]
pub struct Spanning<T> {
    pub value: T,
    pub filename: Option<Arc<String>>,
    pub span: Range<usize>
}

#[derive(Debug)]
pub enum SyntaxError {
    UnexpectedChar(char)
}

#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    Operator,
    Integer
}

#[derive(Debug)]
pub struct Token<'a> {
    pub name: TokenType,
    pub lexeme: &'a str
}

type SyntaxResult<'a> = Result<Spanning<Token<'a>>, Located<SyntaxError>>;

pub struct KyyLexer<'a> {
    chars: &'a str,
    index: usize,
    filename: Option<Arc<String>>
}

impl<'a> KyyLexer<'a> {
    pub fn new(chars: &'a str, filename: Option<Arc<String>>) -> Self {
        KyyLexer {chars, index: 0, filename}
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index..)
            .and_then(|cs| cs.chars().next())
    }

    fn pop(&mut self) -> Option<char> {
        self.chars.get(self.index..)
            .and_then(|cs| {
                let mut cis = cs.char_indices();
                match cis.next() {
                    Some((_, c)) => {
                        match cis.next() {
                            Some((c_len, _)) => self.index += c_len,
                            None => self.index += 1
                        }
                        Some(c)
                    },
                    None => None
                }
            })
    }

    fn here<T>(&self, value: T) -> Located<T> {
        Located {value, offset: self.index, filename: self.filename.clone()}
    }

    fn token(&self, name: TokenType, span: Range<usize>) -> Spanning<Token<'a>> {
        Spanning {
            filename: self.filename.clone(),
            span: span.clone(),
            value: Token {name, lexeme: &self.chars[span]},
        }
    }
}

impl<'a> Iterator for KyyLexer<'a> {
    type Item = SyntaxResult<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.peek() {
                Some(c) if c.is_whitespace() => { self.pop(); }, // skip \s (\s* with the outer loop)

                Some(c) if c.is_digit(10) => { // \d+ = \d \d*
                    let start_index = self.index;

                    self.pop(); // \d

                    loop { // \d*
                        match self.peek() {
                            Some(c) if c.is_digit(10) => { self.pop(); },
                            Some(_) | None =>
                                return Some (Ok(self.token(TokenType::Integer, start_index..self.index)))
                        }
                    }
                },
                
                Some('+') | Some('-') | Some('*') | Some('/') => { // [+\-*/]
                    let start_index = self.index;

                    self.pop();

                    return Some(Ok(self.token(TokenType::Operator, start_index..self.index)))
                },

                Some(c) => return Some(Err(self.here(SyntaxError::UnexpectedChar(c)))),

                None => return None // EOF
            }
        }
    }
}

