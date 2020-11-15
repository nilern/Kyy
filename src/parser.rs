use super::lexer::{self, Token, KyyLexer, Spanning, Located};

type Const = isize;

#[derive(Debug)]
pub enum Expr_ {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>), // FIXME: __truediv__ vs. __floordiv__

    Const(Const)
}

type Expr = Spanning<Expr_>;

// ---

#[derive(Debug)]
pub enum Error {
    UnexpectedToken(Token),
    Eof,
    Lex(lexer::Error)
}

impl From<Located<lexer::Error>> for Located<Error> {
    fn from(err: Located<lexer::Error>) -> Self {
        Located {
            value: Error::Lex(err.value),
            filename: err.filename,
            offset: err.offset
        }
    }
}

type ParseResult<T> = Result<T, Located<Error>>;

// ---

fn peek<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<Option<Token>> {
    match tokens.peek() {
        Some(res) => Ok(Some(res.clone()?.value)),
        None => Ok(None)
    }
}

fn peek_some<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<Token> {
    match peek(tokens)? {
        Some(tok) => Ok(tok),
        None => Err(tokens.here(Error::Eof))
    }
}

// ---

// ::= INTEGER
fn atom<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<Expr> {
    match peek_some(tokens)? {
        Token::Integer(n) => { // INTEGER
            let tok = tokens.next().unwrap()?;
            Ok(tokens.spanning(Expr_::Const(n), tok.span))
        },
        tok => Err(tokens.here(Error::UnexpectedToken(tok)))
    }
}

// ::= <multiplicative> (STAR | SLASH) <atom>
//   | <atom>
fn multiplicative<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<Expr> {
    let mut l = atom(tokens)?; // <atom>
    loop { // ((STAR | SLASH) <atom>)*
        match peek(tokens)? {
            Some(Token::Star) => {
                let _ = tokens.next();
                let r = atom(tokens)?;
                let span = l.span.start..r.span.end;
                l = tokens.spanning(Expr_::Mul(Box::new(l), Box::new(r)), span);
            },
            Some(Token::Slash) => {
                let _ = tokens.next();
                let r = atom(tokens)?;
                let span = l.span.start..r.span.end;
                l = tokens.spanning(Expr_::Div(Box::new(l), Box::new(r)), span);
            },
            _ => return Ok(l)
        }
    }
}

// ::= <additive> (PLUS | MINUS) <multiplicative>
//   | <multiplicative>
fn additive<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<Expr> {
    let mut l = multiplicative(tokens)?; // <multiplicative>
    loop { // ((PLUS | MINUS) <multiplicative>)*
        match peek(tokens)? {
            Some(Token::Plus) => {
                let _ = tokens.next();
                let r = multiplicative(tokens)?;
                let span = l.span.start..r.span.end;
                l = tokens.spanning(Expr_::Add(Box::new(l), Box::new(r)), span);
            },
            Some(Token::Minus) => {
                let _ = tokens.next();
                let r = multiplicative(tokens)?;
                let span = l.span.start..r.span.end;
                l = tokens.spanning(Expr_::Sub(Box::new(l), Box::new(r)), span);
            },
            _ => return Ok(l)
        }
    }
}

// ::= <additive>
fn expr<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<Expr> { additive(tokens) }

// ::= <expr> EOF
pub fn parse(mut lexer: KyyLexer) -> ParseResult<Expr> {
    let expr = expr(&mut lexer)?;
    match peek(&mut lexer)? {
        None => Ok(expr),
        Some(tok) => Err(lexer.here(Error::UnexpectedToken(tok)))
    }
}

