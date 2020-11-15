use super::lexer::{self, Token, KyyLexer, Spanning, Located};

type Const = isize;

#[derive(Debug)]
pub enum Expr {
    Add(ExprRef, ExprRef),
    Sub(ExprRef, ExprRef),
    Mul(ExprRef, ExprRef),
    Div(ExprRef, ExprRef), // FIXME: __truediv__ vs. __floordiv__

    Var(String),
    Const(Const)
}

pub type ExprRef = Box<Spanning<Expr>>;

#[derive(Debug)]
pub enum Stmt {
    Assign(String, ExprRef),
    Expr(ExprRef)
}

// ---

#[derive(Debug)]
pub enum Error<'a> {
    UnexpectedToken(Token<'a>),
    Eof,
    Lex(lexer::Error)
}

impl<'a> From<Located<lexer::Error>> for Located<Error<'a>> {
    fn from(err: Located<lexer::Error>) -> Self {
        Located {
            value: Error::Lex(err.value),
            filename: err.filename,
            offset: err.offset
        }
    }
}

type ParseResult<'a, T> = Result<T, Located<Error<'a>>>;

// ---

fn peek<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Option<Token<'a>>> {
    match tokens.peek() {
        Some(res) => Ok(Some(res.clone()?.value)),
        None => Ok(None)
    }
}

fn peek_some<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Token<'a>> {
    match peek(tokens)? {
        Some(tok) => Ok(tok),
        None => Err(tokens.here(Error::Eof))
    }
}

// ---

// ::= IDENTIFIER
//   | INTEGER
fn atom<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<'a, ExprRef> {
    match peek_some(tokens)? {
        Token::Identifier(chars) => {
            let tok = tokens.next().unwrap()?;
            Ok(Box::new(tokens.spanning(Expr::Var(String::from(chars)), tok.span)))
        },
        Token::Integer(n) => {
            let tok = tokens.next().unwrap()?;
            Ok(Box::new(tokens.spanning(Expr::Const(n), tok.span)))
        },
        tok => Err(tokens.here(Error::UnexpectedToken(tok)))
    }
}

// ::= <multiplicative> ('*' | '/') <atom>
//   | <atom>
fn multiplicative<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<'a, ExprRef> {
    let mut l = atom(tokens)?; // <atom>
    loop { // ((STAR | SLASH) <atom>)*
        match peek(tokens)? {
            Some(Token::Star) => {
                let _ = tokens.next();
                let r = atom(tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Mul(l, r), span));
            },
            Some(Token::Slash) => {
                let _ = tokens.next();
                let r = atom(tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Div(l, r), span));
            },
            _ => return Ok(l)
        }
    }
}

// ::= <additive> ('+' | '-') <multiplicative>
//   | <multiplicative>
fn additive<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<'a, ExprRef> {
    let mut l = multiplicative(tokens)?; // <multiplicative>
    loop { // ((PLUS | MINUS) <multiplicative>)*
        match peek(tokens)? {
            Some(Token::Plus) => {
                let _ = tokens.next();
                let r = multiplicative(tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Add(l, r), span));
            },
            Some(Token::Minus) => {
                let _ = tokens.next();
                let r = multiplicative(tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Sub(l, r), span));
            },
            _ => return Ok(l)
        }
    }
}

// ::= <additive>
fn expr<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<'a, ExprRef> { additive(tokens) }

// ::= VAR '=' <expr>
//   | <expr>
fn stmt<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Stmt> {
    match peek_some(tokens)? {
        Token::Identifier(name) => { // IDENTIFIER
            let id = tokens.next().unwrap()?;
            match peek(tokens)? { // ('=' <expr>)?
                Some(Token::Assign) => {
                    let _ = tokens.next();
                    let rvalue = expr(tokens)?;
                    Ok(Stmt::Assign(String::from(name), rvalue))
                },
                _ => Ok(Stmt::Expr(Box::new(tokens.spanning(Expr::Var(String::from(name)), id.span))))
            }
        },
        Token::Integer(_) => Ok(Stmt::Expr(expr(tokens)?)),
        tok => Err(tokens.here(Error::UnexpectedToken(tok)))
    }
}

// ::= <stmt> EOF
pub fn parse<'a>(mut lexer: KyyLexer<'a>) -> ParseResult<'a, Stmt> {
    let stmt = stmt(&mut lexer)?;
    match peek(&mut lexer)? {
        None => Ok(stmt),
        Some(tok) => Err(lexer.here(Error::UnexpectedToken(tok)))
    }
}

