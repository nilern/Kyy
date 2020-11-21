use std::sync::Arc;

use super::lexer::{self, Token, TokenTag, KyyLexer, Spanning, Located};

#[derive(Debug)]
pub enum Const {
    Int(isize),
    Bool(bool),
}

#[derive(Debug)]
pub enum Expr {
    Add(ExprRef, ExprRef),
    Sub(ExprRef, ExprRef),
    Mul(ExprRef, ExprRef),
    Div(ExprRef, ExprRef), // FIXME: __truediv__ vs. __floordiv__

    Lt(ExprRef, ExprRef),
    Le(ExprRef, ExprRef),
    Eq(ExprRef, ExprRef),
    Ne(ExprRef, ExprRef),
    Gt(ExprRef, ExprRef),
    Ge(ExprRef, ExprRef),

    Var(Arc<String>),
    Const(Const)
}

pub type ExprRef = Box<Spanning<Expr>>;

#[derive(Debug)]
pub enum Stmt {
    If {
        condition: ExprRef,
        conseq: Vec<Stmt>,
        alt: Vec<Stmt>
    },

    Assign(String, ExprRef),
    Expr(ExprRef)
}

// ---

#[derive(Debug)]
pub enum Error<'a> {
    UnexpectedToken(Token<'a>),
    Expected(TokenTag, Token<'a>),
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

fn token<'a>(tokens: &mut KyyLexer<'a>, tag: TokenTag) -> ParseResult<'a, Token<'a>> {
    let tok = peek_some(tokens)?;
    if tok.tag() == tag {
        let _ = tokens.next();
        Ok(tok)
    } else {
        Err(tokens.here(Error::Expected(tag, tok)))
    }
}


// ---

// ::= IDENTIFIER
//   | INTEGER
fn atom<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<'a, ExprRef> {
    match peek_some(tokens)? {
        Token::Identifier(chars) => {
            let tok = tokens.next().unwrap()?;
            Ok(Box::new(tokens.spanning(Expr::Var(Arc::new(String::from(chars))), tok.span)))
        },
        Token::Integer(n) => {
            let tok = tokens.next().unwrap()?;
            Ok(Box::new(tokens.spanning(Expr::Const(Const::Int(n)), tok.span)))
        },
        Token::True => {
            let tok = tokens.next().unwrap()?;
            Ok(Box::new(tokens.spanning(Expr::Const(Const::Bool(true)), tok.span)))
        },
        Token::False => {
            let tok = tokens.next().unwrap()?;
            Ok(Box::new(tokens.spanning(Expr::Const(Const::Bool(false)), tok.span)))
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

// TODO: Chained comparisons (`a < b >= c` = `a < b and b >= c`)
// ::= <comparison> ('<', '<=', '==', '!=', '>', '>=') <additive>
//   | <additive>
fn comparison<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<'a, ExprRef> {
    let mut l = additive(tokens)?; // <additive>
    loop { // ((LT | LE | EQ | NE | GT | GE) <additive>)*
        match peek(tokens)? {
            Some(Token::Lt) => {
                let _ = tokens.next();
                let r = additive(tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Lt(l, r), span));
            },
            Some(Token::Le) => {
                let _ = tokens.next();
                let r = additive(tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Le(l, r), span));
            },
            Some(Token::Eq) => {
                let _ = tokens.next();
                let r = additive(tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Eq(l, r), span));
            },
            Some(Token::Ne) => {
                let _ = tokens.next();
                let r = additive(tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Ne(l, r), span));
            },
            Some(Token::Gt) => {
                let _ = tokens.next();
                let r = additive(tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Gt(l, r), span));
            },
            Some(Token::Ge) => {
                let _ = tokens.next();
                let r = additive(tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Ge(l, r), span));
            },
            _ => return Ok(l)
        }
    }
}

// ::= <additive>
fn expr<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<'a, ExprRef> { comparison(tokens) }

// ::= NEWLINE INDENT <stmt>+ DEDENT
fn block<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Vec<Stmt>> {
    token(tokens, TokenTag::Newline)?;
    token(tokens, TokenTag::Indent)?;

    let mut stmts = Vec::new();
    stmts.push(stmt(tokens)?); // <stmt>
    // <stmt>*
    while let Some(Token::If) | Some(Token::Identifier(_))
        | Some(Token::Integer(_)) | Some(Token::True) | Some(Token::False)
        = peek(tokens)?
    {
        stmts.push(stmt(tokens)?);
    }

    token(tokens, TokenTag::Dedent)?;

    Ok(stmts)
}

fn else_block<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Vec<Stmt>> {
    token(tokens, TokenTag::Else)?;
    token(tokens, TokenTag::Colon)?;
    block(tokens)
}

// ::= 'if' <expr> ':' <block> ('else' ':' <block>)?
//   | VAR '=' <expr> NEWLINE
//   | <expr> NEWLINE
fn stmt<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Stmt> {
    let res = match peek_some(tokens)? {
        Token::If => { // IF
            let _ = tokens.next();
            let condition = expr(tokens)?;
            token(tokens, TokenTag::Colon)?;
            let conseq = block(tokens)?;
            let alt = match peek(tokens)? {
                Some(Token::Else) => else_block(tokens)?,
                _ => Vec::new()
            };
            return Ok(Stmt::If {condition, conseq, alt});
        },

        Token::Identifier(name) => { // IDENTIFIER
            let id = tokens.next().unwrap()?;
            match peek(tokens)? { // ('=' <expr>)?
                Some(Token::Assign) => {
                    let _ = tokens.next();
                    let rvalue = expr(tokens)?;
                    Ok(Stmt::Assign(String::from(name), rvalue))
                },
                _ => Ok(Stmt::Expr(Box::new(tokens.spanning(Expr::Var(Arc::new(String::from(name))), id.span))))
            }
        },

        Token::Integer(_) | Token::True | Token::False => Ok(Stmt::Expr(expr(tokens)?)),

        tok => Err(tokens.here(Error::UnexpectedToken(tok)))
    };
    token(tokens, TokenTag::Newline)?;
    res
}

// ::= <stmt> EOF
pub fn parse<'a>(mut lexer: KyyLexer<'a>) -> ParseResult<'a, Stmt> {
    let stmt = stmt(&mut lexer)?;
    match peek(&mut lexer)? {
        None => Ok(stmt),
        Some(tok) => Err(lexer.here(Error::UnexpectedToken(tok)))
    }
}

