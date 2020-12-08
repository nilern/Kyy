use std::sync::Arc;

use super::lexer::{self, Token, TokenTag, KyyLexer, Located};
use super::ast;
use super::mutator::{KyyMutator, KyySizedBytesType};
use super::orefs::Root;
use super::object::Object;
use super::tuple::Tuple;
use super::int::Int;

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
fn atom<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Object>> {
    match peek_some(tokens)? {
        Token::Identifier(chars) => {
            let tok = tokens.next().unwrap()?;
            Ok(Box::new(tokens.spanning(Expr::Var(Arc::new(String::from(chars))), tok.span)))
        },
        Token::Integer(n) => {
            let tok = tokens.next().unwrap()?;
            Ok(Box::new(tokens.spanning(Expr::Const(Int::new(km, Int(n)).as_obj()), tok.span)))
        },
        Token::True => {
            let tok = tokens.next().unwrap()?;
            Ok(Box::new(tokens.spanning(Expr::Const(km.the_true().as_obj()), tok.span)))
        },
        Token::False => {
            let tok = tokens.next().unwrap()?;
            Ok(Box::new(tokens.spanning(Expr::Const(km.the_false().as_obj()), tok.span)))
        },
        tok => Err(tokens.here(Error::UnexpectedToken(tok)))
    }
}

// ::= <multiplicative> ('*' | '/') <atom>
//   | <atom>
fn multiplicative<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Object>> {
    let mut l = atom(km, tokens)?; // <atom>
    loop { // ((STAR | SLASH) <atom>)*
        match peek(tokens)? {
            Some(Token::Star) => {
                let _ = tokens.next();
                let r = atom(km, tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Mul(l, r), span));
            },
            Some(Token::Slash) => {
                let _ = tokens.next();
                let r = atom(km, tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Div(l, r), span));
            },
            _ => return Ok(l)
        }
    }
}

// ::= <additive> ('+' | '-') <multiplicative>
//   | <multiplicative>
fn additive<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Object>> {
    let mut l = multiplicative(km, tokens)?; // <multiplicative>
    loop { // ((PLUS | MINUS) <multiplicative>)*
        match peek(tokens)? {
            Some(Token::Plus) => {
                let _ = tokens.next();
                let r = multiplicative(km, tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Add(l, r), span));
            },
            Some(Token::Minus) => {
                let _ = tokens.next();
                let r = multiplicative(km, tokens)?;
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
fn comparison<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Object>> {
    let mut l = additive(km, tokens)?; // <additive>
    loop { // ((LT | LE | EQ | NE | GT | GE) <additive>)*
        match peek(tokens)? {
            Some(Token::Lt) => {
                let _ = tokens.next();
                let r = additive(km, tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Lt(l, r), span));
            },
            Some(Token::Le) => {
                let _ = tokens.next();
                let r = additive(km, tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Le(l, r), span));
            },
            Some(Token::Eq) => {
                let _ = tokens.next();
                let r = additive(km, tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Eq(l, r), span));
            },
            Some(Token::Ne) => {
                let _ = tokens.next();
                let r = additive(km, tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Ne(l, r), span));
            },
            Some(Token::Gt) => {
                let _ = tokens.next();
                let r = additive(km, tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Gt(l, r), span));
            },
            Some(Token::Ge) => {
                let _ = tokens.next();
                let r = additive(km, tokens)?;
                let span = l.span.start..r.span.end;
                l = Box::new(tokens.spanning(Expr::Ge(l, r), span));
            },
            _ => return Ok(l)
        }
    }
}

// ::= <additive>
fn expr<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Object>> {
    comparison(km, tokens)
}

// ::= NEWLINE INDENT <stmt>+ DEDENT
fn block<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Tuple>> {
    token(tokens, TokenTag::Newline)?;
    token(tokens, TokenTag::Indent)?;

    let mut stmts = Vec::new();
    stmts.push(stmt(km, tokens)?); // <stmt>
    // <stmt>*
    while let Some(Token::If) | Some(Token::Identifier(_))
        | Some(Token::Integer(_)) | Some(Token::True) | Some(Token::False)
        = peek(tokens)?
    {
        stmts.push(stmt(km, tokens)?);
    }

    token(tokens, TokenTag::Dedent)?;

    Ok(Tuple::new(km, &stmts))
}

fn else_block<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Tuple>> {
    token(tokens, TokenTag::Else)?;
    token(tokens, TokenTag::Colon)?;
    block(km, tokens)
}

// ::= 'if' <expr> ':' <block> ('else' ':' <block>)?
//   | VAR '=' <expr> NEWLINE
//   | <expr> NEWLINE
fn stmt<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Object>> {
    let res = match peek_some(tokens)? {
        Token::If => { // IF
            let _ = tokens.next();
            let condition = expr(km, tokens)?;
            token(tokens, TokenTag::Colon)?;
            let conseq = block(km, tokens)?;
            let alt = match peek(tokens)? {
                Some(Token::Else) => else_block(km, tokens)?,
                _ => Tuple::new(km, &[])
            };
            return Ok(Stmt::If {condition, conseq, alt});
        },

        Token::Identifier(name) => { // IDENTIFIER
            let id = tokens.next().unwrap()?;
            match peek(tokens)? { // ('=' <expr>)?
                Some(Token::Assign) => {
                    let _ = tokens.next();
                    let rvalue = expr(km, tokens)?;
                    Ok(Stmt::Assign(String::from(name), rvalue))
                },
                _ => Ok(Stmt::Expr(Box::new(tokens.spanning(Expr::Var(Arc::new(String::from(name))), id.span))))
            }
        },

        Token::Integer(_) | Token::True | Token::False => Ok(Stmt::Expr(expr(km, tokens)?)),

        tok => Err(tokens.here(Error::UnexpectedToken(tok)))
    };
    token(tokens, TokenTag::Newline)?;
    res
}

// ::= <stmt> EOF
pub fn parse<'a>(km: &mut KyyMutator, mut lexer: KyyLexer<'a>) -> ParseResult<'a, Root<Object>> {
    let stmt = stmt(km, &mut lexer)?;
    match peek(&mut lexer)? {
        None => Ok(stmt),
        Some(tok) => Err(lexer.here(Error::UnexpectedToken(tok)))
    }
}

