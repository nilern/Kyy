use std::convert::TryInto;

use super::lexer::{self, Token, TokenTag, KyyLexer, Located};
use super::ast::*;
use super::mutator::KyyMutator;
use super::orefs::{Gc, Root};
use super::object::Object;
use super::tuple::Tuple;
use super::string::String;
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
fn atom<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Expr>> {
    match peek_some(tokens)? {
        Token::Identifier(chars) => {
            let tok = tokens.next().unwrap()?;
            Ok(Var::new(km, tok.filename, tok.span, chars).into())
        },
        Token::Integer(n) => {
            let tok = tokens.next().unwrap()?;
            Ok(Const::new(km, tok.filename, tok.span, Int::new(km, n).as_obj()).into())
        },
        Token::True => {
            let tok = tokens.next().unwrap()?;
            Ok(Const::new(km, tok.filename, tok.span, km.the_true().as_obj()).into())
        },
        Token::False => {
            let tok = tokens.next().unwrap()?;
            Ok(Const::new(km, tok.filename, tok.span, km.the_false().as_obj()).into())
        },
        tok => Err(tokens.here(Error::UnexpectedToken(tok)))
    }
}

// ::= <multiplicative> ('*' | '/') <atom>
//   | <atom>
fn multiplicative<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Expr>> {
    let mut l = atom(km, tokens)?; // <atom>
    loop { // ((STAR | SLASH) <atom>)*
        match peek(tokens)? {
            Some(Token::Star) => {
                let _ = tokens.next();
                let r = atom(km, tokens)?;
                l = Mul::new(km, l.filename(km), l, r).into();
            },
            Some(Token::Slash) => {
                let _ = tokens.next();
                let r = atom(km, tokens)?;
                l = Div::new(km, l.filename(km), l, r).into();
            },
            _ => return Ok(l)
        }
    }
}

// ::= <additive> ('+' | '-') <multiplicative>
//   | <multiplicative>
fn additive<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Expr>> {
    let mut l = multiplicative(km, tokens)?; // <multiplicative>
    loop { // ((PLUS | MINUS) <multiplicative>)*
        match peek(tokens)? {
            Some(Token::Plus) => {
                let _ = tokens.next();
                let r = multiplicative(km, tokens)?;
                l = Add::new(km, l.filename(km), l, r).into();
            },
            Some(Token::Minus) => {
                let _ = tokens.next();
                let r = multiplicative(km, tokens)?;
                l = Sub::new(km, l.filename(km), l, r).into();
            },
            _ => return Ok(l)
        }
    }
}

// TODO: Chained comparisons (`a < b >= c` = `a < b and b >= c`)
// ::= <comparison> ('<', '<=', '==', '!=', '>', '>=') <additive>
//   | <additive>
fn comparison<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Expr>> {
    let mut l = additive(km, tokens)?; // <additive>
    loop { // ((LT | LE | EQ | NE | GT | GE) <additive>)*
        match peek(tokens)? {
            Some(Token::Lt) => {
                let _ = tokens.next();
                let r = additive(km, tokens)?;
                l = Lt::new(km, l.filename(km), l, r).into();
            },
            Some(Token::Le) => {
                let _ = tokens.next();
                let r = additive(km, tokens)?;
                l = Le::new(km, l.filename(km), l, r).into();
            },
            Some(Token::Eq) => {
                let _ = tokens.next();
                let r = additive(km, tokens)?;
                l = Eq::new(km, l.filename(km), l, r).into();
            },
            Some(Token::Ne) => {
                let _ = tokens.next();
                let r = additive(km, tokens)?;
                l = Ne::new(km, l.filename(km), l, r).into();
            },
            Some(Token::Gt) => {
                let _ = tokens.next();
                let r = additive(km, tokens)?;
                l = Gt::new(km, l.filename(km), l, r).into();
            },
            Some(Token::Ge) => {
                let _ = tokens.next();
                let r = additive(km, tokens)?;
                l = Ge::new(km, l.filename(km), l, r).into();
            },
            _ => return Ok(l)
        }
    }
}

// ::= <additive>
fn expr<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Expr>> {
    comparison(km, tokens)
}

// ::= NEWLINE INDENT <stmt>+ DEDENT
fn block<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Tuple>> {
    token(tokens, TokenTag::Newline)?;
    token(tokens, TokenTag::Indent)?;

    let mut stmts: Vec<Gc<Object>> = Vec::new();
    stmts.push(stmt(km, tokens)?.oref().as_obj()); // <stmt>
    // <stmt>*
    while let Some(Token::If) | Some(Token::Identifier(_))
        | Some(Token::Integer(_)) | Some(Token::True) | Some(Token::False)
        = peek(tokens)?
    {
        stmts.push(stmt(km, tokens)?.oref().as_obj());
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
fn stmt<'a>(km: &mut KyyMutator, tokens: &mut KyyLexer<'a>) -> ParseResult<'a, Root<Stmt>> {
    let res = match peek_some(tokens)? {
        Token::If => { // IF
            let if_tok = tokens.next().unwrap()?;
            let condition = expr(km, tokens)?;
            token(tokens, TokenTag::Colon)?;
            let conseq = block(km, tokens)?;
            let alt = match peek(tokens)? {
                Some(Token::Else) => else_block(km, tokens)?,
                _ => Tuple::new(km, &[])
            };
            let end = km.root(conseq.slots()[conseq.len() - 1]) // conseq.len() >= 1
                .unchecked_cast::<Stmt>().end(km);
            return Ok(If::new(km, if_tok.filename, if_tok.span.start, end,
                              condition, conseq, alt).into());
        },

        Token::Identifier(name) => { // IDENTIFIER
            let id = tokens.next().unwrap()?;
            match peek(tokens)? { // ('=' <expr>)?
                Some(Token::Assign) => {
                    let lvalue_tok = tokens.next().unwrap()?;
                    let rvalue = expr(km, tokens)?;
                    let span = lvalue_tok.span.start..isize::from(rvalue.end(km)).try_into().unwrap();
                    Ok(Assign::new(km, id.filename, span, String::new(km, name), rvalue).into())
                },
                _ => Ok(ExprStmt::new(km, Var::new(km, id.filename, id.span, name).into()).into())
            }
        },

        Token::Integer(_) | Token::True | Token::False => Ok(ExprStmt::new(km, expr(km, tokens)?).into()),

        tok => Err(tokens.here(Error::UnexpectedToken(tok)))
    };
    token(tokens, TokenTag::Newline)?;
    res
}

// ::= <stmt> EOF
pub fn parse<'a>(km: &mut KyyMutator, mut lexer: KyyLexer<'a>) -> ParseResult<'a, Root<Stmt>> {
    let stmt = stmt(km, &mut lexer)?;
    match peek(&mut lexer)? {
        None => Ok(stmt),
        Some(tok) => Err(lexer.here(Error::UnexpectedToken(tok)))
    }
}

