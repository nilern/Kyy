---
layout: default
title: "Parsing Arithmetic Expressions"
date: 2020-12-01 20:50:00 +0200
tags: parsing LL recursive-descent
---

# Parsing Arithmetic Expressions

Okay, now that we have a token stream it is time to actually parse arithmetic
expressions into a syntax tree. I will be writing an LL(1) recursive descent
parser by hand. CPython used LL(1) for a long time, although it had its own
parser generator to handle the boilerplate. Nowadays that parser generator
[uses PEG instead](https://docs.python.org/3/reference/grammar.html) but
[Parr's
book](https://www.amazon.com/gp/product/193435645X/ref=as_li_qf_asin_il_tl?ie=UTF8&tag=deepbeginning-20&creative=9325&linkCode=as2&creativeASIN=193435645X&linkId=2a158d853eecc599bed5cff5950cc0af)
(affiliate link) from the previous post also showed me how to do backtracking
and even Packrat parsing by hand if we ever have to go there. Handwritten
parsers and parser combinators can also [incorporate
backtracking](https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec.html#v:try)
and even [precedence
climbing](https://en.wikipedia.org/wiki/Operator-precedence_parser#Precedence_climbing_method)
(e.g.
[Parsec.Expr](https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec-Expr.html))
as necessary while mostly sticking to LL(1).

As also mentioned in the previous post on lexing it is my firm conviction that
language designers should use deterministic (i.e. LL or LR) parsing to avoid
introducing ambiguity into the language but Kyy is free of such
responsibilities so here we can use whatever means necessary.

Handwritten recursive descent is probably the most popular parsing technique in
interpreters and compilers. Once upon a time (LA)LR parser generators were to
conquer the world but they have been losing ground because of their usually
cryptic error messages and inflexibility. Although I have to mention that
[LALRPOP](http://lalrpop.github.io/lalrpop/) actually has quite nice error
messages, even better than [Menhir](http://gallium.inria.fr/~fpottier/menhir/).

## Adding Lookahead to the Lexer

LL(1) only requires a single token of lookahead but at the moment our lexer
does not provide even that. To highlight this omission let's rename the
current lexer:

```diff
- pub struct KyyLexer<'a> {
+ struct LookaheadlessLexer<'a> {
```

and replace it with a wrapper that provides the lookahead:

```rust
pub struct KyyLexer<'a> {
    tokens: LookaheadlessLexer<'a>,
    lookahead: Option<LexResult>
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
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        self.lookahead.take()
            .or_else(|| self.tokens.next())
    }
}
```

The lookahead token handling in `peek` and `next` gets a lot of mileage out of
the `Option` utility methods. It is neat when that happens but one should not
feel bad about just `match`ing `Option`s directly either.

Speaking of utilities, why not just use `std::iter::Peekable` to add lookahead?
Lookahead could indeed be conveniently implemented that way, but we also need
access (via `here` and `spanning`) to the byte index and filename in `.tokens`
and `Peekable` does not allow that. Surely `Peekable` could be made to work as
well but I rolled my own lookahead and there is no profit in getting hung up on
such issues.

## The Recursive Descent Parser

```rust
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
```

```rust
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
```

```rust
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
```

```rust

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
```

## Seeing the Results

```rust
fn main() {
    let mut repl = rustyline::Editor::<()>::new();

    loop {
        match repl.readline(PROMPT) {
            Ok(line) => {
                let lexer = lexer::KyyLexer::new(&line, None);
                match parser::parse(lexer) {
                    Ok(expr) => println!("{:#?}", expr),
                    Err(err) => println!("Syntax error: {:?}", err)
                }
            },
            Err(ReadlineError::Eof) => break,
            Err(ReadlineError::Interrupted) => continue,
            Err(err) => println!("Readline error: {}", err)
        }
    }
}
```

```sh
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.01s
     Running `target/debug/kyy`
>>> 1 + 2 * 3
Spanning {
    value: Add(
        Spanning {
            value: Const(
                1,
            ),
            filename: None,
            span: 0..1,
        },
        Spanning {
            value: Mul(
                Spanning {
                    value: Const(
                        2,
                    ),
                    filename: None,
                    span: 4..5,
                },
                Spanning {
                    value: Const(
                        3,
                    ),
                    filename: None,
                    span: 8..9,
                },
            ),
            filename: None,
            span: 4..9,
        },
    ),
    filename: None,
    span: 0..9,
}
```

---

[Back to the front page](/Kyy/)

As an Amazon Associate I earn from qualifying purchases. [If such things ever happen...]

