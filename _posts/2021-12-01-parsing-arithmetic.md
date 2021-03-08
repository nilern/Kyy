---
layout: default
title: "Parsing Arithmetic Expressions"
date: 2021-02-13 21:50:00 +0200
tags: parsing LL recursive-descent
---

# Parsing Arithmetic Expressions

Okay, now that we have a token stream it is time to actually parse arithmetic
expressions into a syntax tree. I will be writing an LL(1) recursive descent
parser by hand. CPython used LL(1) for a long time, although it had its own
parser generator to handle the boilerplate. Nowadays that parser generator
[uses PEG instead](https://docs.python.org/3/reference/grammar.html) but
[Parr's book (affiliate
link)](https://www.amazon.com/gp/product/193435645X/ref=as_li_qf_asin_il_tl?ie=UTF8&tag=deepbeginning-20&creative=9325&linkCode=as2&creativeASIN=193435645X&linkId=2a158d853eecc599bed5cff5950cc0af)
from the previous post also showed me how to do backtracking and even Packrat
parsing by hand if we ever have to go there. Handwritten parsers and parser
combinators can also [incorporate
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

## The Grammar

We are making an arithmetic expression parser which parses the `KyyLexer` token
iterator. The previous lexer post already showed the precise grammar over
tokens:

```
expr = expr (PLUS | MINUS) term
     | term

term = term (STAR | SLASH) atom
     | atom

atom = INT
```

This grammar already encodes operator precedence: e.g. `expr (PLUS | MINUS)
term` makes addition and subtraction left-associative. The only point in
showing a naïve ambiguous grammar like

```
expr = expr PLUS expr | expr MINUS expr
     | expr STAR expr | expr SLASH expr
     | INT
```

is to point out that it can't possibly lead to the parser you want *because it
simply does not express operator precedence at all*.

## Abstract Syntax Tree

A **recognizer** just checks that the sequence of symbols matches the grammar.
Regular expressions are often used that way, but as usual with context-free
grammars we want a proper **parser** which produces [something more convenient
than the input string](https://wiki.c2.com/?StringlyTyped). And we will produce
the usual thing, an **A**bstract **S**yntax **T**ree (in a new file
`parser.rs`):

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

Although the structure of this `enum` is quite close to the grammar, it is not
a **conrete syntax tree**; those match the structure of the grammar exactly and
often also retain all the tokens while our tree here retains none.

Since Rust (like C and Go) uses value semantics so recursion in types has to go
through some kind of explicit pointer or the compiler will complain about
infinitely sized types. The default `Box` is good enough for now, but
eventually we will have to implement a garbage collector and use GC'd pointers
in AST:s like everything else.

Note how the recursion also goes through `Spanning`, a nice [orthogonal way to
add source locations](http://mlton.org/AST#_details_and_notes). And in Rust this
has guaranteed zero cost, unlike in ML (although MLton does have [optimizations
for this](http://mlton.org/Flatten)).

## Error Handling, Front and Center

As good Rustaceans we want to be very explicit with error handling. It also
bears reiterating that error messages are half of the compiler UI (the other
half being the language syntax) and probably most of the UX.

There aren't actually that many types of error situations the parser can get
into. It can encounter an unexpected token, unexpected end of input or a lexer
error (unexpected character):

```rust
#[derive(Debug)]
pub enum Error {
    UnexpectedToken(Token),
    Eof,
    Lex(lexer::Error)
}
```

Errors should be wrapped with source locations, so let's add some boilerplate
to convert a `Located` lexer error to a `Located` parse error and the usual
`Result` alias:

```rust
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

This does not provide super-helpful high-level error messages, but at least the
error is pinpointed. I am just keeping it simple to get some sort of parser
going but many production compilers settle for this level of parse error. Which
is part of the reason people tend to resent compilers for non-mainstream
languages (static type errors are worse, but Python does not have standard
built-in static typing, so we get to avoid that issue).

Finally (with the benefit of hindsight) we'll add some helpers that peek at the
first token while producing `ParseResult`s of tokens:

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

It is convenient to centralize the error munging here instead of fiddling with
`LexResult = Result<Spanning<Token>, Located<Error>>`:s all over the place;
remember, that format was just forced upon us by the `Iterator` trait. (Maybe
implementing `Iterator` wasn't such a bright idea after all? But otherwise, it
fit so well...)

## The Actual Parser

As could be expected, parsing an "atom" prefix of the input is trivial: just
check that there is a first token and it is suitable using `peek_some` and if
so, consume that token. Furthermore at this point atoms can only be integer
constants:

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
```

The triviality is not accidental, but occurs because our friendly neighbourhood
lexer does all the hard work. This reliance on a lexer is the main trick to
parsing anything more complicated than Lisp or JSON with an LL(1) parser; by
itself LL(1) is quite weak.

When writing the code I thought to rename the math-y `expr` and `term` to the
more obvious `additive` and `multiplicative`. Additionally (could not resist
the pun) "expr(ession)" and "term" are also very generic terms (argh) e.g.  I
named the AST expression type `Expr` without even giving it any thought.

```rust
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
```

The code seems straightforward but constructing it required detailed knowledge
of the LL(1) parsing technique. First of all (depending on your level of
overwhelm) you might have noticed that instead of immediately recursing these
functions call lower precedence ones and then enter a loop, which does not
follow the grammar productions at all. This is because LL parsing does not
support left recursion; a function that immediately calls itself never gets to
do any useful work before overflowing the stack. (In truly high-level languages
self-tail-calls loop forever and nontail self-calls grow the stack until the
process runs out of memory).

Fortunately there are widely known grammar transformations that get rid of
left recursion while retaining the same input language. In particular simple
directly left-recursive nonterminals like

```
additive = additive (PLUS | MINUS) multiplicative
         | multiplicative
```

can be turned into

```
additive = multiplicative additive'
additive' = (PLUS | MINUS) multiplicative additive'
          |
```

or, transitioning to EBNF with the regex repetition operators `?`, `*` and `+`:

```
additive = multiplicative ((PLUS | MINUS) multiplicative)*
```

And as we already saw on the lexer post, `*` can be implemented with a loop.
So this explains the `loop`s.

But what about the token `match`ing inside the loops? We could just handwave it
like in the lexer post. But [my hefty parsing tome (affiliate
link)](https://www.amazon.com/gp/product/038720248X/ref=as_li_tl?ie=UTF8&tag=deepbeginning-20&camp=1789&creative=9325&linkCode=as2&creativeASIN=038720248X&linkId=2754d9e90cb516a38c7c1f43740ebb3a)
informs me that there is a method to this madness. (Can an e-book be hefty?
Even the Kindle edition certainly feels hefty to me...):

1. Compute the FIRST set of each production. This is just the set of tokens
   that can start a valid input for the production. If a production is
   "nullable" (matches the empty string), add `epsilon` to the FIRST set.
2. If there were any nullable productions, compute the FOLLOW set of each
   production. This is just the set of tokens that can follow the production in
   a valid input string.
3. Compute the LOOKAHEAD set of each production. It is just FIRST if `epsilon
   not in FIRST` and `FIRST - {epsilon} U FOLLOW` otherwise.

If the LOOKAHEAD sets of the alternative productions of a nonterminal overlap,
the grammar is unsuitable for LL(1) parsing. The LOOKAHEAD set of any
left-recursive production will overlap with all the others, which is another
way to prove that LL(1) cannot handle left recursion.

The FIRST and FOLLOW computations are closure algorithms over the entire
grammar. This is because nonterminals can refer to each other (and themselves)
by name. It is also the reason why parser combinators usually use backtracking;
their point is to be composable, and the FIRST and FOLLOW computations are not.
I won't spell out the details of the FIRST and FOLLOW computations; maybe you
are like me and would like to work it out yourself or splurge on
[parsing](https://www.amazon.com/gp/product/038720248X/ref=as_li_tl?ie=UTF8&tag=deepbeginning-20&camp=1789&creative=9325&linkCode=as2&creativeASIN=038720248X&linkId=2754d9e90cb516a38c7c1f43740ebb3a)
and
[compiler](https://www.amazon.com/gp/product/052182060X/ref=as_li_tl?ie=UTF8&tag=deepbeginning-20&camp=1789&creative=9325&linkCode=as2&creativeASIN=052182060X&linkId=06c0fb743b0089e4771a6bbfa61a64d9)
books (affiliate links).

Honestly, most people should not need to know such tedious details and
certainly not do these computations by hand; that's what computers and parser
generators in particular are for. But we are trying to learn something here and
besides LL(1) is so weak that parser generators just get in the way of hacks
that are necessary to implement a full programming language.

Anyway, the LOOKAHEAD sets for our LL(1)-modified grammar

```
additive = multiplicative additive'
additive' = (PLUS | MINUS) multiplicative additive'
          |
multiplicative = atom multiplicative'
multiplicative' = (STAR | SLASH) atom multiplicative'
                |
atom = INT
```

are

```
additive : {INT}
additive'[0] = (PLUS | MINUS) multiplicative additive' : {PLUS, MINUS}
additive'[1] = : EOF
multiplicative = {INT}
multiplicative'[0] = (STAR | SLASH) atom multiplicative' : {STAR, SLASH}
multiplicative'[1] = : EOF
atom = {INT}
```

which should explain the production-determining `match`ing inside the loops.

Finally we have a top level `expr` function which happens to be an
eta-expansion of `additive` at the moment but I would not bet on that
continuing to be the case. So far all the parsing functions have parsed a
prefix of the input, but the exported overall `parse` ensures that the entire
input is consumed by checking with `peek` that no tokens are left.

```rust
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

Since `Expr` derives `Debug` we can just use the semi-obscure format directive
`"{:#?}"` to get a verbose pretty-print of the expression AST instead of the
tokens in the previous post:

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

Run it and throw an expression in:

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

Looks good to me. The verbosity is mostly due to the source locations that we
keep for error messages. Did I already mention that those *are* important?

---

[Back to the front page](/Kyy/)

You can [sponsor me](https://github.com/sponsors/nilern) if you would like to
see more posts sooner.

As an Amazon Associate I earn from qualifying purchases. [If such things ever happen...]

