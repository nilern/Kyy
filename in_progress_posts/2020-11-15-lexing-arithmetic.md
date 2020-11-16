---
layout: default
title: "Lexing Arithmetic Expressions"
date: 2020-11-15 20:58:00 +0200
categories: lexing parsing
---

# Lexing Arithmetic Expressions

## Parsing

Parsing derives structure from streams of characters, raw bytes or generally
any "symbols of an alphabet". Usually the resulting structure is a tree but it
can be a general graph, a simple integer or even a compiled program as happens
with one-pass compilers.

## Simple Arithmetic Language

As is customary in parsing and parser tooling tutorials we shall start by
parsing simple arithmetic expressions, described by this grammar:

```
expr = expr ('+' | '-') term
     | term

term = term ('*' | '/') atom
     | atom

atom = int

int = digit int
    | digit

digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
```

Or rather, that grammar with whitespace skipping added. (Incidentally, if you
don't know how to read BNF you probably shouldn't be reading this blog just
yet.)

Basically the goal is to turn input like

```
1 + 2 * 3
```

into an abstract syntax tree (AST) format such as

```rust
Add(Const(1), Multiply(Const(2), Const(3)))
```

## Separating Tokenization Concern into Lexer

Character-level grammars look noisy. What is much worse is that popular parsing
techniques with a small lookahead like LL(1) (e.g. handwritten recursive
descent) and (LA)LR(1) (e.g. YACC or LALRPOP). So the parsing task is often
split into two layers: tokenization AKA lexing, which splits the input into
tokens (e.g. keywords, operators, constants, identifiers) and parsing proper,
which parses the token stream to create more structured data like syntax trees.

Tokens don't usually contain nested structures so they can be described with
regular grammars. Because a token contains much more information than a
character the token stream can often be parsed with relatively weak parsing
techniques. So the combination of regular expressions and e.g. LL(1) becomes
considerably more powerful than either component.

Being able to use a deterministic parsing algorithm like LL(k) or (LA)LR(k)
also has the benefit of ensuring that the programming language grammar is
deterministic. In fact we do not have any better way to avoid grammar
ambiguities. Natural language is full of ambiguity, but an ambiguous
programming language syntax would probably be unusable at best.

So we can split our arithmetic grammar into a token grammar

```
PLUS = '+'
MINUS = '-'
STAR = '*'
SLASH = '/'
INT = \d+
```

and the main grammar that uses the token names:

```
exps = expr (PLUS | MINUS) term
     | term

term = term (STAR | SLASH) atom
     | atom

atom = INT
```

Unfortunately there is no established term that would unambiguously (hehe)
refer to the parsing layer above the lexer. You see, tokenization is a form of
parsing too; it turns a stream of characters such as

```
1 + 2 * 3
```

into a stream of tokens somewhat like

```rust
["1", "+", "2", "*", "3"]
```

It is also useful to view the lexer as a stream or iterator transformer like
`map` and `filter`.

## Token Datatype

Let's start by adding a `Token` enum into an empty file `src/lexer.rs`:

```rust
#[derive(Debug, Clone)]
pub enum Token {
    Plus, Minus,
    Star, Slash,

    Integer(isize)
}
```

Various grammar-like specifications can be conveniently transliterated into
algebraic datatypes like this. We have tokens for the arithmetic operators '+',
'-', '*', '/' and integer literals. The `Integer` tokens carry the parsed
integer. Using `isize` is quite sloppy; just like garbage collection, the
implementation of arbitrary-precision arithmetic is postponed to maintain
motivation.

We could make the tokens homogeneous records of a token type tag and a string
slice. We would still have a 'zero-copy' lexer with lightweight tokens since
taking a `&str` causes no (heap) allocation. But the string slices would
contain no useful data for the operators while failing to convey through the
type system that the characters of an integer token are guaranteed to be
parsable into an integer.

## Tracking Source Locations

One cross-cutting concern in any interpreter or compiler is keeping track of
source locations so that errors (both static type errors and runtime exceptions)
can be pinpointed to their place of origin in the source code. Source locations
are also part of the debug information required by e.g. source-level debuggers.

Source location tracking is boring and tedious so most compiler textbooks and
other teaching materials largely ignore it, which is understandable. What they
should not ignore is the generation of good error messages since error messages
are most of the user interface of a language implementation (apart from the
language syntax itself, of course).

Since I anticipate that tokens will not be the only values that need to be
wrapped with location information I chose to make generic structs `Located<T>`
and `Spanning<T>` which add a single source location or a span between two
source locations to some `T`:


```rust
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
```

The location information consists of just the filename and byte offset into the
file. Not saving the line and column saves space and can be recomputed from the
file contents and the offset. Reopening files should not be a problem since
when we are printing error messages or using the debugger we are on a slow path
anyway. And if the source code file changes it should be reloaded anyway.

The filenames are optional because not all code comes from files (e.g. REPL
input lines or `eval`/`exec` input strings). The filename should be shared
among many located values so I put it in the **a**tomically
**r**eference-**c**ounted `Arc` heap allocation. CPython uses reference
counting as the primary system of automatic memory management. Its reference
counting is not thread-safe, which is one major reason why CPython has to have
the Global Interpreter Lock. Using atomic instructions for incrementing and
decrementing reference counts is slightly slower, so Rust also offers the `Rc`
type for single-threaded reference counting (to enforce single-threaded use
`Rc` does not implement `Send` or `Sync`). Reference counting also cannot
collect cycles which interpreters inevitably create e.g. to support recursion.
I plan to implement a tracing garbage collector for Kyy but debugging garbage
collection before being able to even parse anything would be extremely
demoralizing.

These are just simple records so I made the fields public. I anticipate no
problems from this "lack of abstraction". Often objects or other abstract
datatypes are a good idea. Many other times plain old data is best, especially
when it is kept immutable. Getters and setters make a mockery of both.

## Tokenization

A lexer can be represented idiomatically in Rust by making it a function from
an iterator of chars to an iterator of tokens. However it is easier to
implement a lexer that only works on string slices `&str` and in these days of
ample memory it is actually more efficient to read the compilation unit into a
`String` up front.

```rust
struct LookaheadlessLexer<'a> {
    chars: &'a str,
    index: usize,
    filename: Option<Arc<String>>
}
```

```rust
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
```

```rust
impl<'a> Iterator for LookaheadlessLexer<'a> {
    type Item = LexResult;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.peek_char() {
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

                    return Some (Ok(self.spanning(Token::Integer(n), start_index..self.index)))
                },

                Some(c) if c.is_whitespace() => { self.pop_char(); }, // skip \s (\s* with the outer loop)

                Some(c) => return Some(Err(self.here(Error::UnexpectedChar(c)))),

                None => return None // EOF
            }
        }
    }
}
```

## Plugging the Lexer into Our REPL

For manual testing and motivating stimulation we can make the REPL echo the
tokens instead of just the input line itself:

```rust
mod lexer;

use rustyline::error::ReadlineError;

const PROMPT: &'static str = ">>> ";

fn main() {
    let mut repl = rustyline::Editor::<()>::new();

    loop {
        match repl.readline(PROMPT) {
            Ok(line) => {
                let lexer = lexer::KyyLexer::new(&line, None);
                match lexer.collect::<Result<Vec<_>, _>>() {
                    Ok(tokens) => println!("{:#?}", tokens),
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

`collect`ing an `Iterator` of `Result`s into a `Result<Vec<_>, _>` is a nice
trick (although not very discoverable). But this token printing will not last
long; in the next post we will parse the token stream into an abstract syntax
tree, a much more useful data structure.

---

[Back to the front page](/Kyy/)

As an Amazon Associate I earn from qualifying purchases. [If such things ever happen...]

