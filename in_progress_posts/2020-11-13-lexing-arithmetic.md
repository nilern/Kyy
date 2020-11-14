---
layout: default
title: "Lexing Arithmetic Expressions"
date: 2020-11-13 19:35:00 +0200
categories: lexing parsing
---

# Lexing Arithmetic Expressions

## Parsing

## Simple Arithmetic Language

```
expr = expr ('+' | '-') term
     | term

term = term ('*' | '/') atom
     | atom

atom = '(' expr ')'
     | int

int = digit int
    | digit

digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
```

```
1 + 2 * 3
```

```rust
Add(Int(1), Multiply(Int(2), Int(3)))
```

## Separating Tokenization Concern into Lexer

```
expr = expr ADDITIVE_OP term
     | term

term = term MULTIPLICATIVE_OP atom
     | atom

atom = LPAREN expr RPAREN
     | INT
```

```
LPAREN = '('
RPAREN = ')'
ADDITIVE_OP = '+' | '-'
MULTIPLICATIVE_OP = '*' | '/'
INT = \d+
```

## Tokens

`src/lexer.rs`:

```rust
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
```

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
the Global Interpreter Lock.  Using atomic instructions for incrementing and
decrementing reference counts is slightly slower, so Rust also offers the `Rc`
type for single-threaded reference counting (to enforce single-threaded use
`Rc` does not implement `Send` or `Sync`). Reference counting also cannot
collect cycles which interpreters inevitably create e.g. to support recursion.
I plan to implement a tracing garbage collector for Kyy but debugging garbage
collection before being able to even parse anything would be extremely
demoralizing.

I made the fields of these structs public because they are "Plain Old Data".
Getters and setters are not "real OO" (whatever *that* means); usually they do
not even provide encapsulation, only boilerplate. On the other hand I do find
the lack of private field support in Python (and Javascript) extremely
disturbing. (Yes, I know you can approximate private fields in these languages
but basic encapsulation should not require design patterns or metaprogramming
magic.)

## Tokenization

A lexer can be represented idiomatically in Rust by making it a function from
an iterator of chars to an iterator of tokens. However it is easier to
implement a lexer that only works on string slices `&str` and in these days of
ample memory it is actually more efficient to read the compilation unit into a
`String` up front. And this makes for a relatively easy demonstration of the
famed zero-copy parsing abilities of Rust!

```rust
pub struct KyyLexer<'a> {
    chars: &'a str,
    index: usize,
    filename: Option<Arc<String>>
}
```

```rust
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
```

```rust
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
```

## Plugging the Lexer into Our REPL

```rust
mod lexer;

use lexer::{Located, SyntaxError, KyyLexer};

use rustyline::error::ReadlineError;

const PROMPT: &'static str = ">>> ";

fn main() {
    let mut repl = rustyline::Editor::<()>::new();

    loop {
        match repl.readline(PROMPT) {
            Ok(line) => {
                let lexer = KyyLexer::new(&line, None);
                match lexer.collect::<Result<Vec<_>, Located<SyntaxError>>>() {
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
