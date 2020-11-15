---
layout: default
title: "Lexing Arithmetic Expressions"
date: 2020-11-15 20:58:00 +0200
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

atom = int

int = digit int
    | digit

digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
```

```
1 + 2 * 3
```

```rust
Add(Const(1), Multiply(Const(2), Const(3)))
```

## Separating Tokenization Concern into Lexer

```
expr = expr (PLUS | MINUS) term
     | term

term = term (STAR | SLASH) atom
     | atom

atom = INT
```

```
PLUS = '+'
MINUS = '-'
STAR = '*'
SLASH = '/'
INT = \d+
```

```
1 + 2 * 3
```

```rust
["1", "+", "2", "*", "3"]
```

## Tokens

`src/lexer.rs`:

```rust
#[derive(Debug, Clone)]
pub enum Token {
    Plus, Minus,
    Star, Slash,

    Integer(isize)
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

