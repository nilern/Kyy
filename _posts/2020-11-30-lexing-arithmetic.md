---
layout: default
title: "Lexing Arithmetic Expressions"
date: 2020-11-19 21:59:00 +0200
tags: lexing parsing
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
have never encountered BNF before you probably shouldn't be reading this blog
just yet.)

Basically the goal is to turn input like

```
1 + 2 * 3
```

into an abstract syntax tree (AST) format such as

```rust
Add(Const(1), Mul(Const(2), Const(3)))
```

## Separating Tokenization Concern into Lexer

Character-level grammars look noisy. Far worse, they often cannot be parsed
with popular parsing techniques with a small lookahead like LL(1) (e.g.
handwritten recursive descent) and (LA)LR(1) (e.g. YACC or
[LALRPOP](http://lalrpop.github.io/lalrpop/)). So the parsing task is often
split into two layers: tokenization a.k.a. lexing, which splits the input into
tokens (e.g. keywords, operators, constants, identifiers) and parsing "proper"
which parses the token stream to create more structured data like syntax trees.

The division into lexing and parsing makes deterministic parsing techniques
usable. Tokens can usually be defined with regular expressions which can be
implemented very efficiently with finite state machines. Because a token
contains much more information than a character the token stream can often be
parsed with relatively weak parsing techniques like the deterministic LR and LL
parsers. (LA)LR(1) can parse most sensible programming languages from a token
stream and LL(1) served CPython for many years (as we will see in later posts,
indentation sensitivity is mostly implemented in the lexer).

In language design a deterministic parsing algorithm like LL(k) or (LA)LR(k)
also has the benefit of ensuring that the programming language grammar is
deterministic. In fact it is one of the best known methods to avoid grammar
ambiguities. Natural language is full of ambiguity, but an ambiguous
programming language syntax would probably be unusable at best. But we don't
need to worry about any of that when reimplementing an existing language.

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
expr = expr (PLUS | MINUS) term
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
contain no useful data for the operators while failing to [convey through the
type
system](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
that the characters of an integer token are guaranteed to be parsable into an
integer.

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
when we are printing error messages or using the debugger we are already on a
slow path. And if the source code file changes it should be reloaded anyway.

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
collect cycles which interpreters inevitably create e.g. to support recursion,
so one must either also implement a tracing GC for backup (like CPython) or
break cycles manually (like `Arc` and Swift). I plan to implement a tracing
garbage collector for Kyy but debugging garbage collection before being able to
even parse anything would be extremely demoralizing.

These are just simple records so I made the fields public. I anticipate no
problems from this "lack of abstraction". Often objects or other abstract
datatypes are a good idea. Many other times plain old data is best, especially
when it is kept immutable. Getters and setters make a mockery of both.

## Tokenization

A lexer can be represented idiomatically in Rust by making it a function from
an iterator of chars to an iterator of tokens. However it is easier to
implement a lexer that only works on string slices `&str` and in these days of
ample memory [it is actually more
efficient](https://nothings.org/computer/lexing.html) to read the compilation
unit into a `String` up front.

```rust
struct LookaheadlessLexer<'a> {
    chars: &'a str,
    index: usize,
    filename: Option<Arc<String>>
}
```

Of course we have the string slice in `chars`. Additionally we need a byte
`index` to keep track of our position in the slice. The index also gets saved
into the tokens, along with the `filename`. One can get into very Rust-specific
troubles when parameterizing structs over lifetimes like this but hey,
zero-copy lexing sure makes a Rust programmer proud.

Some utility methods are essential to `impl`ement. In particular it would be
unbearable to repeat the UTF-8 iterator fiddling in `peek_char` and `pop_char`:

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

As previously alluded to, regular expressions are sufficiently powerful to
describe tokens. Lexer generators like Lex convert the token regexen into
efficient (and undebuggable) deterministive finite automata that run in
constant space and linear time. Unfortunately the regex types of most
programming languages [use exponential backtracking
algorithms](https://swtch.com/~rsc/regexp/regexp1.html) instead (but [not
Rust](https://crates.io/crates/regex), yay!).

Handwritten lexers naturally gravitate to LL(1) (or [LL(m,
k)](https://www.antlr.org/papers/parr.phd.thesis.pdf)). The only systematic
account of this methodology that I have seen was "Pattern 2: LL(1)
Recursive-Descent Lexer" in [Language Implementation
Patterns](https://www.amazon.com/gp/product/193435645X/ref=as_li_qf_asin_il_tl?ie=UTF8&tag=deepbeginning-20&creative=9325&linkCode=as2&creativeASIN=193435645X&linkId=2a158d853eecc599bed5cff5950cc0af)
(affiliate link). [Intriguing that [the ANTLR
guy](https://twitter.com/the_antlr_guy) would write a book with 50 pages of
material on manual LL and Packrat (!) parsing.] Parr also points out that
unlike regular expressions, LL(1) can easily deal with nested block comments
(one of the numerous ML features also found in Rust). On the other hand regular
expression implementations support infinite lookahead -- in constant space.

To select from several alternatives we `match` a lookahead character:

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
```

Unsurprisingly loops (or tail recursion) can be used for repetition. Parr
implements `*` with `while` loops and `+` with `do while` loops, but Rust does
not have the latter and `while` conditions get a bit funky when we also have to
deal with `Option`s.  My non-`for` loops usually start out as `loop { match
...` anyway:

```rust
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
```

Later I refactored to a while loop:

```rust
while let Some(d) = self.peek_char().and_then(|c| c.to_digit(10)) { // \d*
    let _ = self.pop_char();
    n = 10*n + isize::try_from(d).unwrap();
}
```

What a complicated condition! At least there are less lines, which reduces
bugs, I once heard Cliff Click say.

Whitespace is just skipped and if we run out of characters when looking for the
start of a token we are also out of tokens, so just propagate the `None`. Any
other character is an error:

```rust
                Some(c) if c.is_whitespace() => { self.pop_char(); }, // skip \s (\s* with the outer loop)

                Some(c) => return Some(Err(self.here(Error::UnexpectedChar(c)))),

                None => return None // EOF
            }
        }
    }
}
```

To support returning `Result`s from an iterator the `next` method needs to
return an `Option<Result, ...`. I picked this idea up from LALRPOP's lexer
interface but the `Iterator` trait basically forces this strategy.

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
trick (although not very discoverable). In Haskell the more general pattern
`traverse` is in constant use. In any case this token printing will not last
long; in the next post we will parse the token stream into an abstract syntax
tree, a much more useful data structure.

---

[Back to the front page](/Kyy/)

As an Amazon Associate I earn from qualifying purchases. [If such things ever happen...]

