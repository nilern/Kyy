---
layout: default
title: "Global Variables and Statements"
date: 2021-03-13 12:20:00 +0200
tags: variables
---

# Global Variables and Statements

After the previous post we have an integer calculator with a syntax that
inevitably overlaps with Python expressions. Of course that is intentional, but
you can't even tell.

It is time to add one of the more impressive features of my trusty [TI-86]();
global variables. [Assignment
expressions](https://www.python.org/dev/peps/pep-0572/) are a recent and
controversial addition, so I decided to add just variable reference expressions
and variable assignment statements. Python makes more of a distinction between
expressions and statements than C and its derivatives, so this post will also
add the syntactic category of statements. Despite these additions, even after
this post the implemented subset of syntax is not distinctly Python-like even
though it is a valid subset of Python syntax.

Python does not have variable declaration/definition statements like C `int i(
= 0);` or Javascript `var i( = 0);`; variables are considered defined at first
assignment. This post does not add any scoping constructs and the naïve
implementation of global variables shown here happens to implement the Python
semantics nicely. We don't yet need to consider idiosyncrasies of Python
variable scoping like function scope, `global` and `nonlocal`.

## Lexing

Previous posts have built the layers of [lexing](), [parsing](), [evaluation]()
and [REPL](). From now on we will usually implement features that span layers
instead of adding new layers or subsystems on every post.

`lexer.rs`

```diff
  #[derive(Debug, Clone)]
- pub enum Token {
+ pub enum Token<'a> {
+     Assign,
      Plus, Minus,
      Star, Slash,
       
+     Identifier(&'a str),
      Integer(isize)
  }
```

```diff
- pub type LexResult = Result<Spanning<Token>, Located<Error>>;
+ pub type LexResult<'a> = Result<Spanning<Token<'a>>, Located<Error>>;
```

```diff
  impl<'a> Iterator for LookaheadlessLexer<'a> {
-     type Item = LexResult;
+     type Item = LexResult<'a>;
```

```diff
      fn next(&mut self) -> Option<Self::Item> {
          loop {
              match self.peek_char() {
+                 Some('=') => {
+                     let start_index = self.index;
+                     let _ = self.pop_char();
+                     return Some(Ok(self.spanning(Token::Assign, start_index..self.index)));
+                 },
```

```diff
+                 Some(c) if c.is_alphabetic() => { // [:alpha]+ = [:alpha:] [:alpha:]*
+                     let start_index = self.index;
+ 
+                     let _ = self.pop_char(); // [:alpha:]
+                     loop { // [:alpha:]*
+                         match self.peek_char() {
+                             Some(c) if c.is_alphabetic() => { self.pop_char(); },
+                             _ => break
+                         }
+                     }
  
+                     let span = start_index..self.index;
+                     let name = &self.chars[span.clone()];
+                     return Some(Ok(self.spanning(Token::Identifier(name), span)));
+                 },

                  Some(c) if c.is_digit(10) => { // \d+ = \d \d*
```

```diff
  pub struct KyyLexer<'a> {
      tokens: LookaheadlessLexer<'a>,
-     lookahead: Option<LexResult>
+     lookahead: Option<LexResult<'a>>
  }
```

```diff
  impl<'a> Iterator for KyyLexer<'a> {
-     type Item = LexResult;
+     type Item = LexResult<'a>;
```

## Parsing

`parser.rs`

```diff
      Mul(ExprRef, ExprRef),
      Div(ExprRef, ExprRef), // FIXME: __truediv__ vs. __floordiv__
  
+     Var(String),
      Const(Const)
  }
```

```rust
#[derive(Debug)]
pub enum Stmt {
    Assign(String, ExprRef),
    Expr(ExprRef)
}
```

```diff
- pub enum Error {
-     UnexpectedToken(Token),
+ pub enum Error<'a> {
+     UnexpectedToken(Token<'a>),
```

```diff
- // ::= INTEGER
- fn atom<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<ExprRef> {
+ // ::= IDENTIFIER
+ //   | INTEGER
+ fn atom<'a>(tokens: &mut KyyLexer<'a>) -> ParseResult<'a, ExprRef> {
      match peek_some(tokens)? {
-         Token::Integer(n) => { // INTEGER
+         Token::Identifier(chars) => {
+             let tok = tokens.next().unwrap()?;
+             Ok(Box::new(tokens.spanning(Expr::Var(String::from(chars)), tok.span)))
+         },
-         Token::Integer(n) => {
```

```rust
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
```

```diff
- // ::= <expr> EOF
- pub fn parse(mut lexer: KyyLexer) -> ParseResult<ExprRef> {
-     let expr = expr(&mut lexer)?;
+ // ::= <stmt> EOF
+ pub fn parse<'a>(mut lexer: KyyLexer<'a>) -> ParseResult<'a, Stmt> {
+     let stmt = stmt(&mut lexer)?;
      match peek(&mut lexer)? {
-         None => Ok(expr),
+         None => Ok(stmt),
          Some(tok) => Err(lexer.here(Error::UnexpectedToken(tok)))
      }
  }
```

## Evaluation

`eval.rs`

```diff
- use super::parser::{Expr, ExprRef};
+ use std::collections::HashMap;
  
+ use super::parser::{Expr, ExprRef, Stmt};
```

```rust
type Env = HashMap<String, Value>;

pub fn new_global_env() -> Env { HashMap::new() }
```

```rust
pub fn eval(env: &Env, expr: ExprRef) -> Value {
    match expr.value {
        Expr::Add(l, r) => eval(env, l) + eval(env, r),
        Expr::Sub(l, r) => eval(env, l) - eval(env, r),
        Expr::Mul(l, r) => eval(env, l) * eval(env, r),
        Expr::Div(l, r) => eval(env, l) / eval(env, r),

        Expr::Var(name) => *env.get(&name).expect("unbound"),

        Expr::Const(c) => c
    }
}
```

```rust
pub fn exec(env: &mut Env, stmt: Stmt) -> Value {
    match stmt {
        Stmt::Assign(var, rvalue) => {
            let value = eval(env, rvalue);
            env.insert(var, value);
            value
        },
        Stmt::Expr(expr) => eval(env, expr)
    }
}
```

## REPL

`main.rs`

```diff
- use eval::eval;
+ use eval::exec;
```

```diff
                  match parser::parse(lexer) {
-                     Ok(expr) => {
-                         println!("{:#?}", expr);
-                         println!("=> {:?}", eval(expr));
+                     Ok(stmt) => {
+                         println!("{:#?}", stmt);
+                         println!("=> {:?}", exec(&mut env, stmt));
                      },
```

```
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.01s
     Running `target/debug/kyy`
>>> foo = 5
Assign(
    "foo",
    Spanning {
        value: Const(
            5,
        ),
        filename: None,
        span: 6..7,
    },
)
=> 5
>>> foo
Expr(
    Spanning {
        value: Var(
            "foo",
        ),
        filename: None,
        span: 0..3,
    },
)
=> 5
```

