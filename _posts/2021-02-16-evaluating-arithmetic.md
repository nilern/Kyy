---
layout: default
title: "Evaluating Arithmetic Expressions"
date: 2021-02-16 20:20:00 +0200
tags: evaluation AST interpreter
---

# Evaluating Arithmetic Expressions

Now that we have an AST, we finally get to run it. Interpret it. Evaluate it.
I'll call it evaluation because we only have (arithmetic) expressions at this
point. (*Not* because I contracted a Lisp at some point and there seems to be
no cure.)

This post is going to be shorter and more satisfying than the previous ones.
Parsing can be interesting, but when implementing a language one is not
interested in parsing theory -- how awkward.

## More Pointers Please

With trees like these it is more convenient to handle the node pointers most of
the time, so I added an `ExprRef` alias to `parser.rs`:

```diff

#[derive(Debug)]
- pub enum Expr_ {
+ pub enum Expr {
-     Add(Box<Expr>, Box<Expr>),
-     Sub(Box<Expr>, Box<Expr>),
-     Mul(Box<Expr>, Box<Expr>),
-     Div(Box<Expr>, Box<Expr>), // FIXME: __truediv__ vs. __floordiv__
+     Add(ExprRef, ExprRef),
+     Sub(ExprRef, ExprRef),
+     Mul(ExprRef, ExprRef),
+     Div(ExprRef, ExprRef), // FIXME: __truediv__ vs. __floordiv__

    Const(Const)
}

- type Expr = Spanning<Expr_>;
+ pub type ExprRef = Box<Spanning<Expr>>;

```

Besides, once we get a GC all the managed values have to be handled through
some type of GC pointer anyway. As a bonus now that we don't use the unboxed
`Expr` alias the node type can be `Expr` instead of the contrived `Expr_`.

## The Evaluator

Now just start a new file `eval.rs`:

```rust
use super::parser::{Expr, ExprRef};

type Value = isize;
```

`Value` represents all the runtime values we can have. Eventually it should be
a static type "mirror" of `<class 'object'>`. But since we only have integer
constants and arithmetic operators at the moment, everything can just be a
signed machine word -sized integer.

The evaluator itself is really simple and obvious (after all this sort of thing
is more typical of *parser* than *interpreter* tutorials):

```rust
pub fn eval(expr: ExprRef) -> Value {
    match expr.value {
        Expr::Add(l, r) => eval(l) + eval(r),
        Expr::Sub(l, r) => eval(l) - eval(r),
        Expr::Mul(l, r) => eval(l) * eval(r),
        Expr::Div(l, r) => eval(l) / eval(r),
        Expr::Const(c) => c
    }
}
```

For a constant, just grab the value. For the arithmetic operators, just use the
corresponding Rust operator ("snarfing features from the implementation
language", as Lispers said of old).

Strict evaluation is implemented by recursively evaluating the operator
arguments before the operation itself. Lazy evaluation would quite a bit more
complicated to implement, and as a rather imperative language (Despite OO? Or
because of it?) Python could not really cope with it anyway.

## Plug It in

Once again we can activate our progress by slightly modifying `main`, this time
by `eval`uating the AST and printing (also) the resulting value:

```diff
mod lexer;
mod parser;
+ mod eval;

use rustyline::error::ReadlineError;
 
+ use eval::eval;
+ 
const PROMPT: &'static str = ">>> ";

                let lexer = lexer::KyyLexer::new(&line, None);
                match parser::parse(lexer) {
-                     Ok(expr) => println!("{:#?}", expr),
+                     Ok(expr) => {
+                         println!("{:#?}", expr);
+                         println!("=> {:?}", eval(expr));
+                     },
                    Err(err) => println!("Syntax error: {:?}", err)
                }
```

Spin it:

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
7
```

That is correct. We could disable the AST printing now but at least for quite a
while the only user will be me, debugging the interpreter.

---

[Back to the front page](/Kyy/)

