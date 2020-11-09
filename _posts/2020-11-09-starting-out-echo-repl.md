---
layout: default
title: "Starting Out: Echo REPL"
date: 2020-11-09 22:46:00 +0200
categories: tooling REPL
---

## Starting Out: Echo REPL

### Build Dependencies

We will be using [Rust](rust-lang.org/) so you need to have the Cargo build tool
(and the `rustc` compiler) [installed](https://www.rust-lang.org/tools/install).
This is not a beginner tutorial on the Rust language or tooling, but the Rust
community [has that covered](https://www.rust-lang.org/learn). And Cargo is
really user-friendly, especially for a build tool.

I will also assume you have access to a terminal, a text editor and Git. I use
(Arch) Linux so the command lines will be in Unix `sh` syntax. If you are using
something exotic like [scsh](https://scsh.net/) or Windows adjust the commands
to taste.

Let's go!

### Creating a Project

Create a Cargo project:

```sh
$ cargo new kyy
     Created binary (application) `kyy` package
```

As you can see, the default template is an application. What you cannot see is
that the default VCS initialization is Git. Exactly what I wanted. Although at
some point we will probably extract a library to support embedding the Kyy VM
and to encourage putting some design into module interfaces.

Now we can test that our Cargo setup works:

```sh
$ cargo run
   Compiling kyy v0.1.0 (/home/nilern/kyy)
    Finished dev [unoptimized + debuginfo] target(s) in 0.25s
     Running `target/debug/kyy`
Hello, world!
```

Time for the initial commit:

```sh
$ git add .
$ git commit -m "Initialize Cargo project."
[master (root-commit) 31daf98] Initialize Cargo project.
 4 files changed, 20 insertions(+)
 create mode 100644 .gitignore
 create mode 100644 Cargo.lock
 create mode 100644 Cargo.toml
 create mode 100644 src/main.rs
```

Satisfying as always. As you might have deduced from the URL of this page I push my
commits to GitHub.

### Licence Up

Next I added the (MIT) `LICENSE` file and also set the license in the project file `Cargo.toml`:

```toml
[package]
name = "kyy"
version = "0.1.0"
authors = ["Pauli Jaakkola <pauli.jaakkola@iki.fi>"]
edition = "2018"
license = "MIT"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[dependencies]
```

Add a seal to that:

```sh
$ git commit -am "Add License"
```

and now we are legit. Moving on.

### Echo REPL

It is always handy to have a REPL (Read-Eval-Print-Loop, a Lisp invention
[like so many other things we will encounter](https://en.wikipedia.org/wiki/Lisp_(programming_language)#Language_innovations)),
even when implementing the language the REPL evaluates. So our first Rust code will add one
to `main.rs`:

```Rust
use rustyline::error::ReadlineError;

const PROMPT: &'static str = ">>> ";

fn main() {
    let mut repl = rustyline::Editor::<()>::new();

    loop {
        match repl.readline(PROMPT) {
            Ok(line) => println!("{}", line),
            Err(ReadlineError::Eof) => break,
            Err(ReadlineError::Interrupted) => continue,
            Err(err) => println!("Readline error: {}", err)
        }
    }
}
```

This uses [RustyLine](https://crates.io/crates/rustyline/), a Rust port of the compact
[Linenoise](https://github.com/antirez/linenoise) library. I find it strange that so
many tools omit line editing so that one has to remember to start them via `rlwrap`;
as Linenoise demonstrated,
[line editing does not have to be 20000 lines of code](https://github.com/antirez/linenoise#can-a-line-editing-library-be-20k-lines-of-code).

The `Err(ReadlineError::Eof) => break,` line implements exit from REPL (using e.g.
Ctrl+D on *nix) and the `Err(ReadlineError::Eof) => break,` line implements abort
to next input line (e.g. Ctrl+C on *nix). And now we have an echo REPL:

```sh
$ cargo run
    Finished dev [unoptimized + debuginfo] target(s) in 0.01s
     Running `target/debug/kyy`
>>> foo
foo
>>>
>>>
```

Useless perhaps but as Jamie Zawinski put it in [Coders at Work](https://amzn.to/36jOdoj):

> I find that getting something on the screen as soon as possible really helps focus the problem for me.
> It helps me decide what to work on next. Because if you’re just looking at that big to-do list it’s like,
> eh, I don’t know which one I should do—does it matter which one I do? But if there’s something you can
> actually look at, even if it’s just the debug output of your mailbox parser, it’s like, OK, there!
> That’s something; what’s the next direction this needs to go in? OK, instead of just displaying a
> tree structure, now maybe I should be emitting HTML or something along those lines. Or parsing the
> headers in a more detailed way. You just look for the next thing to build on from there.

And of course I have a huge to-do list for Kyy. I have [a lot of lists](https://amzn.to/35ckTRm),
they keep me calm.

From now on I will not bore you with `git commit` commands, you can
[see for yourself](https://github.com/nilern/Kyy/commits/master) if you must.

### To Be Continued

Next time we will be getting into some actual programming language technology:
lexing AKA tokenization.
