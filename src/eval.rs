use std::collections::HashMap;

use super::parser::{Expr, ExprRef, Stmt};

type Value = isize;

type Env = HashMap<String, Value>;

pub fn new_global_env() -> Env { HashMap::new() }

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

