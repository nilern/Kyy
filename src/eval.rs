use std::collections::HashMap;
use std::sync::Arc;

use super::lexer::Spanning;
use super::parser::{Const, Expr, ExprRef, Stmt};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Int(isize),
    Bool(bool)
}

#[derive(Debug)]
pub enum Error {
    TypeError(Vec<Value>),
    Unbound(Arc<String>)
}

type EvalResult = Result<Value, Spanning<Error>>;

type Env = HashMap<String, Value>;

pub fn new_global_env() -> Env { HashMap::new() }

pub fn eval(env: &Env, expr: &ExprRef) -> EvalResult {
    use Value::*;

    match expr.value {
        Expr::Add(ref l, ref r) => match (eval(env, l)?, eval(env, r)?) {
            (Int(l), Int(r)) => Ok(Int(l + r)),
            (l, r) => Err(expr.here(Error::TypeError(vec![l, r])))
        },
        Expr::Sub(ref l, ref r) => match (eval(env, l)?, eval(env, r)?) {
            (Int(l), Int(r)) => Ok(Int(l - r)),
            (l, r) => Err(expr.here(Error::TypeError(vec![l, r])))
        },
        Expr::Mul(ref l, ref r) => match (eval(env, l)?, eval(env, r)?) {
            (Int(l), Int(r)) => Ok(Int(l - r)),
            (l, r) => Err(expr.here(Error::TypeError(vec![l, r])))
        },
        Expr::Div(ref l, ref r) => match (eval(env, l)?, eval(env, r)?) {
            (Int(l), Int(r)) => Ok(Int(l - r)),
            (l, r) => Err(expr.here(Error::TypeError(vec![l, r])))
        },

        Expr::Lt(ref l, ref r) => match (eval(env, l)?, eval(env, r)?) {
            (Int(l), Int(r)) => Ok(Bool(l < r)),
            (l, r) => Err(expr.here(Error::TypeError(vec![l, r])))
        },
        Expr::Le(ref l, ref r) => match (eval(env, l)?, eval(env, r)?) {
            (Int(l), Int(r)) => Ok(Bool(l <= r)),
            (l, r) => Err(expr.here(Error::TypeError(vec![l, r])))
        },
        Expr::Eq(ref l, ref r) => match (eval(env, l)?, eval(env, r)?) {
            (Int(l), Int(r)) => Ok(Bool(l == r)),
            (l, r) => Err(expr.here(Error::TypeError(vec![l, r])))
        },
        Expr::Ne(ref l, ref r) => match (eval(env, l)?, eval(env, r)?) {
            (Int(l), Int(r)) => Ok(Bool(l != r)),
            (l, r) => Err(expr.here(Error::TypeError(vec![l, r])))
        },
        Expr::Gt(ref l, ref r) => match (eval(env, l)?, eval(env, r)?) {
            (Int(l), Int(r)) => Ok(Bool(l > r)),
            (l, r) => Err(expr.here(Error::TypeError(vec![l, r])))
        },
        Expr::Ge(ref l, ref r) => match (eval(env, l)?, eval(env, r)?) {
            (Int(l), Int(r)) => Ok(Bool(l >= r)),
            (l, r) => Err(expr.here(Error::TypeError(vec![l, r])))
        },

        Expr::Var(ref name) => match env.get(&**name) {
            Some(&v) => Ok(v),
            None => Err(expr.here(Error::Unbound(name.clone())))
        },

        Expr::Const(ref c) => match *c {
            Const::Int(n) => Ok(Int(n)),
            Const::Bool(b) => Ok(Bool(b))
        }
    }
}

pub fn exec(env: &mut Env, stmt: Stmt) -> EvalResult {
    match stmt {
        Stmt::Assign(ref var, ref rvalue) => {
            let value = eval(env, rvalue)?;
            env.insert(var.clone(), value);
            Ok(value)
        },
        Stmt::Expr(ref expr) => eval(env, expr)
    }
}

