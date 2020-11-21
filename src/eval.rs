use std::collections::HashMap;
use std::sync::Arc;

use super::lexer::Spanning;
use super::parser::{Const, Expr, ExprRef, Stmt};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Int(isize),
    Bool(bool)
}

impl From<Value> for bool {
    fn from(v: Value) -> bool {
        match v {
            Value::Int(0) => false,
            Value::Int(_) => true,
            Value::Bool(b) => b
        }
    }
}

#[derive(Debug)]
pub enum Error {
    TypeError(Vec<Value>),
    Unbound(Arc<String>)
}

type EvalResult<T> = Result<T, Spanning<Error>>;

type Env = HashMap<String, Value>;

pub fn new_global_env() -> Env { HashMap::new() }

pub fn eval(env: &Env, expr: &ExprRef) -> EvalResult<Value> {
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

fn exec_block(env: &mut Env, stmts: &[Stmt]) -> EvalResult<Option<Value>> {
    let mut res = None;
    for stmt in stmts {
        res = exec(env, stmt)?;
    }
    Ok(res)
}

pub fn exec(env: &mut Env, stmt: &Stmt) -> EvalResult<Option<Value>> {
    match stmt {
        Stmt::If {ref condition, ref conseq, ref alt} =>
            if eval(env, condition)?.into() {
                exec_block(env, conseq)
            } else {
                exec_block(env, alt)
            },

        Stmt::Assign(ref var, ref rvalue) => {
            env.insert(var.clone(), eval(env, rvalue)?);
            Ok(None)
        },

        Stmt::Expr(ref expr) => eval(env, expr).map(Some)
    }
}

