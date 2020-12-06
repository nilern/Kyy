use std::collections::HashMap;
use std::sync::Arc;

use super::mutator::{KyyMutator, KyyType, KyySizedBytesType, Root};
use super::int::Int;
use super::bool::Bool;
use super::lexer::Spanning;
use super::parser::{Expr, ExprRef, Stmt};

pub enum Error {
    TypeError(Vec<Root<()>>),
    Unbound(Arc<String>)
}

type EvalResult<T> = Result<T, Spanning<Error>>;

type Env = HashMap<String, Root<()>>;

pub fn new_global_env() -> Env { HashMap::new() }

fn arithmetic<F>(km: &mut KyyMutator, l: Root<()>, r: Root<()>, f: F)
    -> Result<Root<()>, Error> where F: Fn(isize, isize) -> isize
{
    if let Some(l) = Int::downcast(km, l.clone()) {
        if let Some(r) = Int::downcast(km, r.clone()) {
            return Ok(unsafe { Int::new(km, Int(f(l.into(), r.into()))).unchecked_cast() });
        }
    }

    Err(Error::TypeError(vec![l, r]))
}

fn comparison<F>(km: &KyyMutator, l: Root<()>, r: Root<()>, f: F)
    -> Result<Root<()>, Error> where F: Fn(isize, isize) -> bool
{
    if let Some(l) = Int::downcast(km, l.clone()) {
        if let Some(r) = Int::downcast(km, r.clone()) {
            return Ok(unsafe { Bool::new(km, f(l.into(), r.into())).unchecked_cast() });
        }
    }

    Err(Error::TypeError(vec![l, r]))
} 

pub fn eval(km: &mut KyyMutator, env: &Env, expr: &ExprRef) -> EvalResult<Root<()>> {
    let res = match expr.value {
        Expr::Add(ref l, ref r) => {
            let l = eval(km, env, l)?;
            let r = eval(km, env, r)?;
            arithmetic(km, l, r, |l, r| l + r)
        },
        Expr::Sub(ref l, ref r) => {
            let l = eval(km, env, l)?;
            let r = eval(km, env, r)?;
            arithmetic(km, l, r, |l, r| l - r)
        },
        Expr::Mul(ref l, ref r) => {
            let l = eval(km, env, l)?;
            let r = eval(km, env, r)?;
            arithmetic(km, l, r, |l, r| l * r)
        },
        Expr::Div(ref l, ref r) => {
            let l = eval(km, env, l)?;
            let r = eval(km, env, r)?;
            arithmetic(km, l, r, |l, r| l / r)
        },

        Expr::Lt(ref l, ref r) => {
            let l = eval(km, env, l)?;
            let r = eval(km, env, r)?;
            comparison(km, l, r, |l, r| l < r)
        },
        Expr::Le(ref l, ref r) => {
            let l = eval(km, env, l)?;
            let r = eval(km, env, r)?;
            comparison(km, l, r, |l, r| l <= r)
        },
        Expr::Eq(ref l, ref r) => {
            let l = eval(km, env, l)?;
            let r = eval(km, env, r)?;
            comparison(km, l, r, |l, r| l == r)
        },
        Expr::Ne(ref l, ref r) => {
            let l = eval(km, env, l)?;
            let r = eval(km, env, r)?;
            comparison(km, l, r, |l, r| l != r)
        },
        Expr::Gt(ref l, ref r) => {
            let l = eval(km, env, l)?;
            let r = eval(km, env, r)?;
            comparison(km, l, r, |l, r| l > r)
        },
        Expr::Ge(ref l, ref r) => {
            let l = eval(km, env, l)?;
            let r = eval(km, env, r)?;
            comparison(km, l, r, |l, r| l >= r)
        },

        Expr::Var(ref name) => match env.get(&**name) {
            Some(v) => Ok(v.clone()),
            None => Err(Error::Unbound(name.clone()))
        },

        Expr::Const(ref c) => Ok(c.clone())
    };
    res.map_err(|err| expr.here(err))
}

fn exec_block(km: &mut KyyMutator, env: &mut Env, stmts: &[Stmt]) -> EvalResult<Option<Root<()>>> {
    let mut res = None;
    for stmt in stmts {
        res = exec(km, env, stmt)?;
    }
    Ok(res)
}

pub fn exec(km: &mut KyyMutator, env: &mut Env, stmt: &Stmt) -> EvalResult<Option<Root<()>>> {
    match stmt {
        Stmt::If {ref condition, ref conseq, ref alt} => {
            let cond = eval(km, env, condition)?;
            // HACK:
            if let Some(b) = Bool::downcast(km, cond.clone()) {
                if b.into() {
                    exec_block(km, env, conseq)
                } else {
                    exec_block(km, env, alt)
                }
            } else if let Some(n) = Int::downcast(km, cond) {
                if isize::from(n) != 0 {
                    exec_block(km, env, conseq)
                } else {
                    exec_block(km, env, alt)
                }
            } else {
                todo!()
            }
        },

        Stmt::Assign(ref var, ref rvalue) => {
            env.insert(var.clone(), eval(km, env, rvalue)?);
            Ok(None)
        },

        Stmt::Expr(ref expr) => eval(km, env, expr).map(Some)
    }
}

