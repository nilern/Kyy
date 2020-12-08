use std::collections::HashMap;
use std::sync::Arc;

use super::orefs::Root;
use super::mutator::{KyyMutator, KyyType, KyySizedBytesType};
use super::object::Object;
use super::int::Int;
use super::bool::Bool;
use super::tuple::Tuple;
use super::ast;
use super::lexer::Spanning;

pub enum Error {
    TypeError(Vec<Root<Object>>),
    Unbound(Arc<String>)
}

type EvalResult<T> = Result<T, Spanning<Error>>;

type Env = HashMap<String, Root<Object>>;

pub fn new_global_env() -> Env { HashMap::new() }

fn arithmetic<F>(km: &mut KyyMutator, l: Root<Object>, r: Root<Object>, f: F)
    -> Result<Root<Object>, Error> where F: Fn(isize, isize) -> isize
{
    if let Some(l) = Int::downcast(km, l.clone()) {
        if let Some(r) = Int::downcast(km, r.clone()) {
            return Ok(Int::new(km, Int(f(l.into(), r.into()))).as_obj());
        }
    }

    Err(Error::TypeError(vec![l, r]))
}

fn comparison<F>(km: &KyyMutator, l: Root<Object>, r: Root<Object>, f: F)
    -> Result<Root<Object>, Error> where F: Fn(isize, isize) -> bool
{
    if let Some(l) = Int::downcast(km, l.clone()) {
        if let Some(r) = Int::downcast(km, r.clone()) {
            return Ok(Bool::new(km, f(l.into(), r.into())).as_obj());
        }
    }

    Err(Error::TypeError(vec![l, r]))
} 

pub fn eval(km: &mut KyyMutator, env: &Env, expr: Root<Object>) -> EvalResult<Root<Object>> {
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

fn exec_block(km: &mut KyyMutator, env: &mut Env, stmts: Root<Tuple>) -> EvalResult<Option<Root<Object>>> {
    let mut res = None;
    for i in 0..stmts.as_ref().len() {
        res = exec(km, env, km.root(stmts.as_ref().slots()[i]))?;
    }
    Ok(res)
}

pub fn exec(km: &mut KyyMutator, env: &mut Env, stmt: Root<Object>) -> EvalResult<Option<Root<Object>>> {
    if let Some(ast::If {condition, conseq, alt}) = ast::If::downcast(km, stmt)
        .map(|root| unsafe { *root.as_ref() })
    {
        let conseq = km.root(conseq);
        let alt = km.root(alt);
        let cond = eval(km, env, km.root(condition))?;
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
    } else if let Some(ast::Assign {left: var, right: rvalue}) = ast::Assign::downcast(km, stmt)
        .map(|root| unsafe { *root.as_ref() })
    {
        env.insert(var.clone().as_ref().as_str().to_string(), eval(km, env, km.root(rvalue))?);
        Ok(None)
    } else {
        eval(km, env, stmt).map(Some)
    }
}

