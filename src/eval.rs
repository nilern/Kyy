use std::collections::HashMap;

use super::orefs::Root;
use super::mutator::{KyyMutator, KyyType};
use super::object::Object;
use super::int::Int;
use super::bool::Bool;
use super::tuple::Tuple;
use super::string;
use super::ast::{self, Expr, Stmt};
use super::lexer::Spanning;

pub enum Error {
    TypeError(Vec<Root<Object>>),
    Unbound(Root<string::String>)
}

type EvalResult<T> = Result<T, Spanning<Error>>;

type Env = HashMap<String, Root<Object>>;

pub fn new_global_env() -> Env { HashMap::new() }

fn arithmetic<F>(km: &mut KyyMutator, l: Root<Object>, r: Root<Object>, f: F)
    -> Result<Root<Object>, Error> where F: Fn(isize, isize) -> isize
{
    if let Some(l) = Int::downcast(km, l.clone()) {
        if let Some(r) = Int::downcast(km, r.clone()) {
            return Ok(Int::new(km, f(l.into(), r.into())).as_obj());
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

pub fn eval(km: &mut KyyMutator, env: &Env, expr: Root<Expr>) -> EvalResult<Root<Object>> {
    use ast::*;

    let res = {
        let expr: Root<Object> = expr.clone().as_obj(); // HACK

        if let Some(expr) = Add::downcast(km, expr.clone()) {
            let l = expr.clone().left(km);
            let l = eval(km, env, l)?;
            let r = expr.right(km);
            let r = eval(km, env, r)?;
            arithmetic(km, l, r, |l, r| l + r)
        } else if let Some(expr) = Sub::downcast(km, expr.clone()) {
            let l = expr.clone().left(km);
            let l = eval(km, env, l)?;
            let r = expr.right(km);
            let r = eval(km, env, r)?;
            arithmetic(km, l, r, |l, r| l - r)
        } else if let Some(expr) = Mul::downcast(km, expr.clone()) {
            let l = expr.clone().left(km);
            let l = eval(km, env, l)?;
            let r = expr.right(km);
            let r = eval(km, env, r)?;
            arithmetic(km, l, r, |l, r| l * r)
        } else if let Some(expr) = Div::downcast(km, expr.clone()) {
            let l = expr.clone().left(km);
            let l = eval(km, env, l)?;
            let r = expr.right(km);
            let r = eval(km, env, r)?;
            arithmetic(km, l, r, |l, r| l / r)

        } else if let Some(expr) = Lt::downcast(km, expr.clone()) {
            let l = expr.clone().left(km);
            let l = eval(km, env, l)?;
            let r = expr.right(km);
            let r = eval(km, env, r)?;
            comparison(km, l, r, |l, r| l < r)
        } else if let Some(expr) = Le::downcast(km, expr.clone()) {
            let l = expr.clone().left(km);
            let l = eval(km, env, l)?;
            let r = expr.right(km);
            let r = eval(km, env, r)?;
            comparison(km, l, r, |l, r| l <= r)
        } else if let Some(expr) = Eq::downcast(km, expr.clone()) {
            let l = expr.clone().left(km);
            let l = eval(km, env, l)?;
            let r = expr.right(km);
            let r = eval(km, env, r)?;
            comparison(km, l, r, |l, r| l == r)
        } else if let Some(expr) = Ne::downcast(km, expr.clone()) {
            let l = expr.clone().left(km);
            let l = eval(km, env, l)?;
            let r = expr.right(km);
            let r = eval(km, env, r)?;
            comparison(km, l, r, |l, r| l != r)
        } else if let Some(expr) = Gt::downcast(km, expr.clone()) {
            let l = expr.clone().left(km);
            let l = eval(km, env, l)?;
            let r = expr.right(km);
            let r = eval(km, env, r)?;
            comparison(km, l, r, |l, r| l > r)
        } else if let Some(expr) = Ge::downcast(km, expr.clone()) {
            let l = expr.clone().left(km);
            let l = eval(km, env, l)?;
            let r = expr.right(km);
            let r = eval(km, env, r)?;
            comparison(km, l, r, |l, r| l >= r)

        } else if let Some(expr) = Var::downcast(km, expr.clone()) {
            let name = expr.name(km);
            match env.get(name.as_str()) {
                Some(v) => Ok(v.clone()),
                None => Err(Error::Unbound(name))
            }

        } else if let Some(expr) = Const::downcast(km, expr) {
            Ok(expr.value(km))

        } else {
            todo!()
        }
    };

    res.map_err(|err| expr.here(km, err))
}

fn exec_block(km: &mut KyyMutator, env: &mut Env, stmts: Root<Tuple>) -> EvalResult<Option<Root<Object>>> {
    let mut res = None;
    for i in 0..stmts.len() {
        let stmt = km.root(stmts.slots()[i]).unchecked_cast();
        res = exec(km, env, stmt)?;
    }
    Ok(res)
}

pub fn exec(km: &mut KyyMutator, env: &mut Env, stmt: Root<Stmt>) -> EvalResult<Option<Root<Object>>> {
    let stmt: Root<Object> = stmt.as_obj(); // HACK

    if let Some(stmt) = ast::If::downcast(km, stmt.clone()) {
        let cond = stmt.clone().condition(km);
        let cond = eval(km, env, cond)?;
        // HACK:
        if let Some(b) = Bool::downcast(km, cond.clone()) {
            if b.into() {
                let conseq = stmt.conseq(km);
                exec_block(km, env, conseq)
            } else {
                let alt = stmt.alt(km);
                exec_block(km, env, alt)
            }
        } else if let Some(n) = Int::downcast(km, cond) {
            if isize::from(n) != 0 {
                let conseq = stmt.conseq(km);
                exec_block(km, env, conseq)
            } else {
                let alt = stmt.alt(km);
                exec_block(km, env, alt)
            }
        } else {
            todo!()
        }
    } else if let Some(stmt) = ast::Assign::downcast(km, stmt.clone()) {
        let rvalue = stmt.clone().rvalue(km);
        let rvalue = eval(km, env, rvalue)?;
        let var = stmt.var(km);
        env.insert(var.as_str().to_string(), rvalue);
        Ok(None)
    } else if let Some(stmt) = ast::ExprStmt::downcast(km, stmt) {
        let expr = stmt.expr(km);
        eval(km, env, expr).map(Some)
    } else {
        todo!()
    }
}

