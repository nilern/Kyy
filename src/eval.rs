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
        let expr: Root<Object> = expr.as_obj(); // HACK

        if let Some(expr) = Add::downcast(km, expr) {
            let l = eval(km, env, expr.left(km))?;
            let r = eval(km, env, expr.right(km))?;
            arithmetic(km, l, r, |l, r| l + r)
        } else if let Some(expr) = Sub::downcast(km, expr) {
            let l = eval(km, env, expr.left(km))?;
            let r = eval(km, env, expr.right(km))?;
            arithmetic(km, l, r, |l, r| l - r)
        } else if let Some(expr) = Mul::downcast(km, expr) {
            let l = eval(km, env, expr.left(km))?;
            let r = eval(km, env, expr.right(km))?;
            arithmetic(km, l, r, |l, r| l * r)
        } else if let Some(expr) = Div::downcast(km, expr) {
            let l = eval(km, env, expr.left(km))?;
            let r = eval(km, env, expr.right(km))?;
            arithmetic(km, l, r, |l, r| l / r)

        } else if let Some(expr) = Lt::downcast(km, expr) {
            let l = eval(km, env, expr.left(km))?;
            let r = eval(km, env, expr.right(km))?;
            comparison(km, l, r, |l, r| l < r)
        } else if let Some(expr) = Le::downcast(km, expr) {
            let l = eval(km, env, expr.left(km))?;
            let r = eval(km, env, expr.right(km))?;
            comparison(km, l, r, |l, r| l <= r)
        } else if let Some(expr) = Eq::downcast(km, expr) {
            let l = eval(km, env, expr.left(km))?;
            let r = eval(km, env, expr.right(km))?;
            comparison(km, l, r, |l, r| l == r)
        } else if let Some(expr) = Ne::downcast(km, expr) {
            let l = eval(km, env, expr.left(km))?;
            let r = eval(km, env, expr.right(km))?;
            comparison(km, l, r, |l, r| l != r)
        } else if let Some(expr) = Gt::downcast(km, expr) {
            let l = eval(km, env, expr.left(km))?;
            let r = eval(km, env, expr.right(km))?;
            comparison(km, l, r, |l, r| l > r)
        } else if let Some(expr) = Ge::downcast(km, expr) {
            let l = eval(km, env, expr.left(km))?;
            let r = eval(km, env, expr.right(km))?;
            comparison(km, l, r, |l, r| l >= r)

        } else if let Some(expr) = Var::downcast(km, expr) {
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
        res = exec(km, env, km.root(stmts.slots()[i]).unchecked_cast())?;
    }
    Ok(res)
}

pub fn exec(km: &mut KyyMutator, env: &mut Env, stmt: Root<Stmt>) -> EvalResult<Option<Root<Object>>> {
    let stmt: Root<Object> = stmt.as_obj(); // HACK

    if let Some(stmt) = ast::If::downcast(km, stmt) {
        let cond = eval(km, env, stmt.condition(km))?;
        // HACK:
        if let Some(b) = Bool::downcast(km, cond) {
            if b.into() {
                exec_block(km, env, stmt.conseq(km))
            } else {
                exec_block(km, env, stmt.alt(km))
            }
        } else if let Some(n) = Int::downcast(km, cond) {
            if isize::from(n) != 0 {
                exec_block(km, env, stmt.conseq(km))
            } else {
                exec_block(km, env, stmt.alt(km))
            }
        } else {
            todo!()
        }
    } else if let Some(stmt) = ast::Assign::downcast(km, stmt) {
        env.insert(stmt.var(km).as_str().to_string(), eval(km, env, stmt.rvalue(km))?);
        Ok(None)
    } else if let Some(stmt) = ast::ExprStmt::downcast(km, stmt) {
        eval(km, env, stmt.expr(km)).map(Some)
    } else {
        todo!()
    }
}

