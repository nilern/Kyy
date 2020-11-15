use super::parser::{Expr, ExprRef};

type Value = isize;

pub fn eval(expr: ExprRef) -> Value {
    match expr.value {
        Expr::Add(l, r) => eval(l) + eval(r),
        Expr::Sub(l, r) => eval(l) - eval(r),
        Expr::Mul(l, r) => eval(l) * eval(r),
        Expr::Div(l, r) => eval(l) / eval(r),
        Expr::Const(c) => c
    }
}

