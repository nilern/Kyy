use std::sync::Arc;

use super::lexer::Spanning;
use super::object::Object;
use super::orefs::Root;

pub enum Expr {
    Add(ExprRef, ExprRef),
    Sub(ExprRef, ExprRef),
    Mul(ExprRef, ExprRef),
    Div(ExprRef, ExprRef), // FIXME: __truediv__ vs. __floordiv__

    Lt(ExprRef, ExprRef),
    Le(ExprRef, ExprRef),
    Eq(ExprRef, ExprRef),
    Ne(ExprRef, ExprRef),
    Gt(ExprRef, ExprRef),
    Ge(ExprRef, ExprRef),

    Var(Arc<String>),
    Const(Root<Object>)
}

pub type ExprRef = Box<Spanning<Expr>>;

pub enum Stmt {
    If {
        condition: ExprRef,
        conseq: Vec<Stmt>,
        alt: Vec<Stmt>
    },

    Assign(String, ExprRef),
    Expr(ExprRef)
}

