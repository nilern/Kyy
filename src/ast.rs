use std::ops::Range;

use super::object::Object;
use super::orefs::{Gc, Root};
use super::mutator::{KyySizedSlotsType, KyyMutator};
use super::string::String;
use super::tuple::Tuple;
use super::int::Int;

#[repr(C)]
pub struct Expr {
    filename: Gc<String>,
    start: Gc<Int>,
    end: Gc<Int>
}

impl Root<Expr> {
    pub fn filename(self, km: &KyyMutator) -> Root<String> { km.root(self.as_ref().filename) }

    pub fn start(self) -> isize { self.as_ref().start.into() }

    pub fn end(self) -> isize { self.as_ref().end.into() }
}

macro_rules! binary_node {
    ($name: ident) => {
        #[repr(C)]
        pub struct $name {
            base: Expr,
            left: Gc<Object>,
            right: Gc<Object>
        }

        unsafe impl KyySizedSlotsType for $name {}

        impl $name {
            pub fn new(km: &mut KyyMutator, filename: Root<String>, span: Range<isize>,
                 left: Root<Expr>, right: Root<Expr>
            ) -> Root<$name> {
                Self::alloc(km, Self {
                    base: Expr {
                        filename: filename.oref(),
                        start: Int::new(km, span.start).oref(),
                        end: Int::new(km, span.end).oref()
                    },
                    left: left.oref().as_obj(),
                    right: right.oref().as_obj()
                })
            }
        }

        impl From<Root<$name>> for Root<Expr> {
            fn from(expr: Root<$name>) -> Root<Expr> { expr.unchecked_cast() }
        }

        impl Root<$name> {
            pub fn left(self, km: &KyyMutator) -> Root<Object> { km.root(self.as_ref().left) }

            pub fn right(self, km: &KyyMutator) -> Root<Object> { km.root(self.as_ref().right) }
        }
    }
}

binary_node!(Add);
binary_node!(Sub);
binary_node!(Mul);
binary_node!(Div); // FIXME: __truediv__ vs. __floordiv__

binary_node!(Le);
binary_node!(Lt);
binary_node!(Eq);
binary_node!(Ne);
binary_node!(Gt);
binary_node!(Ge);

#[repr(C)]
pub struct Var {
    base: Expr,
    name: Gc<String>
}

unsafe impl KyySizedSlotsType for Var {}

impl Var {
    pub fn new(km: &mut KyyMutator, filename: &str, span: Range<isize>, name: &str) -> Root<Var> {
        Self::alloc(km, Var {
            base: Expr {
                filename: String::new(km, filename).oref(),
                start: Int::new(km, span.start).oref(),
                end: Int::new(km, span.end).oref()
            },
            name: String::new(km, name).oref()
        })
    }
}

impl From<Root<Var>> for Root<Expr> {
    fn from(expr: Root<Var>) -> Root<Expr> { expr.unchecked_cast() }
}

impl Root<Var> {
    pub fn name(self, km: &KyyMutator) -> Root<String> { km.root(self.as_ref().name) }
}

#[repr(C)]
pub struct Const {
    base: Expr,
    value: Gc<Object>
}

unsafe impl KyySizedSlotsType for Const {}

impl Const {
    pub fn new(km: &mut KyyMutator, filename: &str, span: Range<isize>, value: Root<Object>
    ) -> Root<Const> {
        Self::alloc(km, Const {
            base: Expr {
                filename: String::new(km, filename).oref(),
                start: Int::new(km, span.start).oref(),
                end: Int::new(km, span.end).oref(),
            },
            value: value.oref()
        })
    }
}

impl From<Root<Const>> for Root<Expr> {
    fn from(expr: Root<Const>) -> Root<Expr> { expr.unchecked_cast() }
}

impl Root<Const> {
    pub fn value(self, km: &KyyMutator) -> Root<Object> { km.root(self.as_ref().value) }
}

#[repr(C)]
pub struct Stmt {
    filename: Gc<String>,
    start: Gc<Int>,
    end: Gc<Int>
}

#[repr(C)]
pub struct ExprStmt {
    base: Stmt,
    expr: Gc<Expr>
}

unsafe impl KyySizedSlotsType for ExprStmt {}

impl ExprStmt {
    pub fn new(km: &mut KyyMutator, expr: Root<Expr>) -> Root<ExprStmt> {
        Self::alloc(km, ExprStmt {
            base: Stmt {
                filename: expr.filename(km).oref(),
                start: Int::new(km, expr.start()).oref(),
                end: Int::new(km, expr.end()).oref(),
            },
            expr: expr.oref(),
        })
    }
}

impl From<Root<ExprStmt>> for Root<Stmt> {
    fn from(expr: Root<ExprStmt>) -> Root<Stmt> { expr.unchecked_cast() }
}

#[repr(C)]
pub struct If {
    base: Stmt,
    condition: Gc<Expr>,
    conseq: Gc<Tuple>,
    alt: Gc<Tuple>
}

unsafe impl KyySizedSlotsType for If {}

impl If {
    pub fn new(km: &mut KyyMutator, filename: &str, span: Range<isize>,
        condition: Root<Expr>, conseq: Root<Tuple>, alt: Root<Tuple>
    ) -> Root<If> {
        Self::alloc(km, If {
            base: Stmt {
                filename: String::new(km, filename).oref(),
                start: Int::new(km, span.start).oref(),
                end: Int::new(km, span.end).oref(),
            },
            condition: condition.oref(),
            conseq: conseq.oref(),
            alt: alt.oref()
        })
    }
}

impl From<Root<If>> for Root<Stmt> {
    fn from(expr: Root<If>) -> Root<Stmt> { expr.unchecked_cast() }
}

#[repr(C)]
pub struct Assign {
    base: Stmt,
    left: Gc<String>,
    right: Gc<Object>,
}

unsafe impl KyySizedSlotsType for Assign {}

impl Assign {
    pub fn new(km: &mut KyyMutator, filename: &str, span: Range<isize>,
        lvalue: Root<String>, rvalue: Root<Expr>
    ) -> Root<Assign> {
        Self::alloc(km, Assign {
            base: Stmt {
                filename: String::new(km, filename).oref(),
                start: Int::new(km, span.start).oref(),
                end: Int::new(km, span.end).oref()
            },
            left: lvalue.oref(),
            right: rvalue.oref()
        })
    }
}

impl From<Root<Assign>> for Root<Stmt> {
    fn from(expr: Root<Assign>) -> Root<Stmt> { expr.unchecked_cast() }
}

