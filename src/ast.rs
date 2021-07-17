use std::convert::TryInto;
use std::ops::Range;

use super::lexer::Spanning;
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

impl Expr {
    unsafe fn new(filename: Root<String>, start: Root<Int>, end: Root<Int>) -> Expr {
        Expr {
            filename: filename.oref(),
            start: start.oref(),
            end: end.oref()
        }
    }
}

impl Root<Expr> {
    pub fn filename(self, km: &mut KyyMutator) -> Root<String> { km.root(unsafe { self.as_ref().filename }) }

    pub fn start(self, km: &mut KyyMutator) -> Root<Int> { km.root(unsafe { self.as_ref().start }) }

    pub fn end(self, km: &mut KyyMutator) -> Root<Int> { km.root(unsafe { self.as_ref().end }) }

    pub fn here<T>(self, km: &mut KyyMutator, value: T) -> Spanning<T> {
        Spanning {
            filename: self.clone().filename(km),
            span: isize::from(self.clone().start(km)).try_into().unwrap()
                ..isize::from(self.end(km)).try_into().unwrap(),
            value
        }
    }
}

macro_rules! binary_node {
    ($name: ident) => {
        #[repr(C)]
        pub struct $name {
            base: Expr,
            left: Gc<Expr>,
            right: Gc<Expr>
        }

        unsafe impl KyySizedSlotsType for $name {}

        impl $name {
            pub fn new(km: &mut KyyMutator, filename: Root<String>,
                 left: Root<Expr>, right: Root<Expr>
            ) -> Root<$name> {
                let start = left.clone().start(km);
                let end = right.clone().end(km);
                unsafe {
                    let base = Expr::new(filename, start, end);
                    Self::alloc(km, Self {base, left: left.oref(), right: right.oref()})
                }
            }
        }

        impl From<Root<$name>> for Root<Expr> {
            fn from(expr: Root<$name>) -> Root<Expr> { unsafe { expr.unchecked_cast() } }
        }

        impl Root<$name> {
            pub fn left(self, km: &mut KyyMutator) -> Root<Expr> { km.root(unsafe { self.as_ref().left }) }

            pub fn right(self, km: &mut KyyMutator) -> Root<Expr> { km.root(unsafe { self.as_ref().right }) }
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
    pub fn new(km: &mut KyyMutator, filename: Root<String>, span: Range<usize>, name: &str) -> Root<Var> {
        let start = Int::new(km, span.start.try_into().unwrap());
        let end = Int::new(km, span.end.try_into().unwrap());
        unsafe {
            let base = Expr::new(filename, start, end);
            let name = String::new(km, name).oref();
            Self::alloc(km, Var {base, name})
        }
    }
}

impl From<Root<Var>> for Root<Expr> {
    fn from(expr: Root<Var>) -> Root<Expr> { unsafe { expr.unchecked_cast() } }
}

impl Root<Var> {
    pub fn name(self, km: &mut KyyMutator) -> Root<String> { km.root(unsafe { self.as_ref().name }) }
}

#[repr(C)]
pub struct Const {
    base: Expr,
    value: Gc<Object>
}

unsafe impl KyySizedSlotsType for Const {}

impl Const {
    pub fn new(km: &mut KyyMutator, filename: Root<String>, span: Range<usize>, value: Root<Object>
    ) -> Root<Const> {
        let start = Int::new(km, span.start.try_into().unwrap());
        let end = Int::new(km, span.end.try_into().unwrap());
        unsafe {
            let base = Expr::new(filename, start, end);
            Self::alloc(km, Const {base, value: value.oref()})
        }
    }
}

impl From<Root<Const>> for Root<Expr> {
    fn from(expr: Root<Const>) -> Root<Expr> { unsafe { expr.unchecked_cast() } }
}

impl Root<Const> {
    pub fn value(self, km: &mut KyyMutator) -> Root<Object> { km.root(unsafe { self.as_ref().value }) }
}

#[repr(C)]
pub struct Stmt {
    filename: Gc<String>,
    start: Gc<Int>,
    end: Gc<Int>
}

impl Stmt {
    unsafe fn new(filename: Root<String>, start: Root<Int>, end: Root<Int>) -> Stmt {
        Stmt {
            filename: filename.oref(),
            start: start.oref(),
            end: end.oref()
        }
    }
}

impl Root<Stmt> {
    pub fn filename(self, km: &mut KyyMutator) -> Root<String> { km.root(unsafe { self.as_ref().filename }) }

    pub fn start(self, km: &mut KyyMutator) -> Root<Int> { km.root(unsafe { self.as_ref().start }) }

    pub fn end(self, km: &mut KyyMutator) -> Root<Int> { km.root(unsafe { self.as_ref().end }) }
}

#[repr(C)]
pub struct ExprStmt {
    base: Stmt,
    expr: Gc<Expr>
}

unsafe impl KyySizedSlotsType for ExprStmt {}

impl ExprStmt {
    pub fn new(km: &mut KyyMutator, expr: Root<Expr>) -> Root<ExprStmt> {
        let filename = expr.clone().filename(km);
        let start = expr.clone().start(km);
        let end = expr.clone().end(km);
        unsafe {
            Self::alloc(km, ExprStmt {
                base: Stmt::new(filename, start, end),
                expr: expr.oref()
            })
        }
    }
}

impl From<Root<ExprStmt>> for Root<Stmt> {
    fn from(expr: Root<ExprStmt>) -> Root<Stmt> { unsafe { expr.unchecked_cast() } }
}

impl Root<ExprStmt> {
    pub fn expr(self, km: &mut KyyMutator) -> Root<Expr> { km.root(unsafe { self.as_ref().expr }) }
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
    pub fn new(km: &mut KyyMutator, filename: Root<String>, start: usize, end: Root<Int>,
        condition: Root<Expr>, conseq: Root<Tuple>, alt: Root<Tuple>
    ) -> Root<If> {
        let start = Int::new(km, start.try_into().unwrap());
        unsafe {
            Self::alloc(km, If {
                base: Stmt::new(filename, start, end),
                condition: condition.oref(),
                conseq: conseq.oref(),
                alt: alt.oref()
            })
        }
    }
}

impl From<Root<If>> for Root<Stmt> {
    fn from(expr: Root<If>) -> Root<Stmt> { unsafe { expr.unchecked_cast() } }
}

impl Root<If> {
    pub fn condition(self, km: &mut KyyMutator) -> Root<Expr> { km.root(unsafe { self.as_ref().condition }) }

    pub fn conseq(self, km: &mut KyyMutator) -> Root<Tuple> { km.root(unsafe { self.as_ref().conseq }) }

    pub fn alt(self, km: &mut KyyMutator) -> Root<Tuple> { km.root(unsafe { self.as_ref().alt }) }
}

#[repr(C)]
pub struct Assign {
    base: Stmt,
    left: Gc<String>,
    right: Gc<Expr>,
}

unsafe impl KyySizedSlotsType for Assign {}

impl Assign {
    pub fn new(km: &mut KyyMutator, filename: Root<String>, span: Range<usize>,
        lvalue: Root<String>, rvalue: Root<Expr>
    ) -> Root<Assign> {
        let start = Int::new(km, span.start.try_into().unwrap());
        let end = Int::new(km, span.end.try_into().unwrap());
        unsafe {
            Self::alloc(km, Assign {
                base: Stmt::new(filename, start, end),
                left: lvalue.oref(),
                right: rvalue.oref()
            })
        }
    }
}

impl From<Root<Assign>> for Root<Stmt> {
    fn from(expr: Root<Assign>) -> Root<Stmt> { unsafe { expr.unchecked_cast() } }
}

impl Root<Assign> {
    pub fn var(self, km: &mut KyyMutator) -> Root<String> { km.root(unsafe { self.as_ref().left }) }

    pub fn rvalue(self, km: &mut KyyMutator) -> Root<Expr> { km.root(unsafe { self.as_ref().right }) }
}

