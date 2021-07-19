use std::convert::TryInto;
use std::ops::Range;

use super::handle::Handle;
use super::lexer::Spanning;
use super::object::Object;
use super::orefs::ObjectPtr;
use super::mutator::{KyySizedSlotsType, KyyMutator};
use super::string::String;
use super::tuple::Tuple;
use super::int::Int;

// TODO: Compatibility with CPython `ast` module

#[repr(C)]
pub struct Expr {
    filename: ObjectPtr<String>,
    start: ObjectPtr<Int>,
    end: ObjectPtr<Int>
}

impl Expr {
    unsafe fn new(filename: Handle<String>, start: Handle<Int>, end: Handle<Int>) -> Expr {
        Expr {
            filename: filename.oref(),
            start: start.oref(),
            end: end.oref()
        }
    }
}

impl Handle<Expr> {
    pub fn filename(self, km: &mut KyyMutator) -> Handle<String> { km.root(unsafe { self.as_ref().filename }) }

    pub fn start(self, km: &mut KyyMutator) -> Handle<Int> { km.root(unsafe { self.as_ref().start }) }

    pub fn end(self, km: &mut KyyMutator) -> Handle<Int> { km.root(unsafe { self.as_ref().end }) }

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
            left: ObjectPtr<Expr>,
            right: ObjectPtr<Expr>
        }

        unsafe impl KyySizedSlotsType for $name {}

        impl $name {
            pub fn new(km: &mut KyyMutator, filename: Handle<String>,
                 left: Handle<Expr>, right: Handle<Expr>
            ) -> Handle<$name> {
                let start = left.clone().start(km);
                let end = right.clone().end(km);
                unsafe {
                    let base = Expr::new(filename, start, end);
                    Self::alloc(km, Self {base, left: left.oref(), right: right.oref()})
                }
            }
        }

        impl From<Handle<$name>> for Handle<Expr> {
            fn from(expr: Handle<$name>) -> Handle<Expr> { unsafe { expr.unchecked_cast() } }
        }

        impl Handle<$name> {
            pub fn left(self, km: &mut KyyMutator) -> Handle<Expr> { km.root(unsafe { self.as_ref().left }) }

            pub fn right(self, km: &mut KyyMutator) -> Handle<Expr> { km.root(unsafe { self.as_ref().right }) }
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
    name: ObjectPtr<String>
}

unsafe impl KyySizedSlotsType for Var {}

impl Var {
    pub fn new(km: &mut KyyMutator, filename: Handle<String>, span: Range<usize>, name: &str) -> Handle<Var> {
        let start = Int::new(km, span.start.try_into().unwrap());
        let end = Int::new(km, span.end.try_into().unwrap());
        unsafe {
            let base = Expr::new(filename, start, end);
            let name = String::new(km, name).oref();
            Self::alloc(km, Var {base, name})
        }
    }
}

impl From<Handle<Var>> for Handle<Expr> {
    fn from(expr: Handle<Var>) -> Handle<Expr> { unsafe { expr.unchecked_cast() } }
}

impl Handle<Var> {
    pub fn name(self, km: &mut KyyMutator) -> Handle<String> { km.root(unsafe { self.as_ref().name }) }
}

#[repr(C)]
pub struct Const {
    base: Expr,
    value: ObjectPtr<Object>
}

unsafe impl KyySizedSlotsType for Const {}

impl Const {
    pub fn new(km: &mut KyyMutator, filename: Handle<String>, span: Range<usize>, value: Handle<Object>
    ) -> Handle<Const> {
        let start = Int::new(km, span.start.try_into().unwrap());
        let end = Int::new(km, span.end.try_into().unwrap());
        unsafe {
            let base = Expr::new(filename, start, end);
            Self::alloc(km, Const {base, value: value.oref()})
        }
    }
}

impl From<Handle<Const>> for Handle<Expr> {
    fn from(expr: Handle<Const>) -> Handle<Expr> { unsafe { expr.unchecked_cast() } }
}

impl Handle<Const> {
    pub fn value(self, km: &mut KyyMutator) -> Handle<Object> { km.root(unsafe { self.as_ref().value }) }
}

#[repr(C)]
pub struct Stmt {
    filename: ObjectPtr<String>,
    start: ObjectPtr<Int>,
    end: ObjectPtr<Int>
}

impl Stmt {
    unsafe fn new(filename: Handle<String>, start: Handle<Int>, end: Handle<Int>) -> Stmt {
        Stmt {
            filename: filename.oref(),
            start: start.oref(),
            end: end.oref()
        }
    }
}

impl Handle<Stmt> {
    pub fn filename(self, km: &mut KyyMutator) -> Handle<String> { km.root(unsafe { self.as_ref().filename }) }

    pub fn start(self, km: &mut KyyMutator) -> Handle<Int> { km.root(unsafe { self.as_ref().start }) }

    pub fn end(self, km: &mut KyyMutator) -> Handle<Int> { km.root(unsafe { self.as_ref().end }) }
}

#[repr(C)]
pub struct ExprStmt {
    base: Stmt,
    expr: ObjectPtr<Expr>
}

unsafe impl KyySizedSlotsType for ExprStmt {}

impl ExprStmt {
    pub fn new(km: &mut KyyMutator, expr: Handle<Expr>) -> Handle<ExprStmt> {
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

impl From<Handle<ExprStmt>> for Handle<Stmt> {
    fn from(expr: Handle<ExprStmt>) -> Handle<Stmt> { unsafe { expr.unchecked_cast() } }
}

impl Handle<ExprStmt> {
    pub fn expr(self, km: &mut KyyMutator) -> Handle<Expr> { km.root(unsafe { self.as_ref().expr }) }
}

#[repr(C)]
pub struct If {
    base: Stmt,
    condition: ObjectPtr<Expr>,
    conseq: ObjectPtr<Tuple>,
    alt: ObjectPtr<Tuple>
}

unsafe impl KyySizedSlotsType for If {}

impl If {
    pub fn new(km: &mut KyyMutator, filename: Handle<String>, start: usize, end: Handle<Int>,
        condition: Handle<Expr>, conseq: Handle<Tuple>, alt: Handle<Tuple>
    ) -> Handle<If> {
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

impl From<Handle<If>> for Handle<Stmt> {
    fn from(expr: Handle<If>) -> Handle<Stmt> { unsafe { expr.unchecked_cast() } }
}

impl Handle<If> {
    pub fn condition(self, km: &mut KyyMutator) -> Handle<Expr> { km.root(unsafe { self.as_ref().condition }) }

    pub fn conseq(self, km: &mut KyyMutator) -> Handle<Tuple> { km.root(unsafe { self.as_ref().conseq }) }

    pub fn alt(self, km: &mut KyyMutator) -> Handle<Tuple> { km.root(unsafe { self.as_ref().alt }) }
}

#[repr(C)]
pub struct Assign {
    base: Stmt,
    left: ObjectPtr<String>,
    right: ObjectPtr<Expr>,
}

unsafe impl KyySizedSlotsType for Assign {}

impl Assign {
    pub fn new(km: &mut KyyMutator, filename: Handle<String>, span: Range<usize>,
        lvalue: Handle<String>, rvalue: Handle<Expr>
    ) -> Handle<Assign> {
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

impl From<Handle<Assign>> for Handle<Stmt> {
    fn from(expr: Handle<Assign>) -> Handle<Stmt> { unsafe { expr.unchecked_cast() } }
}

impl Handle<Assign> {
    pub fn var(self, km: &mut KyyMutator) -> Handle<String> { km.root(unsafe { self.as_ref().left }) }

    pub fn rvalue(self, km: &mut KyyMutator) -> Handle<Expr> { km.root(unsafe { self.as_ref().right }) }
}

