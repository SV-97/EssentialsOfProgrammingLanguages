#![allow(dead_code)]
use std::convert::{TryFrom, TryInto};

type Error = &'static str;
type Var = &'static str;
type LexicalAdress = usize;
#[derive(Debug, Eq, PartialEq, Clone)]
struct StaticEnv(Vec<Var>); // The envs should be something like a Rc<LinkedList<Var>>
                            // (probably a custom implementation of som sort)
#[derive(Debug, Eq, PartialEq, Clone)]
struct RuntimeEnv(Vec<Value>);

#[derive(Debug, Eq, PartialEq, Clone)]
enum Value {
    Thunk(RuntimeEnv, NExpr),
    Num(i32),
    Bool(bool),
    List(Vec<Value>),
    Procedure(RuntimeEnv, NExpr),
}

impl TryFrom<Value> for i32 {
    type Error = &'static str;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Num(i) => Ok(i),
            Value::Thunk(env, expr) => expr.value(&env).and_then(TryInto::try_into),
            _ => Err("Invalid Conversion"),
        }
    }
}

impl TryFrom<Value> for bool {
    type Error = &'static str;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Bool(b) => Ok(b),
            Value::Thunk(env, expr) => expr.value(&env).and_then(TryInto::try_into),
            _ => Err("Invalid Conversion"),
        }
    }
}

impl TryFrom<Value> for Vec<Value> {
    type Error = &'static str;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::List(l) => Ok(l),
            Value::Thunk(env, expr) => expr.value(&env).and_then(TryInto::try_into),
            _ => Err("Invalid Conversion"),
        }
    }
}

impl TryFrom<Value> for (RuntimeEnv, NExpr) {
    type Error = &'static str;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Procedure(env, expr) => Ok((env, expr)),
            Value::Thunk(env, expr) => expr.value(&env).and_then(TryInto::try_into),
            _ => Err("Invalid Conversion"),
        }
    }
}

impl StaticEnv {
    pub fn empty() -> Self {
        StaticEnv(Vec::new())
    }

    pub fn extend(&mut self, var: Var) -> &mut Self {
        self.0.push(var);
        self
    }

    pub fn apply(&self, var: Var) -> Result<LexicalAdress, Error> {
        self.0
            .iter()
            .position(|x| x == &var)
            .ok_or("Couldn't find var in StaticEnv")
    }
}

impl RuntimeEnv {
    pub fn empty() -> Self {
        RuntimeEnv(Vec::new())
    }

    pub fn extend(&mut self, var: Value) -> &mut Self {
        self.0.push(var);
        self
    }

    pub fn apply(&self, index: LexicalAdress) -> Result<Value, Error> {
        self.0
            .get(index)
            .map(Clone::clone)
            .ok_or("Invalid Identifier")
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Expr {
    Const(i32),
    Diff(Box<Expr>, Box<Expr>),
    Var(Var),
    IsZero(Box<Expr>),
    Let(Var, Box<Expr>, Box<Expr>),
    Lambda(Var, Box<Expr>),
    Application(Box<Expr>, Box<Expr>),
}

impl Expr {
    #[inline]
    pub fn new_const(i: i32) -> Self {
        Expr::Const(i)
    }
    #[inline]
    pub fn new_diff(a: Self, b: Self) -> Self {
        Expr::Diff(Box::new(a), Box::new(b))
    }
    #[inline]
    pub fn new_var(a: Var) -> Self {
        Expr::Var(a)
    }
    #[inline]
    pub fn new_is_zero(a: Self) -> Self {
        Expr::IsZero(Box::new(a))
    }
    #[inline]
    pub fn new_let(var: Var, a: Self, b: Self) -> Self {
        Expr::Let(var, Box::new(a), Box::new(b))
    }
    #[inline]
    pub fn new_lambda(var: Var, a: Self) -> Self {
        Expr::Lambda(var, Box::new(a))
    }
    #[inline]
    pub fn new_application(a: Self, b: Self) -> Self {
        Expr::Application(Box::new(a), Box::new(b))
    }
}

fn lifted_m2<A, B, C, E, F>(f: F, ma: Result<A, E>, mb: Result<B, E>) -> Result<C, E>
where
    F: FnOnce(A, B) -> C,
{
    ma.and_then(|a| mb.map(|b| f(a, b)))
}

impl Expr {
    fn into_nameless(self, env: &StaticEnv) -> Result<NExpr, Error> {
        match self {
            Expr::Const(n) => Ok(NExpr::Const(n)),
            Expr::Diff(exp1, exp2) => {
                let l = exp1.into_nameless(env);
                let r = exp2.into_nameless(env);
                lifted_m2(NExpr::new_diff, l, r)
            }
            Expr::Var(var) => env.apply(&var).map(NExpr::Var),
            Expr::IsZero(exp1) => exp1.into_nameless(env).map(NExpr::new_is_zero),
            Expr::Let(var, exp1, exp2) => {
                let mut inner_env = env.clone();
                lifted_m2(
                    |a, b| NExpr::Let(Box::new(a), Box::new(b)),
                    exp1.into_nameless(env),
                    exp2.into_nameless(inner_env.extend(var)),
                )
            }
            Expr::Lambda(var, body) => {
                let mut inner_env = env.clone();
                body.into_nameless(inner_env.extend(var))
                    .map(|x| NExpr::Lambda(Box::new(x)))
            }
            Expr::Application(exp1, exp2) => lifted_m2(
                |a, b| NExpr::Application(Box::new(a), Box::new(b)),
                exp1.into_nameless(env),
                exp2.into_nameless(env),
            ),
        }
    }
}

// NamelessExpression
#[derive(Debug, Eq, PartialEq, Clone)]
enum NExpr {
    Const(i32),
    Diff(Box<NExpr>, Box<NExpr>),
    Var(LexicalAdress),
    IsZero(Box<NExpr>),
    Let(Box<NExpr>, Box<NExpr>),
    Lambda(Box<NExpr>),
    Application(Box<NExpr>, Box<NExpr>),
}

impl NExpr {
    #[inline]
    pub fn new_const(i: i32) -> Self {
        NExpr::Const(i)
    }
    #[inline]
    pub fn new_diff(a: Self, b: Self) -> Self {
        NExpr::Diff(Box::new(a), Box::new(b))
    }
    #[inline]
    pub fn new_var(a: LexicalAdress) -> Self {
        NExpr::Var(a)
    }
    #[inline]
    pub fn new_is_zero(a: Self) -> Self {
        NExpr::IsZero(Box::new(a))
    }
    #[inline]
    pub fn new_let(a: Self, b: Self) -> Self {
        NExpr::Let(Box::new(a), Box::new(b))
    }
    #[inline]
    pub fn new_lambda(a: Self) -> Self {
        NExpr::Lambda(Box::new(a))
    }
    #[inline]
    pub fn new_application(a: Self, b: Self) -> Self {
        NExpr::Application(Box::new(a), Box::new(b))
    }

    pub fn value(self, env: &RuntimeEnv) -> Result<Value, Error> {
        match self {
            NExpr::Const(n) => Ok(Value::Num(n)),
            NExpr::Diff(expr1, expr2) => {
                let v1 = expr1.value(env).and_then(i32::try_from);
                let v2 = expr2.value(env).and_then(i32::try_from);
                lifted_m2(|a, b| Value::Num(a - b), v1, v2)
            }
            NExpr::Var(adr) => env.apply(adr),
            NExpr::IsZero(expr) => {
                let val = expr.value(env);
                val.map(|x| Value::Bool(x == Value::Num(0)))
            }
            NExpr::Let(expr1, expr2) => {
                let mut inner_env = env.clone();
                expr2.value(inner_env.extend(Value::Thunk(env.clone(), *expr1)))
            }
            NExpr::Lambda(expr1) => Ok(Value::Procedure(env.clone(), *expr1)),
            NExpr::Application(f, arg) => {
                let (mut f_env, body) = <(RuntimeEnv, NExpr)>::try_from(f.value(env)?)?;
                let inner_env = f_env.extend(Value::Thunk(env.clone(), *arg));
                body.value(inner_env)
            }
        }
    }
}

fn main() {
    let program = {
        Expr::new_let(
            "add",
            Expr::new_lambda(
                "a",
                Expr::new_lambda(
                    "b",
                    Expr::new_diff(
                        Expr::new_var("a"),
                        Expr::new_diff(Expr::new_const(0), Expr::new_var("b")),
                    ),
                ),
            ),
            Expr::new_application(
                Expr::new_application(Expr::new_var("add"), Expr::new_const(5)),
                Expr::new_const(10),
            ),
        )
    };
    dbg!(&program);
    let env = StaticEnv::empty();
    let runtime_env = RuntimeEnv::empty();
    dbg!(program.clone().into_nameless(&env).unwrap());
    dbg!(program
        .into_nameless(&env)
        .unwrap()
        .value(&runtime_env)
        .unwrap());
}
