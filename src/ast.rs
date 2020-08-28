use crate::{
    span::Spanned,
    token::Token,
    visit::{Accept, Visitor},
};

#[derive(Debug, Clone)]
pub struct Program {
    pub stmts: Vec<Stmt>,
}

pub type Stmt = Spanned<StmtTy>;
pub type Expr = Spanned<ExprTy>;
pub type Identifier = Spanned<Symbol>;

#[derive(Debug, Clone)]
pub enum StmtTy {
    Expression {
        expr: Expr,
    },
    Print {
        expr: Expr,
    },

    Var {
        binding: Identifier,
        init: Option<Expr>,
    },
    Block {
        exprs: Vec<Stmt>,
    },
    Class {
        ident: Identifier,
        super_: Option<Identifier>,
        functions: Vec<Stmt>,
    },

    Return {
        expr: Expr,
    },

    Function {
        ident: Identifier,
        params: Vec<Identifier>,
        block: Spanned<Vec<Stmt>>,
    },

    Conditional {
        if_: Expr,
        body: Box<Stmt>,
        else_: Option<Box<Stmt>>,
    },
    For {
        init: Option<Box<Stmt>>,
        cond: Option<Expr>,
        loop_: Option<Expr>,
        body: Box<Stmt>,
    },
    While {
        cond: Expr,
        stmt: Box<Stmt>,
    },
}

#[derive(Debug, Clone)]
pub enum ExprTy {
    Assignment {
        ident: Identifier,
        expr: Box<Expr>,
    },
    Binary {
        lhs: Box<Expr>,
        op: Spanned<BinaryOp>,
        rhs: Box<Expr>,
    },
    Call {
        expr: Box<Expr>,
        args: Vec<Expr>,
    },
    Get {
        obj: Box<Expr>,
        ident: Identifier,
    },
    Grouping {
        expr: Box<Expr>,
    },
    Literal {
        lit: Literal,
    },
    Logical {
        lhs: Box<Expr>,
        op: Spanned<LogicalOp>,
        rhs: Box<Expr>,
    },
    Set {
        obj: Box<Expr>,
        ident: Identifier,
        value: Box<Expr>,
    },
    Super {
        ident: Identifier,
    },
    Unary {
        op: Spanned<UnaryOp>,
        rhs: Box<Expr>,
    },
    Variable {
        ident: Identifier,
    },
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub binding: Identifier,
    pub expr: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct Symbol(pub Box<str>);

impl Symbol {
    pub fn new(s: impl Into<Box<str>>) -> Self {
        Self(s.into())
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Nil,
    This,
    Number(f64),
    String(Box<str>),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Not,
    Minus,
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Minus,
    Plus,
    Slash,
    Star,
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

impl BinaryOp {
    pub(super) fn from_token(tok: Token) -> Self {
        use Token::*;
        match tok {
            Minus => Self::Minus,
            Plus => Self::Plus,
            Slash => Self::Slash,
            Star => Self::Star,
            EqualEqual => Self::Eq,
            BangEqual => Self::NotEq,
            Less => Self::Less,
            LessEqual => Self::LessEq,
            Greater => Self::Greater,
            GreaterEqual => Self::GreaterEq,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LogicalOp {
    Or,
    And,
}

impl<'a> Accept<()> for crate::visit::Spanned<'a, Program> {
    fn accept(&self, visit: &mut dyn Visitor<()>) {
        self.item
            .stmts
            .iter()
            .for_each(|s| Spanned::as_ref(s).accept(visit))
    }
}

impl<'a> Accept<String> for crate::visit::Spanned<'a, Program> {
    fn accept(&self, visit: &mut dyn Visitor<String>) -> String {
        self.item
            .stmts
            .iter()
            .map(|s| Spanned::as_ref(s).accept(visit))
            .fold(String::new(), |mut a, c| {
                if !a.is_empty() {
                    a.push('\n');
                }
                a.push_str(&c);
                a
            })
    }
}

impl<'a, T> Accept<T> for crate::visit::Spanned<'a, ExprTy> {
    fn accept(&self, visit: &mut dyn Visitor<T>) -> T {
        match &self.item {
            ExprTy::Assignment { ident, expr } => {
                visit.visit_expr_assignment(
                    Spanned::as_ref(ident), //
                    Spanned::as_ref(expr),
                )
            }

            ExprTy::Binary { lhs, op, rhs } => visit.visit_expr_binary(
                Spanned::as_ref(lhs),
                Spanned::as_ref(op),
                Spanned::as_ref(rhs),
            ),

            ExprTy::Call { expr, args } => {
                let args: Vec<_> = args.iter().map(Spanned::as_ref).collect();
                visit.visit_expr_call(Spanned::as_ref(expr), &args)
            }

            ExprTy::Get { obj, ident } => {
                visit.visit_expr_get(Spanned::as_ref(obj), Spanned::as_ref(ident))
            }

            ExprTy::Grouping { expr } => visit.visit_expr_grouping(Spanned::as_ref(expr)),

            ExprTy::Literal { lit } => {
                let lit = crate::span::Spanned::new(lit.clone(), self.span);
                visit.visit_expr_literal(Spanned::as_ref(&lit))
            }

            ExprTy::Logical { lhs, op, rhs } => visit.visit_expr_logical(
                Spanned::as_ref(lhs),
                Spanned::as_ref(op),
                Spanned::as_ref(rhs),
            ),

            ExprTy::Set { obj, ident, value } => visit.visit_expr_set(
                Spanned::as_ref(obj),
                Spanned::as_ref(ident),
                Spanned::as_ref(value),
            ),

            ExprTy::Super { ident } => visit.visit_expr_super(Spanned::as_ref(ident)),

            ExprTy::Unary { op, rhs } => {
                visit.visit_expr_unary(Spanned::as_ref(op), Spanned::as_ref(rhs))
            }

            ExprTy::Variable { ident } => visit.visit_expr_variable(Spanned::as_ref(ident)),
        }
    }
}

impl<'a, T> Accept<T> for crate::visit::Spanned<'a, StmtTy> {
    fn accept(&self, visit: &mut dyn Visitor<T>) -> T {
        match &self.item {
            StmtTy::Expression { expr } => visit.visit_stmt_expression(Spanned::as_ref(expr)),

            StmtTy::Print { expr } => visit.visit_stmt_print(Spanned::as_ref(expr)),

            StmtTy::Var { binding, init } => {
                visit.visit_stmt_var(Spanned::as_ref(binding), init.as_ref().map(Spanned::as_ref))
            }

            StmtTy::Block { exprs } => {
                let exprs: Vec<_> = exprs.iter().map(Spanned::as_ref).collect();
                visit.visit_stmt_block(&*exprs)
            }

            StmtTy::Class {
                ident,
                super_,
                functions,
            } => {
                let super_ = super_.as_ref().map(Spanned::as_ref);
                let functions: Vec<_> = functions.iter().map(Spanned::as_ref).collect();
                visit.visit_stmt_class(Spanned::as_ref(ident), super_, &functions)
            }

            StmtTy::Return { expr } => visit.visit_stmt_return(Spanned::as_ref(expr)),

            StmtTy::Function {
                ident,
                params,
                block,
            } => {
                let params: Vec<_> = params.iter().map(Spanned::as_ref).collect();
                let block: Vec<_> = block.item.iter().map(Spanned::as_ref).collect();
                visit.visit_stmt_function(Spanned::as_ref(ident), &params, &block)
            }

            StmtTy::Conditional { if_, body, else_ } => {
                let else_ = else_.as_deref().map(Spanned::as_ref);
                visit.visit_stmt_conditional(Spanned::as_ref(if_), Spanned::as_ref(body), else_)
            }

            StmtTy::For {
                init,
                cond,
                loop_,
                body,
            } => {
                let init = init.as_deref().map(Spanned::as_ref);
                let cond = cond.as_ref().map(Spanned::as_ref);
                let loop_ = loop_.as_ref().map(Spanned::as_ref);
                visit.visit_stmt_for(init, cond, loop_, Spanned::as_ref(body))
            }

            StmtTy::While { cond, stmt } => {
                visit.visit_stmt_while(Spanned::as_ref(cond), Spanned::as_ref(stmt))
            }
        }
    }
}
