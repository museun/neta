use crate::ast::*;

pub type Spanned<'a, T> = crate::span::Spanned<&'a T>;

pub trait Visitor<T> {
    // expressions
    fn visit_expr_assignment(&mut self, ident: Spanned<Symbol>, expr: Spanned<ExprTy>) -> T;

    fn visit_expr_binary(
        &mut self,
        lhs: Spanned<ExprTy>,
        op: Spanned<BinaryOp>,
        rhs: Spanned<ExprTy>,
    ) -> T;

    fn visit_expr_call(&mut self, expr: Spanned<ExprTy>, exprs: &[Spanned<ExprTy>]) -> T;

    fn visit_expr_get(&mut self, obj: Spanned<ExprTy>, ident: Spanned<Symbol>) -> T;

    fn visit_expr_grouping(&mut self, expr: Spanned<ExprTy>) -> T;

    fn visit_expr_literal(&mut self, lit: Spanned<Literal>) -> T;

    fn visit_expr_logical(
        &mut self,
        lhs: Spanned<ExprTy>,
        op: Spanned<LogicalOp>,
        rhs: Spanned<ExprTy>,
    ) -> T;

    fn visit_expr_set(
        &mut self,
        obj: Spanned<ExprTy>,
        ident: Spanned<Symbol>,
        value: Spanned<ExprTy>,
    ) -> T;

    fn visit_expr_super(&mut self, ident: Spanned<Symbol>) -> T;

    fn visit_expr_unary(&mut self, op: Spanned<UnaryOp>, rhs: Spanned<ExprTy>) -> T;

    fn visit_expr_variable(&mut self, ident: Spanned<Symbol>) -> T;

    // statements
    fn visit_stmt_expression(&mut self, expr: Spanned<ExprTy>) -> T;

    fn visit_stmt_print(&mut self, expr: Spanned<ExprTy>) -> T;

    fn visit_stmt_var(&mut self, binding: Spanned<Symbol>, init: Option<Spanned<ExprTy>>) -> T;

    fn visit_stmt_block(&mut self, exprs: &[Spanned<StmtTy>]) -> T;

    fn visit_stmt_class(
        &mut self,
        ident: Spanned<Symbol>,
        super_: Option<Spanned<Symbol>>,
        functions: &[Spanned<StmtTy>],
    ) -> T;

    fn visit_stmt_return(&mut self, expr: Spanned<ExprTy>) -> T;

    fn visit_stmt_function(
        &mut self,
        ident: Spanned<Symbol>,
        params: &[Spanned<Symbol>],
        block: &[Spanned<StmtTy>],
    ) -> T;

    fn visit_stmt_conditional(
        &mut self,
        if_: Spanned<ExprTy>,
        body: Spanned<StmtTy>,
        else_: Option<Spanned<StmtTy>>,
    ) -> T;

    fn visit_stmt_for(
        &mut self,
        init: Option<Spanned<StmtTy>>,
        cond: Option<Spanned<ExprTy>>,
        loop_: Option<Spanned<ExprTy>>,
        body: Spanned<StmtTy>,
    ) -> T;

    fn visit_stmt_while(&mut self, cond: Spanned<ExprTy>, stmt: Spanned<StmtTy>) -> T;
}

pub trait Accept<T> {
    fn accept(&self, visit: &mut dyn Visitor<T>) -> T;
}

impl<S, T> Accept<T> for &S
where
    S: Accept<T>,
{
    fn accept(&self, visit: &mut dyn Visitor<T>) -> T {
        (*self).accept(visit)
    }
}
