use crate::ast::*;
use crate::visit::{Accept, Spanned, Visitor};

#[derive(Default)]
pub struct Spanner {
    spans: Vec<crate::span::Span>,
}

impl Spanner {
    fn push<T>(&mut self, s: Spanned<T>) {
        self.spans.push(s.span)
    }

    pub fn walk_tree(mut self, accept: impl Accept<()>) -> Vec<crate::span::Span> {
        accept.accept(&mut self);
        self.spans
    }
}

impl Visitor<()> for Spanner {
    fn visit_expr_assignment(&mut self, ident: Spanned<Symbol>, expr: Spanned<ExprTy>) {
        self.push(ident);
        self.push(expr);
    }

    fn visit_expr_binary(
        &mut self,
        lhs: Spanned<ExprTy>,
        op: Spanned<BinaryOp>,
        rhs: Spanned<ExprTy>,
    ) {
        self.push(lhs);
        self.push(op);
        self.push(rhs);
    }

    fn visit_expr_call(&mut self, expr: Spanned<ExprTy>, exprs: &[Spanned<ExprTy>]) {
        self.push(expr);
        for expr in exprs {
            self.push(*expr);
        }
    }

    fn visit_expr_get(&mut self, obj: Spanned<ExprTy>, ident: Spanned<Symbol>) {
        self.push(obj);
        self.push(ident);
    }

    fn visit_expr_grouping(&mut self, expr: Spanned<ExprTy>) {
        self.push(expr);
    }

    fn visit_expr_literal(&mut self, lit: Spanned<Literal>) {
        self.push(lit)
    }

    fn visit_expr_logical(
        &mut self,
        lhs: Spanned<ExprTy>,
        op: Spanned<LogicalOp>,
        rhs: Spanned<ExprTy>,
    ) {
        self.push(lhs);
        self.push(op);
        self.push(rhs);
    }

    fn visit_expr_set(
        &mut self,
        obj: Spanned<ExprTy>,
        ident: Spanned<Symbol>,
        value: Spanned<ExprTy>,
    ) {
        self.push(obj);
        self.push(ident);
        self.push(value);
    }

    fn visit_expr_super(&mut self, ident: Spanned<Symbol>) {
        self.push(ident)
    }

    fn visit_expr_unary(&mut self, op: Spanned<UnaryOp>, rhs: Spanned<ExprTy>) {
        self.push(op);
        self.push(rhs);
    }

    fn visit_expr_variable(&mut self, ident: Spanned<Symbol>) {
        self.push(ident);
    }

    fn visit_stmt_expression(&mut self, expr: Spanned<ExprTy>) {
        self.push(expr);
    }

    fn visit_stmt_print(&mut self, expr: Spanned<ExprTy>) {
        self.push(expr);
    }

    fn visit_stmt_var(&mut self, binding: Spanned<Symbol>, init: Option<Spanned<ExprTy>>) {
        self.push(binding);
        if let Some(init) = init {
            self.push(init)
        }
    }

    fn visit_stmt_block(&mut self, exprs: &[Spanned<StmtTy>]) {
        for expr in exprs {
            self.push(*expr);
        }
    }

    fn visit_stmt_class(
        &mut self,
        ident: Spanned<Symbol>,
        super_: Option<Spanned<Symbol>>,
        functions: &[Spanned<StmtTy>],
    ) {
        self.push(ident);
        if let Some(super_) = super_ {
            self.push(super_);
        }
        for expr in functions {
            self.push(*expr);
        }
    }

    fn visit_stmt_return(&mut self, expr: Spanned<ExprTy>) {
        self.push(expr);
    }

    fn visit_stmt_function(
        &mut self,
        ident: Spanned<Symbol>,
        params: &[Spanned<Symbol>],
        block: &[Spanned<StmtTy>],
    ) {
        self.push(ident);
        for expr in params {
            self.push(*expr);
        }
        for expr in block {
            self.push(*expr);
        }
    }

    fn visit_stmt_conditional(
        &mut self,
        if_: Spanned<ExprTy>,
        body: Spanned<StmtTy>,
        else_: Option<Spanned<StmtTy>>,
    ) {
        self.push(if_);
        self.push(body);
        if let Some(else_) = else_ {
            self.push(else_);
        }
    }

    fn visit_stmt_for(
        &mut self,
        init: Option<Spanned<StmtTy>>,
        cond: Option<Spanned<ExprTy>>,
        loop_: Option<Spanned<ExprTy>>,
        body: Spanned<StmtTy>,
    ) {
        if let Some(init) = init {
            self.push(init);
        }
        if let Some(cond) = cond {
            self.push(cond);
        }
        if let Some(loop_) = loop_ {
            self.push(loop_);
        }
        self.push(body);
    }

    fn visit_stmt_while(&mut self, cond: Spanned<ExprTy>, stmt: Spanned<StmtTy>) {
        self.push(cond);
        self.push(stmt);
    }
}

pub struct Printer<'a> {
    source: &'a str,
}
impl<'a> Printer<'a> {
    pub const fn new(source: &'a str) -> Self {
        Self { source }
    }
}

impl<'a> Printer<'a> {
    pub fn print(mut self, accept: impl Accept<String>) -> String {
        accept.accept(&mut self)
    }
}

impl<'a> Visitor<String> for Printer<'a> {
    fn visit_expr_assignment(&mut self, ident: Spanned<Symbol>, expr: Spanned<ExprTy>) -> String {
        format!("{} = {}", ident.item.0, expr.accept(self))
    }

    fn visit_expr_binary(
        &mut self,
        lhs: Spanned<ExprTy>,
        op: crate::visit::Spanned<BinaryOp>,
        rhs: Spanned<ExprTy>,
    ) -> String {
        format!("{} {:?} {}", lhs.accept(self), op.item, rhs.accept(self))
    }

    fn visit_expr_call(&mut self, expr: Spanned<ExprTy>, args: &[Spanned<ExprTy>]) -> String {
        let args = sep_list(args.iter(), ", ", self.source);

        match args {
            Some(args) => format!("{}({})", expr.accept(self), args),
            _ => expr.accept(self),
        }
    }

    fn visit_expr_get(&mut self, obj: Spanned<ExprTy>, ident: Spanned<Symbol>) -> String {
        format!("{}.{}", obj.accept(self), &self.source[ident])
    }

    fn visit_expr_grouping(&mut self, expr: Spanned<ExprTy>) -> String {
        format!("({})", expr.accept(self))
    }

    fn visit_expr_literal(&mut self, lit: Spanned<Literal>) -> String {
        match lit.item {
            Literal::Nil => "nil".to_string(),
            Literal::This => "this".to_string(),
            Literal::Number(n) => n.to_string(),
            Literal::String(s) => s.to_string(),
            Literal::Bool(b) => b.to_string(),
        }
    }

    fn visit_expr_logical(
        &mut self,
        lhs: Spanned<ExprTy>,
        op: Spanned<LogicalOp>,
        rhs: Spanned<ExprTy>,
    ) -> String {
        format!(
            "{} {:?} {}",
            lhs.accept(self).as_str(),
            op.item,
            rhs.accept(self).as_str(),
        )
    }

    fn visit_expr_set(
        &mut self,
        obj: Spanned<ExprTy>,
        ident: Spanned<Symbol>,
        value: Spanned<ExprTy>,
    ) -> String {
        todo!()
    }

    fn visit_expr_super(&mut self, ident: Spanned<Symbol>) -> String {
        format!("super.{}", &self.source[ident])
    }

    fn visit_expr_unary(&mut self, op: Spanned<UnaryOp>, rhs: Spanned<ExprTy>) -> String {
        format!("{:?} {}", op.item, rhs.accept(self))
    }

    fn visit_expr_variable(&mut self, ident: Spanned<Symbol>) -> String {
        ident.item.0.to_string()
    }

    fn visit_stmt_expression(&mut self, expr: Spanned<ExprTy>) -> String {
        expr.accept(self)
    }

    fn visit_stmt_print(&mut self, expr: Spanned<ExprTy>) -> String {
        format!("print{}", expr.accept(self))
    }

    fn visit_stmt_var(
        &mut self,
        binding: Spanned<Symbol>,
        init: Option<Spanned<ExprTy>>,
    ) -> String {
        let mut out = format!("var {}", &self.source[binding]);
        if let Some(init) = &init {
            out.push_str(" = ");
            out.push_str(&init.accept(self));
        }
        out.push(';');
        out
    }

    fn visit_stmt_block(&mut self, exprs: &[Spanned<StmtTy>]) -> String {
        let list = sep_list(exprs.iter(), ";\n", self.source).unwrap_or_default();
        format!("{{\n{}\n}}", list)
    }

    fn visit_stmt_class(
        &mut self,
        ident: Spanned<Symbol>,
        super_: Option<Spanned<Symbol>>,
        functions: &[Spanned<StmtTy>],
    ) -> String {
        todo!()
    }

    fn visit_stmt_function(
        &mut self,
        ident: Spanned<Symbol>,
        params: &[Spanned<Symbol>],
        block: &[Spanned<StmtTy>],
    ) -> String {
        todo!()
    }

    fn visit_stmt_conditional(
        &mut self,
        if_: Spanned<ExprTy>,
        body: Spanned<StmtTy>,
        else_: Option<Spanned<StmtTy>>,
    ) -> String {
        todo!()
    }

    fn visit_stmt_for(
        &mut self,
        init: Option<Spanned<StmtTy>>,
        cond: Option<Spanned<ExprTy>>,
        loop_: Option<Spanned<ExprTy>>,
        body: Spanned<StmtTy>,
    ) -> String {
        todo!()
    }

    fn visit_stmt_while(&mut self, cond: Spanned<ExprTy>, stmt: Spanned<StmtTy>) -> String {
        todo!()
    }

    fn visit_stmt_return(&mut self, expr: Spanned<ExprTy>) -> String {
        format!("return{}", expr.accept(self))
    }
}

fn sep_list<'a, I, T>(iter: I, sep: &str, source: &str) -> Option<String>
where
    I: Iterator<Item = &'a Spanned<'a, T>>,
    T: 'a,
{
    let s = iter.fold(String::new(), |mut a, c| {
        if !a.is_empty() {
            a.push_str(sep);
        }
        a.push_str(&source[*c]);
        a
    });
    Some(s).filter(|s| !s.is_empty())
}
