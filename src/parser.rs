use crate::logger::Logger;

macro_rules! pos {
    () => {
        concat!(file!(), ":", line!(), ":", column!())
    };
}

use crate::{
    ast::*,
    diag,
    span::{Span, Spanned},
    token::Token,
    Lexer,
};

use std::iter::Peekable;

#[derive(Debug)]
struct Error {
    span: Span,
    token: Token,
    kind: ErrorKind,
}

impl Error {
    fn into_diag(self) -> diag::Diagnostic {
        use ErrorKind::*;
        let msg = match self.kind {
            ExpectedIdentifier => "expected identifier".to_string(),
            ExpectedPrimary => "expected primary".to_string(),
            ExpectedMultiplication => "expected multiplication".to_string(),
            ExpectedToken(tok) => format!("expected token: {:?}", tok),
            UnexpectedToken(tok) => format!("unexpected token: {:?}", tok),
            UnterminatedString => "unterminated string".to_string(),
            InvalidAssignment => "invalid assignment".to_string(),
            InvalidFloat(err) => format!("invalid float: {}", err),
            Eof => "reached end of find".to_string(),
        };

        diag::Diagnostic::error(msg, self.span)
    }
}

#[derive(Debug)]
enum ErrorKind {
    ExpectedIdentifier,
    ExpectedPrimary,
    ExpectedMultiplication,
    ExpectedToken(Token),
    UnexpectedToken(Token),
    UnterminatedString,
    InvalidAssignment,
    InvalidFloat(std::num::ParseFloatError),
    Eof,
}

impl From<std::num::ParseFloatError> for ErrorKind {
    fn from(err: std::num::ParseFloatError) -> Self {
        Self::InvalidFloat(err)
    }
}

type ParseResult<T> = std::result::Result<T, Error>;

pub fn parse(source: &str) -> (Spanned<Program>, Vec<diag::Diagnostic>) {
    let mut parser = Parser::new(source);
    (parser.program(), parser.errors)
}

struct Parser<'a> {
    source: &'a str,
    tokens: Peekable<Lexer<'a>>,
    current: Spanned<Token>,
    prev: Span,
    errors: Vec<diag::Diagnostic>,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        let mut parser = Self {
            source,
            tokens: Lexer::new(source).peekable(),
            current: Spanned {
                item: Token::Eof,
                span: Span::default(),
            },
            prev: Span::default(),
            errors: vec![],
        };

        parser.bump();
        parser
    }

    fn bump(&mut self) -> Token {
        match self.tokens.next() {
            Some(token) => {
                self.prev = self.current.span;
                std::mem::replace(&mut self.current, token).item
            }
            None => std::mem::replace(&mut self.current.item, Token::Eof),
        }
    }

    fn maybe_bump(&mut self, token: Token) -> bool {
        if self.current.item == token {
            self.bump();
            return true;
        }
        false
    }

    const fn current(&self) -> Token {
        self.current.item
    }

    fn source(&self) -> &str {
        &self.source[self.current]
    }

    const fn error<T>(&self, kind: ErrorKind) -> ParseResult<T> {
        Err(Error {
            span: self.current.span,
            token: self.current.item,
            kind,
        })
    }

    fn expect(&mut self, kind: Token) -> ParseResult<()> {
        if self.current() == kind {
            self.bump();
            return Ok(());
        }
        self.errors.push(diag!(
            self.current.span,
            "expected token {:?}, but found {:?}",
            kind,
            self.current()
        ));
        self.error(ErrorKind::ExpectedToken(kind))
    }

    fn spanned<F, T>(&mut self, func: F) -> ParseResult<Spanned<T>>
    where
        F: FnOnce(&mut Self) -> ParseResult<T>,
    {
        let span = self.current.span;
        func(self).map(|item| Spanned::new(item, span + self.current.span))
    }

    fn manual_span<T>(&self, item: T, start: Span) -> Spanned<T> {
        Spanned::new(item, start + self.current.span)
    }

    fn contains(&self, tokens: &[Token]) -> bool {
        for &token in tokens {
            if self.current() == token {
                return true;
            }
        }
        false
    }
}

impl<'a> Parser<'a> {
    // program        → declaration* EOF ;
    fn program(&mut self) -> Spanned<Program> {
        let _t = Logger::enter(format!("program: {:?}", self.current.span));
        let mut stmts = vec![];
        self.spanned(|this| {
            while !this.maybe_bump(Token::Eof) {
                match this.spanned(Self::declaration) {
                    Ok(decl) => stmts.push(decl),
                    Err(err) => this.errors.push(err.into_diag()),
                };
            }
            Ok(Program { stmts })
        })
        .unwrap()
    }

    // declaration    → classDecl
    //                | funDecl
    //                | varDecl
    //                | statement ;
    fn declaration(&mut self) -> ParseResult<StmtTy> {
        let _t = Logger::enter(format!("declaration: {:?}", self.current.span));
        match self.current() {
            Token::Class => self.class_decl(),
            Token::Fun => self.fun_decl(),
            Token::Var => self.var_decl(),
            _ => self.statement(),
        }
    }

    // classDecl      → "class" IDENTIFIER ( "<" IDENTIFIER )?
    //                  "{" function* "}" ;
    fn class_decl(&mut self) -> ParseResult<StmtTy> {
        let _t = Logger::enter(format!("class_decl: {:?}", self.current.span));
        self.expect(Token::Class)?;
        let ident = self.identifier()?;

        let super_ = if self.maybe_bump(Token::Less) {
            Some(self.identifier()?)
        } else {
            None
        };

        self.expect(Token::LeftBrace)?;
        let mut functions = vec![];
        while !self.maybe_bump(Token::RightBrace) {
            functions.push(self.spanned(Self::function)?)
        }

        Ok(StmtTy::Class {
            ident,
            super_,
            functions,
        })
    }

    // funDecl        → "fun" function ;
    fn fun_decl(&mut self) -> ParseResult<StmtTy> {
        let _t = Logger::enter(format!("fun_decl: {:?}", self.current.span));
        self.expect(Token::Fun)?;
        self.function()
    }

    // function       → IDENTIFIER "(" parameters? ")" block ;
    fn function(&mut self) -> ParseResult<StmtTy> {
        let _t = Logger::enter(format!("function: {:?}", self.current.span));
        let ident = self.identifier()?;

        self.expect(Token::LeftParen)?;
        let mut params = vec![self.identifier()?];
        while !self.maybe_bump(Token::Comma) {
            params.push(self.identifier()?)
        }
        self.expect(Token::RightParen)?;

        let block = self.spanned(Self::block)?;

        Ok(StmtTy::Function {
            ident,
            params,
            block,
        })
    }

    // varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
    fn var_decl(&mut self) -> ParseResult<StmtTy> {
        let _t = Logger::enter(format!("var_decl: {:?}", self.current.span));
        self.expect(Token::Var)?;
        let binding = self.identifier()?;

        let init = if self.maybe_bump(Token::Equal) {
            Some(self.spanned(Self::expression)?)
        } else {
            None
        };

        self.expect(Token::Semicolon)?;
        Ok(StmtTy::Var { binding, init })
    }

    // statement      → exprStmt
    //                | forStmt
    //                | ifStmt
    //                | printStmt
    //                | returnStmt
    //                | whileStmt
    //                | block ;
    fn statement(&mut self) -> ParseResult<StmtTy> {
        let _t = Logger::enter(format!("statement: {:?}", self.current.span));
        match self.current() {
            Token::For => self.for_stmt(),
            Token::If => self.if_stmt(),
            Token::Print => self.print_stmt(),
            Token::Return => self.return_stmt(),
            Token::While => self.while_stmt(),
            Token::LeftBrace => {
                let exprs = self.block()?;
                for expr in &exprs {
                    eprintln!("{} {:?}", pos!(), expr.span);
                }
                Ok(StmtTy::Block { exprs })
            }
            _ => self.expr_stmt(),
        }
    }

    // exprStmt       → expression ";" ;
    fn expr_stmt(&mut self) -> ParseResult<StmtTy> {
        let _t = Logger::enter(format!("expr_stmt: {:?}", self.current.span));

        let start = self.current.span;
        let expr = self.expression()?;
        let expr = self.manual_span(expr, start);
        eprintln!("{} {:#?}", pos!(), self.current);
        self.expect(Token::Semicolon)?;
        Ok(StmtTy::Expression { expr })
    }

    // forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
    //                            expression? ";"
    //                            expression? ")" statement ;
    fn for_stmt(&mut self) -> ParseResult<StmtTy> {
        let _t = Logger::enter(format!("for_stmt: {:?}", self.current.span));
        self.expect(Token::For)?;
        self.expect(Token::LeftParen)?;

        let init = match self.current() {
            Token::Semicolon => None,
            Token::Var => Some(self.spanned(Self::var_decl)?),
            _ => Some(self.spanned(Self::expr_stmt)?),
        }
        .map(Box::new);

        let cond = match self.current() {
            Token::Semicolon => None,
            _ => Some(self.spanned(Self::expression)?),
        };
        self.expect(Token::Semicolon)?;

        let loop_ = match self.current() {
            Token::RightParen => None,
            _ => Some(self.spanned(Self::expression)?),
        };
        self.expect(Token::RightParen)?;

        let body = self.spanned(Self::statement).map(Box::new)?;

        Ok(StmtTy::For {
            init,
            cond,
            loop_,
            body,
        })
    }

    // ifStmt         → "if" "(" expression ")" statement ( "else" statement )? ;
    fn if_stmt(&mut self) -> ParseResult<StmtTy> {
        let _t = Logger::enter(format!("if_stmt: {:?}", self.current.span));
        self.expect(Token::If)?;
        let if_ = {
            self.expect(Token::LeftParen)?;
            let expr = self.spanned(Self::expression)?;
            self.expect(Token::RightParen)?;
            expr
        };

        let body = self.spanned(Self::statement).map(Box::new)?;
        let else_ = if self.maybe_bump(Token::Else) {
            Some(Box::new(self.spanned(Self::statement)?))
        } else {
            None
        };
        Ok(StmtTy::Conditional { if_, body, else_ })
    }

    // printStmt      → "print" expression ";" ;
    fn print_stmt(&mut self) -> ParseResult<StmtTy> {
        let _t = Logger::enter(format!("print_stmt: {:?}", self.current.span));
        self.keyword_stmt(Token::Print)
            .map(|expr| StmtTy::Print { expr })
    }

    // returnStmt     → "return" expression? ";" ;
    fn return_stmt(&mut self) -> ParseResult<StmtTy> {
        let _t = Logger::enter(format!("return_stmt: {:?}", self.current.span));
        self.keyword_stmt(Token::Return)
            .map(|expr| StmtTy::Return { expr })
    }

    fn keyword_stmt(&mut self, keyword: Token) -> ParseResult<Expr> {
        self.expect(keyword)?;
        let expr = self.spanned(Self::expression)?;
        self.expect(Token::Semicolon)?;
        Ok(expr)
    }

    // whileStmt      → "while" "(" expression ")" statement ;
    fn while_stmt(&mut self) -> ParseResult<StmtTy> {
        let _t = Logger::enter(format!("while_stmt: {:?}", self.current.span));
        self.expect(Token::While)?;
        let cond = {
            self.expect(Token::LeftParen)?;
            let expr = self.spanned(Self::expression)?;
            self.expect(Token::RightParen)?;
            expr
        };
        let stmt = self.spanned(Self::statement).map(Box::new)?;
        Ok(StmtTy::While { cond, stmt })
    }

    // block          → "{" declaration* "}" ;
    fn block(&mut self) -> ParseResult<Vec<Stmt>> {
        let _t = Logger::enter(format!("block: {:?}", self.current.span));
        let mut stmts = vec![];
        self.expect(Token::LeftBrace)?;
        while !self.maybe_bump(Token::RightBrace) {
            let start = self.current.span;
            let decl = self.declaration()?;
            stmts.push(self.manual_span(decl, start))
        }
        Ok(stmts)
    }

    // expression     → assignment ;
    fn expression(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("expression: {:?}", self.current.span));
        self.assignment()
    }
    // assignment     → ( call "." )? IDENTIFIER "=" assignment
    //                | logic_or ;
    fn assignment(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("assignment: {:?}", self.current.span));
        let expr = self.logic_or()?;

        if self.maybe_bump(Token::Equal) {
            let value = self.spanned(Self::assignment)?;
            let ty = match expr {
                ExprTy::Get { obj, ident } => ExprTy::Set {
                    ident,
                    obj,
                    value: Box::new(value),
                },

                ExprTy::Variable { ident } => ExprTy::Assignment {
                    ident,
                    expr: Box::new(value),
                },

                _ => return self.error(ErrorKind::InvalidAssignment),
            };

            return Ok(ty);
        }

        Ok(expr)
    }

    // logic_or       → logic_and ( "or" logic_and )* ;
    fn logic_or(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("logic_or: {:?}", self.current.span));
        self.logical(Token::Or, LogicalOp::Or)
    }

    // logic_and      → equality ( "and" equality )* ;
    fn logic_and(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("logic_and: {:?}", self.current.span));
        self.logical(Token::And, LogicalOp::And)
    }

    fn logical(&mut self, token: Token, op: LogicalOp) -> ParseResult<ExprTy> {
        let mut lhs = self.equality()?;
        let start = self.current.span;
        while self.maybe_bump(token) {
            // TODO these spans should be automatically created
            lhs = ExprTy::Logical {
                lhs: Box::new(self.manual_span(lhs, start)),
                op: self.manual_span(op.clone(), start),
                rhs: Box::new(self.spanned(Self::equality)?),
            }
        }
        Ok(lhs)
    }

    // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
    fn equality(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("equality: {:?}", self.current.span));
        self.binary_op(Self::comparison, &[Token::BangEqual, Token::EqualEqual])
    }

    // comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
    fn comparison(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("comparison: {:?}", self.current.span));
        use Token::*;
        self.binary_op(Self::addition, &[Greater, GreaterEqual, Less, LessEqual])
    }

    // addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
    fn addition(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("addition: {:?}", self.current.span));
        self.binary_op(Self::multiplication, &[Token::Minus, Token::Plus])
    }

    // multiplication → unary ( ( "/" | "*" ) unary )* ;
    fn multiplication(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("multiplication: {:?}", self.current.span));
        self.binary_op(Self::unary, &[Token::Slash, Token::Star])
    }

    fn binary_op<F>(&mut self, mut next: F, tokens: &[Token]) -> ParseResult<ExprTy>
    where
        F: Fn(&mut Self) -> ParseResult<ExprTy>,
    {
        let mut lhs = next(self)?;
        while self.contains(tokens) {
            let span = self.current.span;
            // TODO these spans should be created automatically

            let op = BinaryOp::from_token(self.bump());
            lhs = ExprTy::Binary {
                lhs: Box::new(self.manual_span(lhs, span)),
                op: self.manual_span(op, span),
                rhs: Box::new(self.spanned(&mut next)?),
            }
        }
        Ok(lhs)
    }

    // unary          → ( "!" | "-" ) unary | call ;
    fn unary(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("unary: {:?}", self.current.span));
        if self.contains(&[Token::Bang, Token::Minus]) {
            let span = self.current.span;
            let op = match self.bump() {
                Token::Bang => UnaryOp::Not,
                Token::Minus => UnaryOp::Minus,
                _ => unreachable!(),
            };
            let expr = ExprTy::Unary {
                op: self.manual_span(op, span),
                rhs: self.spanned(Self::unary).map(Box::new)?,
            };
            return Ok(expr);
        }

        self.call()
    }

    // call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
    fn call(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("call: {:?}", self.current.span));
        let start = self.current.span;
        let mut expr = self.primary()?;

        while self.maybe_bump(Token::Dot) {
            let obj = Box::new(self.manual_span(expr, start));
            let ident = self.identifier()?;
            expr = ExprTy::Get { obj, ident };
        }

        if self.maybe_bump(Token::LeftParen) {
            let obj = Box::new(self.manual_span(expr, start));
            let mut args = vec![];
            while !self.contains(&[Token::RightParen]) {
                args.push(self.spanned(Self::expression)?);
                if !self.maybe_bump(Token::Comma) {
                    break;
                }
            }
            self.expect(Token::RightParen)?;
            expr = ExprTy::Call { expr: obj, args };
        }

        Ok(expr)
    }
    // primary        → "true" | "false" | "nil" | "this"
    //                | NUMBER | STRING | IDENTIFIER | "(" expression ")"
    //                | "super" "." IDENTIFIER ;
    fn primary(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("primary: {:?}", self.current.span));
        use Token::*;
        match self.current() {
            True | False | Nil | This | Number | String => self.literal(),
            Identifier => self.variable(),
            LeftParen => self.grouping(),
            Super => self.super_(),
            e => self.error(ErrorKind::UnexpectedToken(e)),
        }
    }

    fn super_(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("super_: {:?}", self.current.span));
        self.expect(Token::Super)?;
        self.expect(Token::Dot)?;
        let ident = self.identifier()?;
        Ok(ExprTy::Super { ident })
    }

    fn variable(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("variable: {:?}", self.current.span));
        self.identifier().map(|ident| ExprTy::Variable { ident })
    }

    fn identifier(&mut self) -> ParseResult<Identifier> {
        let _t = Logger::enter(format!("identifier: {:?}", self.current.span));
        if self.current() != Token::Identifier {
            return self.error(ErrorKind::ExpectedIdentifier);
        }

        let span = self.current.span;
        self.bump();
        Ok(Spanned::new(Symbol::new(&self.source[span]), span))
    }

    fn grouping(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("grouping: {:?}", self.current.span));
        self.expect(Token::LeftParen)?;
        let expr = self.spanned(Self::expression).map(Box::new)?;
        self.expect(Token::RightParen)?;
        Ok(ExprTy::Grouping { expr })
    }

    fn literal(&mut self) -> ParseResult<ExprTy> {
        let _t = Logger::enter(format!("literal: {:?}", self.current.span));
        // this looks wrong
        let lit = match self.current() {
            Token::True => Literal::Bool(true),
            Token::False => Literal::Bool(false),
            Token::Nil => Literal::Nil,
            Token::Number => self.number()?,
            Token::String => self.string()?,
            Token::This => Literal::This,
            _ => unreachable!(),
        };
        self.bump();
        Ok(ExprTy::Literal { lit })
    }

    fn string(&mut self) -> ParseResult<Literal> {
        let _t = Logger::enter(format!("string: {:?}", self.current.span));
        // TODO be smarter about this
        let s = self.source();
        if s.starts_with('"') && s.ends_with('"') {
            return Ok(Literal::String(s[1..s.len() - 1].into()));
        }
        self.error(ErrorKind::UnterminatedString)
    }

    fn number(&mut self) -> ParseResult<Literal> {
        let _t = Logger::enter(format!("number: {:?}", self.current.span));
        match self.source().parse().map(Literal::Number) {
            Ok(ast) => Ok(ast),
            Err(err) => self.error(err.into()),
        }
    }

    const fn is_primary(&self) -> bool {
        use Token::*;
        matches!(
            self.current(),
            True | False | Nil | This |    /* .. */
            Number | String | Identifier | /* .. */
            LeftParen | Super
        )
    }

    const fn is_identifier(&self) -> bool {
        matches!(self.current(), Token::Identifier)
    }
}
