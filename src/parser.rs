use crate::logger::Logger;

// macro_rules! pos {
//     () => {
//         concat!(file!(), ":", line!(), ":", column!())
//     };
// }

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

pub fn parse(source: &str) -> (Program, Vec<diag::Diagnostic>) {
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

    fn error<T>(&self, kind: ErrorKind) -> ParseResult<T> {
        let _t = Logger::enter(format!("error: {:?}", kind));
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

    fn contains(&self, tokens: &[Token]) -> bool {
        for &token in tokens {
            if self.current() == token {
                return true;
            }
        }
        false
    }

    fn once<F, T, E>(&mut self, func: F, msg: &str) -> Result<T, E>
    where
        F: Fn(&mut Self) -> Result<T, E>,
    {
        func(self).map_err(|err| {
            self.errors.push(diag!(self.current.span, "{}", msg));
            err
        })
    }

    fn surround<F, T>(&mut self, left: Token, func: F, right: Token) -> ParseResult<T>
    where
        F: Fn(&mut Self) -> ParseResult<T>,
    {
        self.expect(left)?;
        let out = func(self)?;
        self.expect(right)?;
        Ok(out)
    }

    fn surround_spanned<F, T>(
        &mut self,
        left: Token,
        func: F,
        right: Token,
    ) -> ParseResult<Spanned<T>>
    where
        F: Fn(&mut Self) -> ParseResult<T>,
    {
        self.spanned(|this| {
            this.expect(left)?;
            let out = func(this)?;
            this.expect(right)?;
            Ok(out)
        })
    }

    fn question<F, T, E>(&mut self, delimited: Token, func: F) -> Result<Option<T>, E>
    where
        F: Fn(&mut Self) -> Result<T, E>,
    {
        let ok = if self.maybe_bump(delimited) {
            func(self).map(Some)?
        } else {
            None
        };
        Ok(ok)
    }

    fn star<F, D, T>(&mut self, func: F, delimited: D) -> Vec<T>
    where
        F: Fn(&mut Self) -> Result<T, Error>,

        D: Into<Option<Token>>,
    {
        let delimited = delimited.into();
        let mut out = vec![];

        // TODO log this, but ignore the final 'unexpected eof'
        while let Ok(p) = func(self) {
            out.push(p);
            if let Some(tok) = delimited {
                if !self.maybe_bump(tok) {
                    break;
                }
            }
        }
        out
    }

    fn delimited<T, F>(&mut self, func: F, delimited: Token) -> ParseResult<Vec<T>>
    where
        F: Fn(&mut Self) -> ParseResult<T>,
    {
        let mut out = vec![func(self)?];
        if !self.maybe_bump(delimited) {
            return Ok(out);
        }
        while let Ok(p) = func(self) {
            out.push(p);
            if !self.maybe_bump(delimited) {
                break;
            }
        }
        Ok(out)
    }
}

impl<'a> Parser<'a> {
    fn program(&mut self) -> Program {
        let _t = Logger::enter("program");
        let stmts = self.star(Self::declaration, None);
        Program { stmts }
    }

    fn declaration(&mut self) -> ParseResult<Stmt> {
        // declaration    → classDecl
        //                | funDecl
        //                | varDecl
        //                | statement ;
        let _t = Logger::enter("declaration");
        match self.current() {
            Token::Class => self.class_decl(),
            Token::Fun => self.fun_decl(),
            Token::Var => self.var_decl(),
            _ => self.statement(),
        }
    }

    fn class_decl(&mut self) -> ParseResult<Stmt> {
        // classDecl      → "class" IDENTIFIER ( "<" IDENTIFIER )?
        //                  "{" function* "}" ;
        let _t = Logger::enter("class_decl");

        self.spanned(|this| {
            this.expect(Token::Class)?;
            let ident = this.identifier()?;

            let super_ = this.question(Token::Less, Self::identifier)?;
            this.expect(Token::LeftBrace)?;

            let functions = this.delimited(Self::function, Token::RightParen)?;

            let stmt = StmtTy::Class {
                ident,
                super_,
                functions,
            };
            Ok(stmt)
        })
    }

    fn fun_decl(&mut self) -> ParseResult<Stmt> {
        // funDecl        → "fun" function ;
        let _t = Logger::enter("fun_decl");
        self.expect(Token::Fun)?;
        self.function()
    }

    fn function(&mut self) -> ParseResult<Stmt> {
        // function       → IDENTIFIER "(" parameters? ")" block ;
        let _t = Logger::enter("function");

        self.spanned(|this| {
            use Token::*;
            let stmt = StmtTy::Function {
                ident: this.identifier()?,
                params: this.surround(LeftParen, Self::parameters, RightParen)?,
                block: this.block()?,
            };
            Ok(stmt)
        })
    }

    fn var_decl(&mut self) -> ParseResult<Stmt> {
        // varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
        let _t = Logger::enter("var_decl");

        self.spanned(|this| {
            this.expect(Token::Var)?;
            let binding = this.identifier()?;
            let init = this.question(Token::Equal, Self::expression)?;
            this.expect(Token::Semicolon)?;
            let stmt = StmtTy::Var { binding, init };
            Ok(stmt)
        })
    }

    fn statement(&mut self) -> ParseResult<Stmt> {
        // statement      → exprStmt
        //                | forStmt
        //                | ifStmt
        //                | printStmt
        //                | returnStmt
        //                | whileStmt
        //                | block ;
        let _t = Logger::enter("statement");

        match self.current() {
            Token::For => self.for_stmt(),
            Token::If => self.if_stmt(),
            Token::Print => self.print_stmt(),
            Token::Return => self.return_stmt(),
            Token::While => self.while_stmt(),
            Token::LeftBrace => self.spanned(|this| {
                let exprs = this.block()?;
                Ok(StmtTy::Block { exprs })
            }),
            _ => self.expr_stmt(),
        }
    }

    fn expr_stmt(&mut self) -> ParseResult<Stmt> {
        // exprStmt       → expression ";" ;
        let _t = Logger::enter("expr_stmt");
        self.spanned(|this| {
            let expr = this.expression()?;
            this.expect(Token::Semicolon)?;

            let expr = StmtTy::Expression { expr };
            Ok(expr)
        })
    }

    fn for_stmt(&mut self) -> ParseResult<Stmt> {
        // forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
        //                            expression? ";"
        //                            expression? ")" statement ;
        let _t = Logger::enter("for_stmt");

        self.spanned(|this| {
            this.expect(Token::For)?;
            this.expect(Token::LeftParen)?;

            let init = match this.current() {
                Token::Semicolon => None,
                Token::Var => Some(this.var_decl()?),
                _ => Some(this.expr_stmt()?),
            }
            .map(Box::new);

            // TODO: this isn't doing the right thing
            let cond = this.question(Token::Semicolon, Self::expression)?;
            let loop_ = match this.question(Token::RightParen, Self::expression)? {
                Some(expr) => Some(expr),
                None => {
                    this.expect(Token::RightParen)?;
                    None
                }
            };

            let body = this.statement().map(Box::new)?;

            Ok(StmtTy::For {
                init,
                cond,
                loop_,
                body,
            })
        })
    }

    fn if_stmt(&mut self) -> ParseResult<Stmt> {
        // ifStmt         → "if" "(" expression ")" statement ( "else" statement )? ;
        let _t = Logger::enter("if_stmt");

        self.spanned(|this| {
            this.expect(Token::If)?;
            let if_ = this.surround(Token::LeftParen, Self::expression, Token::RightParen)?;
            let body = this.statement().map(Box::new)?;
            let else_ = this.question(Token::Else, Self::statement)?.map(Box::new);
            Ok(StmtTy::Conditional { if_, body, else_ })
        })
    }

    fn print_stmt(&mut self) -> ParseResult<Stmt> {
        // printStmt      → "print" expression ";" ;
        let _t = Logger::enter("print_stmt");

        self.spanned(|this| {
            this.expect(Token::Print)?;
            let expr = this.expression()?;
            this.expect(Token::Semicolon)?;
            Ok(StmtTy::Print { expr })
        })
    }

    fn return_stmt(&mut self) -> ParseResult<Stmt> {
        // returnStmt     → "return" expression? ";" ;
        let _t = Logger::enter("return_stmt");

        self.spanned(|this| {
            this.expect(Token::Return)?;
            let expr = this.expression()?;
            this.expect(Token::Semicolon)?;
            Ok(StmtTy::Return { expr })
        })
    }

    fn while_stmt(&mut self) -> ParseResult<Stmt> {
        // whileStmt      → "while" "(" expression ")" statement ;
        let _t = Logger::enter("while_stmt");

        self.spanned(|this| {
            this.expect(Token::While)?;
            let cond = this.surround(
                Token::LeftParen, //
                Self::expression,
                Token::RightParen,
            )?;
            let stmt = this.statement().map(Box::new)?;
            Ok(StmtTy::While { cond, stmt })
        })
    }

    fn block(&mut self) -> ParseResult<Vec<Stmt>> {
        // block          → "{" declaration* "}" ;
        let _t = Logger::enter("block");
        self.expect(Token::LeftBrace)?;
        self.delimited(Self::declaration, Token::RightBrace)
    }

    fn expression(&mut self) -> ParseResult<Expr> {
        // expression     → assignment ;
        let _t = Logger::enter("expression");
        self.assignment()
    }

    fn assignment(&mut self) -> ParseResult<Expr> {
        // assignment     → ( call "." )? IDENTIFIER "=" assignment
        //                | logic_or ;
        let _t = Logger::enter("assignment");

        let span = self.current.span;
        let expr = self.logic_or()?;

        if self.maybe_bump(Token::Equal) {
            let value = self.assignment()?;
            let ty = match expr.item {
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
            return Ok(Expr::new(ty, span + self.current.span));
        }

        Ok(expr)
    }

    fn logic_or(&mut self) -> ParseResult<Expr> {
        // logic_or       → logic_and ( "or" logic_and )* ;
        let _t = Logger::enter("logic_or");
        self.logical(Token::Or, LogicalOp::Or)
    }

    fn logic_and(&mut self) -> ParseResult<Expr> {
        // logic_and      → equality ( "and" equality )* ;
        let _t = Logger::enter("logic_and");
        self.logical(Token::And, LogicalOp::And)
    }

    fn logical(&mut self, token: Token, op: LogicalOp) -> ParseResult<Expr> {
        let mut lhs = self.equality()?;
        while self.maybe_bump(token) {
            let expr = ExprTy::Logical {
                lhs: Box::new(lhs),
                op: Spanned::new(op.clone(), self.current.span),
                rhs: Box::new(self.equality()?),
            };
            lhs = Expr::new(expr, self.current.span)
        }
        Ok(lhs)
    }

    fn equality(&mut self) -> ParseResult<Expr> {
        // equality       → comparison ( ( "!=" | "==" ) comparison )* ;
        let _t = Logger::enter("equality");
        self.binary_op(Self::comparison, &[Token::BangEqual, Token::EqualEqual])
    }

    fn comparison(&mut self) -> ParseResult<Expr> {
        // comparison     → addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
        let _t = Logger::enter("comparison");
        self.binary_op(
            Self::addition,
            &[
                Token::Greater,
                Token::GreaterEqual,
                Token::Less,
                Token::LessEqual,
            ],
        )
    }

    fn addition(&mut self) -> ParseResult<Expr> {
        // addition       → multiplication ( ( "-" | "+" ) multiplication )* ;
        let _t = Logger::enter("addition");
        self.binary_op(Self::multiplication, &[Token::Minus, Token::Plus])
    }

    fn multiplication(&mut self) -> ParseResult<Expr> {
        // multiplication → unary ( ( "/" | "*" ) unary )* ;
        let _t = Logger::enter("multiplication");
        self.binary_op(Self::unary, &[Token::Slash, Token::Star])
    }

    fn binary_op<F>(&mut self, next: F, tokens: &[Token]) -> ParseResult<Expr>
    where
        F: Fn(&mut Self) -> ParseResult<Expr>,
    {
        let mut lhs = next(self)?;
        while self.contains(tokens) {
            let span = self.current.span;
            let expr = ExprTy::Binary {
                lhs: Box::new(lhs),
                op: Spanned::new(BinaryOp::from_token(self.bump()), span),
                rhs: Box::new(next(self)?),
            };
            lhs = Expr::new(expr, span)
        }

        Ok(lhs)
    }

    fn unary(&mut self) -> ParseResult<Expr> {
        // unary          → ( "!" | "-" ) unary | call ;
        let _t = Logger::enter("unary");

        if self.contains(&[Token::Bang, Token::Minus]) {
            let span = self.current.span;
            let op = match self.bump() {
                Token::Bang => UnaryOp::Not,
                Token::Minus => UnaryOp::Minus,
                _ => unreachable!(),
            };

            let expr = ExprTy::Unary {
                op: Spanned::new(op, span),
                rhs: self.unary().map(Box::new)?,
            };
            return Ok(Expr::new(expr, span));
        }

        self.call()
    }

    fn call(&mut self) -> ParseResult<Expr> {
        // call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
        let _t = Logger::enter("call");

        let mut expr = self.primary()?;

        while self.maybe_bump(Token::Dot) {
            let obj = Box::new(expr);
            expr = self.spanned(move |this| {
                let ident = this.identifier()?;
                Ok(ExprTy::Get { obj, ident })
            })?;
        }

        if self.maybe_bump(Token::LeftParen) {
            let obj = Box::new(expr);
            expr = self.spanned(move |this| {
                let mut args = vec![];
                while !this.contains(&[Token::RightParen]) {
                    args.push(this.expression()?);
                    if !this.maybe_bump(Token::Comma) {
                        break;
                    }
                }
                this.expect(Token::RightParen)?;
                Ok(ExprTy::Call { expr: obj, args })
            })?;
        }

        Ok(expr)
    }

    fn primary(&mut self) -> ParseResult<Expr> {
        // primary        → "true" | "false" | "nil" | "this"
        //                | NUMBER | STRING | IDENTIFIER | "(" expression ")"
        //                | "super" "." IDENTIFIER ;
        use Token::*;

        let _t = Logger::enter("primary");

        match self.current() {
            True | False | Nil | This | Number | String => self.literal(),
            Identifier => self.variable(),
            LeftParen => self.grouping(),
            Super => self.super_(),
            e => self.error(ErrorKind::UnexpectedToken(e)),
        }
    }

    fn parameters(&mut self) -> ParseResult<Vec<Identifier>> {
        let _t = Logger::enter("parameters");
        self.delimited(Self::identifier, Token::Comma)
    }

    fn arguments(&mut self) -> ParseResult<Vec<Expr>> {
        let _t = Logger::enter("arguments");
        self.delimited(Self::expression, Token::Comma)
    }

    fn super_(&mut self) -> ParseResult<Expr> {
        let _t = Logger::enter("super_");
        self.spanned(|this| {
            this.expect(Token::Super)?;
            this.expect(Token::Dot)?;
            let ident = this.identifier()?;
            Ok(ExprTy::Super { ident })
        })
    }

    fn variable(&mut self) -> ParseResult<Expr> {
        let _t = Logger::enter("variable");

        let t = self.identifier()?;
        Ok(t.map(|ident| ExprTy::Variable { ident }))
    }

    fn identifier(&mut self) -> ParseResult<Identifier> {
        let _t = Logger::enter("identifier");
        if self.current() != Token::Identifier {
            return self.error(ErrorKind::ExpectedIdentifier);
        }

        let span = self.current.span;
        self.bump();
        Ok(Spanned::new(Symbol::new(&self.source[span]), span))
    }

    fn grouping(&mut self) -> ParseResult<Expr> {
        let _t = Logger::enter("grouping");

        self.spanned(|this| {
            let expr = this
                .surround(
                    Token::LeftParen, //
                    Self::expression,
                    Token::RightParen,
                )
                .map(Box::new)?;
            Ok(ExprTy::Grouping { expr })
        })
    }

    fn literal(&mut self) -> ParseResult<Expr> {
        let _t = Logger::enter("literal");

        self.spanned(|this| {
            let lit = match this.current() {
                Token::True => Literal::Bool(true),
                Token::False => Literal::Bool(false),
                Token::Nil => Literal::Nil,
                Token::Number => this.number()?,
                Token::String => this.string()?,
                Token::This => Literal::This,
                _ => unreachable!(),
            };
            this.bump();
            Ok(ExprTy::Literal { lit })
        })
    }

    fn string(&mut self) -> ParseResult<Literal> {
        let _t = Logger::enter("string");
        // TODO be smarter about this
        let s = self.source();
        if s.starts_with('"') && s.ends_with('"') {
            return Ok(Literal::String(s[1..s.len() - 1].into()));
        }
        self.error(ErrorKind::UnterminatedString)
    }

    fn number(&mut self) -> ParseResult<Literal> {
        let _t = Logger::enter("number");
        match self.source().parse().map(Literal::Number) {
            Ok(ast) => Ok(ast),
            Err(err) => self.error(err.into()),
        }
    }

    fn is_primary(&mut self) -> bool {
        use Token::*;

        matches!(
            self.current(),
            True | False | Nil | This |    /* .. */
            Number | String | Identifier | /* .. */
            LeftParen | Super
        )
    }

    fn is_identifier(&mut self) -> bool {
        matches!(self.current(), Token::Identifier)
    }

    // fn print_raw<T>(&self, pos: &str, spanned: &Spanned<T>) {
    //     eprintln!(
    //         "?? ({}) {:?} | '{}'",
    //         pos,
    //         spanned.span,
    //         self.source[spanned.span].escape_debug()
    //     )
    // }
}

#[test]
fn asdf() {
    let source = r"
var f = 21;
var a = fib(f);
print(a);
    ";

    let (program, errors) = parse(source);

    if !errors.is_empty() {
        for error in errors {
            eprintln!("{}", error.render(source));
        }
    }

    macro_rules! p {
        ($span:expr) => {
            eprintln!("> '{}'", source[$span].escape_debug())
        };
    }

    if let Spanned {
        item: StmtTy::Var { init, .. },
        span,
    } = &program.stmts[0]
    {
        p!(init.as_ref().unwrap().clone());
        p!(span);
    }

    eprintln!("{}", crate::printer::Printer::new(source).print(program));
}

#[test]
fn primary() {
    let sources = &[
        "true",
        "false",
        "nil",
        "this",
        "1234",
        "\"hello world\"",
        "foobar",
        "super.foobar",
        "(a)",
        "(a.b.c)",
        "(a.c(d))",
    ];

    for source in sources {
        let mut parser = Parser::new(source);
        let primary = parser.primary().unwrap();

        if !parser.errors.is_empty() {
            for error in parser.errors {
                eprintln!("{}", error.render(source));
            }
        }

        // eprintln!("{:#?}", primary);
        eprintln!(
            "{}",
            crate::printer::Printer::new(source).print(Spanned::as_ref(&primary))
        );
    }
}
