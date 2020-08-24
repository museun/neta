use std::{iter::Peekable, str::Chars};

use crate::span::{Pos, Span, Spanned};
use crate::token::Token;

pub fn lex(source: &str) -> impl Iterator<Item = Spanned<Token>> + '_ {
    let mut lexer = Lexer::new(source);
    std::iter::from_fn(move || match lexer.lex() {
        d if matches!(d.item, Token::Eof) => None,
        d => Some(d),
    })
}

struct Lexer<'a> {
    source: &'a str,
    iter: Peekable<Chars<'a>>,
    current: Pos,
    pos: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Lexer {
            source,
            iter: source.chars().peekable(),
            current: Pos { col: 0, line: 1 },
            pos: 0,
        }
    }

    fn lex(&mut self) -> Spanned<Token> {
        use Token::*;
        self.consume_while(char::is_whitespace);

        let start = self.current;
        let next = match self.consume() {
            Some(ch) => ch,
            None => return self.emit(self.current, Eof),
        };

        match (next, self.peek()) {
            ('/', Some('/')) => {
                self.consume_while(|c| c != '\n');
                self.lex()
            }

            ('(', ..) => self.emit(start, LeftParen),
            (')', ..) => self.emit(start, RightParen),
            ('{', ..) => self.emit(start, LeftBrace),
            ('}', ..) => self.emit(start, RightBrace),
            (',', ..) => self.emit(start, Comma),
            ('.', ..) => self.emit(start, Dot),
            ('-', ..) => self.emit(start, Minus),
            ('+', ..) => self.emit(start, Plus),
            (';', ..) => self.emit(start, Semicolon),
            ('/', ..) => self.emit(start, Slash),
            ('*', ..) => self.emit(start, Star),

            ('!', Some('=')) => self.emit_or(start, Bang, BangEqual, |c| c == '='),
            ('=', Some('=')) => self.emit_or(start, Equal, EqualEqual, |c| c == '='),
            ('>', Some('=')) => self.emit_or(start, Greater, GreaterEqual, |c| c == '='),
            ('<', Some('=')) => self.emit_or(start, Less, LessEqual, |c| c == '='),

            (ch, ..) if Self::is_alpha(ch) => self.lex_keyword(start),
            (ch, ..) if Self::is_digit(ch) => self.lex_number(start),
            ('"', ..) => self.lex_string(start),

            (ch, ..) => self.emit(start, Invalid(ch)),
        }
    }

    fn lex_keyword(&mut self, start: Pos) -> Spanned<Token> {
        use Token::*;
        const KEYWORDS: &[(&str, Token)] = &[
            ("and", And),
            ("class", Class),
            ("else", Else),
            ("false", False),
            ("fun", Fun),
            ("for", For),
            ("if", If),
            ("nil", Nil),
            ("or", Or),
            ("print", Print),
            ("return", Return),
            ("super", Super),
            ("this", This),
            ("true", True),
            ("var", Var),
            ("while", While),
        ];

        self.consume_while(Self::is_alpha);

        let word = &self.source[start.col as _..self.current.col as _];
        match KEYWORDS.iter().find(|&&(k, _)| k == word).map(|&(_, v)| v) {
            Some(val) => self.emit(start, val),
            None => self.emit(start, Identifier),
        }
    }

    fn lex_number(&mut self, start: Pos) -> Spanned<Token> {
        while let Some(p) = self.peek() {
            if !Self::is_digit(p) && p != '.' {
                break;
            }
            self.consume();
        }
        self.emit(start, Token::Number)
    }

    fn lex_string(&mut self, start: Pos) -> Spanned<Token> {
        self.consume_while(|ch| ch != '"');
        self.consume();
        self.emit(start, Token::String)
    }

    fn emit_or(
        &mut self,
        start: Pos,
        left: Token,
        right: Token,
        cmp: impl Fn(char) -> bool,
    ) -> Spanned<Token> {
        if cmp(self.peek().expect("not EOF")) {
            self.consume();
            self.emit(start, right)
        } else {
            self.emit(start, left)
        }
    }

    fn emit(&mut self, start: Pos, item: Token) -> Spanned<Token> {
        Spanned {
            item,
            span: Span {
                start,
                end: self.current,
            },
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.iter.peek().copied()
    }

    fn consume(&mut self) -> Option<char> {
        let ch = self.iter.next()?;
        match ch {
            '\n' => {
                self.current.line += 1;
                self.current.col = 0;
            }
            _ => self.current.col += 1,
        };
        self.pos += 1;
        Some(ch)
    }

    fn consume_while<F>(&mut self, func: F)
    where
        F: Fn(char) -> bool,
    {
        while let Some(ch) = self.peek() {
            if !func(ch) {
                break;
            }
            if self.consume().is_none() {
                break;
            }
        }
    }

    fn is_alpha(ch: char) -> bool {
        matches!(ch, 'a'..='z' | 'A'..='Z' | '_')
    }

    fn is_digit(ch: char) -> bool {
        matches!(ch, '0'..='9')
    }
}
