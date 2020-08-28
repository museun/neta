use std::{iter::Peekable, str::Chars};

use crate::span::{Pos, Span, Spanned};
use crate::token::Token;

pub struct Lexer<'a> {
    source: &'a str,

    iter: Peekable<Chars<'a>>,

    previous: Pos,
    current: Pos,

    pos: usize,
    prev: usize,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lex() {
            d if matches!(d.item, Token::Eof) => None,
            d => Some(d),
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Lexer {
            source,
            iter: source.chars().peekable(),
            previous: Pos::new(1, 0),
            current: Pos::new(1, 0),
            pos: 0,
            prev: 0,
        }
    }

    fn lex(&mut self) -> Spanned<Token> {
        use Token::*;
        self.consume_while(char::is_whitespace);
        self.previous = self.current;
        self.prev = self.pos;

        let next = match self.consume() {
            Some(ch) => ch,
            None => return self.emit(Eof),
        };

        match next {
            '(' => self.emit(LeftParen),
            ')' => self.emit(RightParen),
            '{' => self.emit(LeftBrace),
            '}' => self.emit(RightBrace),
            ',' => self.emit(Comma),
            '.' => self.emit(Dot),
            '-' => self.emit(Minus),
            '+' => self.emit(Plus),
            ';' => self.emit(Semicolon),
            '*' => self.emit(Star),
            '/' if self.peek() == Some('/') => {
                self.consume_while(|c| c != '\n');
                self.lex()
            }
            '/' => self.emit(Slash),

            '!' => self.emit_or('=', Bang, BangEqual),
            '=' => self.emit_or('=', Equal, EqualEqual),
            '>' => self.emit_or('=', Greater, GreaterEqual),
            '<' => self.emit_or('=', Less, LessEqual),

            ch if Self::is_alpha(ch) => self.lex_keyword(),
            ch if Self::is_digit(ch) => self.lex_number(),
            '"' => self.lex_string(),

            ch => self.emit(Invalid(ch)),
        }
    }

    fn lex_keyword(&mut self) -> Spanned<Token> {
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

        let word = &self.source[self.prev..self.pos];
        match KEYWORDS.iter().find(|&&(k, _)| k == word).map(|&(_, v)| v) {
            Some(val) => self.emit(val),
            None => self.emit(Identifier),
        }
    }

    fn lex_number(&mut self) -> Spanned<Token> {
        while let Some(p) = self.peek() {
            if !Self::is_digit(p) && p != '.' {
                break;
            }
            self.consume();
        }
        self.emit(Token::Number)
    }

    fn lex_string(&mut self) -> Spanned<Token> {
        self.consume_while(|ch| ch != '"');
        assert!(self.consume() == Some('\"'));
        self.emit(Token::String)
    }

    fn emit_or(&mut self, cmp: char, left: Token, right: Token) -> Spanned<Token> {
        if self.peek().expect("not EOF") == cmp {
            self.consume();
            self.emit(right)
        } else {
            self.emit(left)
        }
    }

    fn emit(&mut self, item: Token) -> Spanned<Token> {
        let pos = std::mem::replace(&mut self.prev, self.pos);
        let start = std::mem::replace(&mut self.previous, self.current);
        Spanned::new(item, Span::new(start, self.current, pos as _))
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

    const fn is_alpha(ch: char) -> bool {
        matches!(ch, 'a'..='z' | 'A'..='Z' | '_')
    }

    const fn is_digit(ch: char) -> bool {
        matches!(ch, '0'..='9')
    }
}
