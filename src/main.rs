use std::{iter::Peekable, str::Chars};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error>>;

fn main() -> Result<()> {
    let v = std::env::args().skip(1).collect::<Vec<_>>();
    match v.as_slice() {
        [] => run_prompt(),
        [script] => run_file(script),
        [..] => {
            eprintln!("usage: {} <script>", env!("CARGO_PKG_NAME"));
            std::process::exit(64)
        }
    }
}

fn run_file(file: &str) -> Result<()> {
    let data = std::fs::read(file)?;
    run(&String::from_utf8(data)?)
}

fn run_prompt() -> Result<()> {
    use std::io::prelude::*;

    let input = std::io::stdin();
    let mut input = input.lock();
    let mut output = std::io::stdout();

    let mut buf = String::new();

    loop {
        write!(&mut output, "> ")?;
        output.flush()?;

        if input.read_line(&mut buf)? == 0 {
            break Ok(());
        }
        run(&buf)?;
        buf.clear();
    }
}

fn run(source: &str) -> Result<()> {
    for token in lex(source) {
        println!("{:?}: '{}'", token, &source[token])
    }

    Ok(())
}

#[derive(Debug, Copy, Clone)]
struct Pos {
    line: u16,
    col: u16,
}

#[derive(Debug, Copy, Clone)]
struct Span<T> {
    item: T,
    start: Pos,
    end: Pos,
}

impl<T> std::ops::Index<Span<T>> for str {
    type Output = str;
    fn index(&self, index: Span<T>) -> &Self::Output {
        &self[index.start.col as usize..index.end.col as usize]
    }
}

#[derive(Debug, Copy, Clone)]
enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String,
    Number,

    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Invalid(char),
    Eof,
}

fn lex(source: &str) -> impl Iterator<Item = Span<Token>> + '_ {
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

    fn lex(&mut self) -> Span<Token> {
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

    fn lex_keyword(&mut self, start: Pos) -> Span<Token> {
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

    fn lex_number(&mut self, start: Pos) -> Span<Token> {
        while let Some(p) = self.peek() {
            if !Self::is_digit(p) && p != '.' {
                break;
            }
            self.consume();
        }
        self.emit(start, Token::Number)
    }

    fn lex_string(&mut self, start: Pos) -> Span<Token> {
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
    ) -> Span<Token> {
        if cmp(self.peek().expect("not EOF")) {
            self.consume();
            self.emit(start, right)
        } else {
            self.emit(start, left)
        }
    }

    fn emit(&mut self, start: Pos, item: Token) -> Span<Token> {
        Span {
            item,
            start,
            end: self.current,
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
