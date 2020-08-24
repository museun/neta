#[derive(Debug, Copy, Clone)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

impl<T> std::ops::Index<Spanned<T>> for str {
    type Output = str;
    fn index(&self, index: Spanned<T>) -> &Self::Output {
        &self[index.span]
    }
}

impl std::ops::Index<Span> for str {
    type Output = str;
    fn index(&self, index: Span) -> &Self::Output {
        &self[index.start.col as usize..index.end.col as usize]
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Pos {
    pub line: u16,
    pub col: u16,
}

#[derive(Debug, Copy, Clone)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl std::ops::Add for Span {
    type Output = Span;
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start: self.start,
            end: rhs.end,
        }
    }
}

impl std::ops::Add<Pos> for Span {
    type Output = Span;
    fn add(self, rhs: Pos) -> Self::Output {
        Self {
            start: self.start,
            end: rhs,
        }
    }
}

impl std::ops::AddAssign<Span> for Span {
    fn add_assign(&mut self, rhs: Span) {
        self.end = rhs.end
    }
}
