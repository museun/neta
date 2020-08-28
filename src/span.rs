#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

#[allow(clippy::use_self)]
impl<T> Spanned<T> {
    pub const fn new(item: T, span: Span) -> Self {
        Self { item, span }
    }

    pub const fn as_ref(self: &Spanned<T>) -> Spanned<&T> {
        Spanned::new(&self.item, self.span)
    }

    // #[deprecated = "don't do this"]
    // pub fn map<S, F: Fn(Self) -> S>(self, map: F) -> Spanned<S> {
    //     let span = self.span;
    //     Spanned::new(map(self), span)
    // }

    // #[deprecated = "don't do this"]
    // pub fn map_item<F: Fn(T) -> U, U>(self, map: F) -> Spanned<U> {
    //     Spanned::new(map(self.item), self.span)
    // }

    // #[deprecated = "don't do this"]
    // pub fn map_span<S, F: Fn(T, Span) -> S>(self, map: F) -> Spanned<S> {
    //     Spanned::new(map(self.item, self.span), self.span)
    // }
}

impl<T> std::ops::Index<Spanned<T>> for str {
    type Output = Self;
    fn index(&self, index: Spanned<T>) -> &Self::Output {
        &self[index.span]
    }
}

impl std::ops::Index<&Span> for str {
    type Output = Self;
    fn index(&self, index: &Span) -> &Self::Output {
        &self[*index]
    }
}

impl std::ops::Index<Span> for str {
    type Output = Self;
    fn index(&self, index: Span) -> &Self::Output {
        // let abs = index.abs as usize;
        // let len = calc_distance(self, index.start, index.end);
        &self[index.start.abs as usize..index.end.abs as usize]
    }
}

#[derive(Default, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Pos {
    pub line: u16,
    pub col: u16,
    pub abs: u32,
}

impl std::fmt::Debug for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{} ({})", self.line, self.col, self.abs)
    }
}

impl Pos {
    pub const fn new(line: u16, col: u16, abs: u32) -> Self {
        Self { line, col, abs }
    }

    pub const fn unknown() -> Self {
        Self::new(u16::max_value(), u16::max_value(), 0)
    }
}

impl std::ops::Sub for Pos {
    type Output = i64;
    fn sub(self, rhs: Self) -> Self::Output {
        self.abs as i64 - rhs.abs as i64
    }
}

#[derive(Default, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}

impl Span {
    pub const fn new(start: Pos, end: Pos) -> Self {
        Self { start, end }
    }

    pub const fn zero() -> Self {
        Self {
            start: Pos::new(0, 0, 0),
            end: Pos::new(0, 0, 0),
        }
    }

    pub const fn unknown() -> Self {
        Self {
            start: Pos::unknown(),
            end: Pos::unknown(),
        }
    }
}

impl std::ops::Add for Span {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start: self.start,
            end: rhs.end,
        }
    }
}

impl std::ops::Add<Pos> for Span {
    type Output = Self;
    fn add(self, rhs: Pos) -> Self::Output {
        Self {
            start: self.start,
            end: rhs,
        }
    }
}

impl std::ops::AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        self.end = rhs.end
    }
}
