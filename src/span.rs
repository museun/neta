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

    pub const fn as_ref(self: &Self) -> Spanned<&T> {
        Spanned::new(&self.item, self.span)
    }

    pub fn map<S, F: Fn(Self) -> S>(self, map: F) -> Spanned<S> {
        let span = self.span;
        Spanned::new(map(self), span)
    }

    pub fn map_item<F: Fn(T) -> U, U>(self, map: F) -> Spanned<U> {
        Spanned::new(map(self.item), self.span)
    }

    pub fn map_span<S, F: Fn(T, Span) -> S>(self, map: F) -> Spanned<S> {
        Spanned::new(map(self.item, self.span), self.span)
    }
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
        let abs = index.abs as usize;
        let len = calc_distance(self, index.start, index.end);
        &self[abs..abs + len]
    }
}

// this assumes the end is always after the start
fn calc_distance(s: &str, start: Pos, end: Pos) -> usize {
    debug_assert!(start.line <= end.line, "{} < {}", start.line, end.line);
    let start_target = start.line as usize - 1;
    let end_target = start.line as usize - 1;

    let start_col = start.col as usize;
    let end_col = end.col as usize;

    let mut distance = 0;
    for (i, range) in s.lines().skip(start_target).enumerate() {
        if start_target == end_target {
            distance += end_col - start_col;
            break;
        }

        if end_target == i {
            distance += range[..end_col].len();
            break;
        }

        distance += if i == 0 && end_target != i {
            range[start_col..].len()
        } else {
            range.len()
        }
    }
    distance
}

#[derive(Default, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Pos {
    pub line: u16,
    pub col: u16,
}

impl std::fmt::Debug for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

impl Pos {
    pub const fn new(line: u16, col: u16) -> Self {
        Self { line, col }
    }

    pub const fn unknown() -> Self {
        Self::new(u16::max_value(), u16::max_value())
    }
}

#[derive(Default, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
    pub abs: u32,
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}..{:?} ({})", self.start, self.end, self.abs)
    }
}

impl Span {
    pub const fn new(start: Pos, end: Pos, abs: u32) -> Self {
        Self { start, end, abs }
    }

    pub const fn zero() -> Self {
        Self {
            start: Pos::new(0, 0),
            end: Pos::new(0, 0),
            abs: 0,
        }
    }

    pub const fn unknown() -> Self {
        Self {
            start: Pos::unknown(),
            end: Pos::unknown(),
            abs: 0,
        }
    }
}

impl std::ops::Add for Span {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start: self.start,
            end: rhs.end,
            abs: self.abs,
        }
    }
}

impl std::ops::Add<Pos> for Span {
    type Output = Self;
    fn add(self, rhs: Pos) -> Self::Output {
        Self {
            start: self.start,
            end: rhs,
            abs: self.abs,
        }
    }
}

impl std::ops::AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        self.end = rhs.end
    }
}
