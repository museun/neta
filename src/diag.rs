use crate::span::Span;

#[allow(unused_macros)]
macro_rules! diag {
    ($span:expr) => {
        compiler_error!("not yet")
    };
    ($span:expr, $fmt:expr, $($args:expr),* $(,)?) => {
        $crate::diag::Diagnostic::error(format!($fmt, $($args),*), $span)
    };
}

#[derive(Copy, Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum Level {
    Warn,
    Error,
    Bug,
}

#[derive(Debug)]
pub struct Annotation {
    pub span: Span,
    pub info: String,
}

#[derive(Debug)]
pub struct Diagnostic {
    pub level: Level,
    pub primary: Annotation,
    pub info: Vec<String>,
    pub other: Vec<Annotation>,
}

impl Diagnostic {
    const fn from_annotation(level: Level, primary: Annotation) -> Self {
        Self {
            level,
            primary,
            info: Vec::new(),
            other: Vec::new(),
        }
    }

    pub fn error(message: impl Into<String>, span: Span) -> Self {
        let info = message.into();
        let anno = Annotation { span, info };
        Self::from_annotation(Level::Error, anno)
    }

    pub fn warn(message: impl Into<String>, span: Span) -> Self {
        let info = message.into();
        let anno = Annotation { span, info };
        Self::from_annotation(Level::Warn, anno)
    }

    pub fn bug(message: impl Into<String>, span: Span) -> Self {
        let info = message.into();
        let anno = Annotation { span, info };
        Self::from_annotation(Level::Bug, anno)
    }

    pub fn message(mut self, span: Span, message: impl Into<String>) -> Self {
        let info = message.into();
        let anno = Annotation { span, info };
        self.other.push(anno);
        self
    }

    pub fn info(mut self, message: impl Into<String>) -> Self {
        self.info.push(message.into());
        self
    }

    pub fn render(mut self, source: &str) -> String {
        let lines = source.lines().collect::<Vec<_>>();

        let mut output = format!("{:?}\n", self.level);
        self.other.insert(0, self.primary);

        for Annotation { span, info } in self.other {
            if span == Span::unknown() {
                output.push_str(&info);
                output.push('\n');
                continue;
            }

            let range = span.start.line.saturating_sub(1)..span.end.line;
            let width = count_digits((span.end.line + 1) as _);
            for n in range {
                output.push_str(&format!("{:>.*} | {}\n", width, n + 1, lines[n as usize]));
                // if span.start.line != n {
                //     continue;
                // }

                let empty = " ".repeat((span.start.col as usize) + 3 + width);
                let line = "^".repeat(span.end.col.saturating_sub(span.start.col) as usize);
                output.push_str(&format!("{}{}-- {}\n", empty, line, info))
            }

            if !self.info.is_empty() {
                output.push('\n');
            }

            self.info.iter().for_each(|s| {
                output.push_str("note: ");
                output.push_str(s);
                output.push('\n')
            });
        }

        output
    }
}

pub const fn count_digits(d: u64) -> usize {
    let (mut len, mut n) = (1, 1u64);
    while len < 20 {
        n *= 10;
        if n > d {
            return len;
        }
        len += 1;
    }
    len
}
