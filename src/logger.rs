#[derive(Clone)]
pub struct Logger {
    depth: usize,
    label: String,
}

impl Logger {
    #[cfg(not(feature = "logger"))]
    pub fn enter(_label: impl ToString) {}

    #[cfg(feature = "logger")]
    pub fn enter(label: impl ToString) -> impl Drop {
        thread_local! {
            static LOGGER: std::cell::RefCell<Logger> = std::cell::RefCell::new(Logger {
                label: "root".to_string(),
                depth: 0,
            });
        }

        fn dec(s: String) {
            LOGGER.with(|l| {
                let mut t = l.borrow_mut();
                t.depth -= 1;
                std::mem::replace(&mut t.label, s)
            });
        }

        fn inc(s: String) -> (String, usize) {
            LOGGER.with(|l| {
                let mut t = l.borrow_mut();
                t.depth += 1;
                (std::mem::replace(&mut t.label, s), t.depth)
            })
        }

        struct Guard {
            label: String,
            prev: String,
            depth: usize,
        }

        impl Drop for Guard {
            fn drop(&mut self) {
                dec(self.label.clone());
                eprintln!(
                    "{} {} <- {}",
                    "-".repeat(self.depth * 2),
                    self.prev,
                    self.label
                );
            }
        }

        let label = label.to_string();
        let (prev, depth) = inc(label.clone());

        eprintln!("{} {} -> {}", "-".repeat(depth * 2), prev, label);
        Guard { label, prev, depth }
    }
}
