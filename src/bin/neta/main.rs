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

fn run(_source: &str) -> Result<()> {
    // for token in neta::lexer::lex(source) {
    //     println!("{:?}: '{}'", token, &source[token])
    // }

    Ok(())
}
