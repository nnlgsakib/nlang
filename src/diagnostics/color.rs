pub fn red(s: &str) -> String { format!("\x1b[31m{}\x1b[0m", s) }
pub fn yellow(s: &str) -> String { format!("\x1b[33m{}\x1b[0m", s) }
pub fn blue(s: &str) -> String { format!("\x1b[34m{}\x1b[0m", s) }
pub fn bold(s: &str) -> String { format!("\x1b[1m{}\x1b[0m", s) }

pub fn error_tag() -> String { "❌".to_string() }
pub fn warn_tag() -> String { "⚠️".to_string() }
pub fn info_tag() -> String { "ℹ️".to_string() }
pub fn help_tag() -> String { format!("{} {}", "💡", bold(&yellow("help:"))) }

pub fn location(path: &str, line: usize, column: usize) -> String {
    format!("  {} {}:{}:{}", blue("-->"), path, line, column)
}

pub fn caret_line(line_text: &str, column: usize) -> String {
    let mut buf = String::new();
    buf.push_str("   |\n");
    buf.push_str(&format!("   | {}\n", line_text));
    let mut caret = String::new();
    caret.push_str("   | ");
    for _ in 1..column { caret.push(' '); }
    caret.push_str(&red("^"));
    buf.push_str(&caret);
    buf
}