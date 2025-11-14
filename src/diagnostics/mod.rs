use std::path::Path;
use std::collections::HashSet;
pub mod color;

pub struct Span {
    pub line: usize,
    pub column: usize,
}

fn get_line(source: &str, line: usize) -> Option<&str> {
    source.lines().nth(line.saturating_sub(1))
}

fn find_column_in_line(line_text: &str, needle: Option<&str>) -> usize {
    if let Some(n) = needle {
        if !n.is_empty() {
            if let Some(pos) = line_text.find(n) {
                return pos + 1;
            }
        }
    }
    1
}

fn analyze_line_for_call_errors(line_text: &str) -> Option<(usize, String)> {
    let bytes = line_text.as_bytes();
    let trimmed = line_text.trim_start();
    if trimmed.starts_with("def ") {
        if let Some(op) = line_text.find('(') {
            let after = &line_text[op + 1..];
            let close = after.find(')');
            let arrow = line_text.find("->");
            let brace = line_text.find('{');
            if close.is_none() && (arrow.is_some() || brace.is_some()) {
                return Some((op + 1, "help: add ')' after parameters; use '()' for none".to_string()));
            }
        }
    }
    if trimmed.starts_with('.') {
        let leading_spaces = line_text.len() - trimmed.len();
        return Some((leading_spaces + 1, "help: method call missing receiver; add an object before '.' (e.g., name.upper())".to_string()));
    }
    if line_text.contains("=.") || line_text.contains("= .") {
        if let Some(pos) = line_text.find('.') {
            return Some((pos + 1, "help: method call missing receiver; add an object before '.' (e.g., obj.method())".to_string()));
        }
    }
    for i in 0..bytes.len() {
        if bytes[i] == b'.' {
            let mut j = i + 1;
            while j < bytes.len() && (bytes[j].is_ascii_alphanumeric() || bytes[j] == b'_') {
                j += 1;
            }
            if j < bytes.len() && bytes[j] == b')' {
                return Some((j + 1, "help: try adding parentheses after method name: '()'".to_string()));
            }
        }
    }
    if let Some(pos) = line_text.find(')') {
        let opens = line_text.matches('(').count();
        let closes = line_text.matches(')').count();
        if closes > opens {
            return Some((pos + 1, "help: remove the extra ')' or add a matching '('".to_string()));
        }
    }
    if let Some(dot_start) = line_text.find('.') {
        if let Some(open_after) = line_text[dot_start..].find('(') {
            let after_open = dot_start + open_after + 1;
            if let Some(inner_dot) = line_text[after_open..].find('.') {
                let col = after_open + inner_dot + 1;
                return Some((col, "help: try closing parentheses: ')' before chaining with '.'".to_string()));
            }
        }
    }
    None
}

fn analyze_line_for_type_errors(line_text: &str) -> Option<(usize, String)> {
    // Known type names
    let known = [
        "int","i8","i16","i32","i64","isize",
        "u8","u16","u32","u64","usize",
        "f32","f64","float","bool","string","void"
    ];
    // Tokenize identifiers with positions
    let bytes = line_text.as_bytes();
    let mut idx = 0usize;
    let mut best: Option<(usize, String, usize)> = None; // (col, suggestion, distance)
    while idx < bytes.len() {
        let ch = bytes[idx] as char;
        if ch.is_ascii_alphabetic() || ch == '_' {
            let start = idx;
            idx += 1;
            while idx < bytes.len() {
                let c = bytes[idx] as char;
                if c.is_ascii_alphanumeric() || c == '_' { idx += 1; } else { break; }
            }
            let ident = &line_text[start..idx];
            if !known.contains(&ident) {
                // compare to known types
                for k in known.iter() {
                    let d = edit_distance(ident, k);
                    // threshold: allow small edits
                    let thr = if ident.len() <= 3 { 1 } else { 2 };
                    if d > 0 && d <= thr {
                        match &best {
                            Some((_, _, bd)) if *bd <= d => {}
                            _ => {
                                best = Some((start + 1, (*k).to_string(), d));
                            }
                        }
                    }
                }
            }
        } else {
            idx += 1;
        }
    }
    if let Some((col, sugg, _)) = best {
        Some((col, format!("help: unknown type. Did you mean: {}?", sugg)))
    } else { None }
}


fn suggest(message: &str) -> Option<String> {
    let m = message.to_lowercase();
    if m.contains("missing receiver") || m.contains("got dot") {
        return Some("help: method call missing receiver; add an object before '.'".to_string());
    }
    if m.contains("expected parameter name") {
        return Some("help: add ')' after parameters; use '()' for none".to_string());
    }
    if m.contains("expected ';'") || m.contains("expected ';' after") {
        Some("help: try adding a semicolon: ';'".to_string())
    } else if m.contains("expected '}'") {
        Some("help: try adding a brace: '}'".to_string())
    } else if m.contains("expected ')'") {
        Some("help: try adding a closing parenthesis: ')'".to_string())
    } else if m.contains("expected ']'") {
        Some("help: try adding a closing bracket: ']'".to_string())
    } else if m.contains("expects") && m.contains("argument") {
        Some("help: check function signature and argument count".to_string())
    } else if m.contains("expected ','") {
        Some("help: try adding a comma: ','".to_string())
    } else if m.contains("expected expression") && m.contains("dot") {
        Some("help: try closing parentheses: ')' or add '()' after method name".to_string())
    } else {
        None
    }
}

fn render_caret(line_text: &str, column: usize) -> String { color::caret_line(line_text, column) }

pub fn emit_basic(
    title: &str,
    file_path: &Path,
    source: &str,
    span: Option<Span>,
    lexeme_hint: Option<&str>,
    message: &str,
) -> String {
    let (line, column, rendered) = if let Some(sp) = span.as_ref() {
        let line_text = get_line(source, sp.line).unwrap_or("");
        let mut col = if sp.column == 0 {
            find_column_in_line(line_text, lexeme_hint)
        } else { sp.column };
        // Apply heuristics for call/parenthesis errors when column unknown
        if sp.column == 0 {
            let is_unknown_method = {
                let tl = title.to_lowercase();
                let ml = message.to_lowercase();
                tl.contains("unknown string method") || ml.contains("unknown string method") || tl.contains("unknown array method") || ml.contains("unknown array method")
            };
            if !is_unknown_method {
                if let Some((hcol, _)) = analyze_line_for_call_errors(line_text) {
                    col = hcol;
                }
            }
            if title.to_lowercase().contains("expected type") {
                if let Some((tcol, _)) = analyze_line_for_type_errors(line_text) {
                    col = tcol;
                }
            }
        }
        (sp.line, col, render_caret(line_text, col))
    } else {
        (1, 1, String::new())
    };
    let mut out = String::new();
    out.push_str(&format!("{} {}\n", color::error_tag(), color::bold(&color::red(title))));
    out.push_str(&format!("{}\n", color::location(&file_path.display().to_string(), line, column)));
    out.push_str(&rendered);
    out.push('\n');
    out.push_str("   |\n");
    // Avoid duplicating the main error message as a help when title already conveys it
    let lt = title.to_lowercase();
    let lm = message.to_lowercase();
    let redundant =
        (lt.contains("parser error") && lm.starts_with("parse error")) ||
        (lt.contains("lexer error") && lm.starts_with("lexer error")) ||
        (lt.contains("runtime error") && lm.starts_with("runtime error")) ||
        (lt.contains("code generation error") && lm.starts_with("code generation error")) ||
        (lt.contains("i/o error") && lm.starts_with("i/o error")) ||
        (lt.contains("feature not implemented") && lm.starts_with("feature not implemented"));
    if !redundant {
        out.push_str(&format!("   = {} {}\n", color::help_tag(), message));
    }
    let mut help_used = false;
    if let Some(help) = suggest(message) {
        out.push_str(&format!("   = {} {}\n", color::help_tag(), help));
        help_used = true;
    }
    if !help_used {
        if let Some(sp) = span.as_ref() {
            if let Some(line_text) = get_line(source, sp.line) {
                let is_unknown_method = {
                    let tl = title.to_lowercase();
                    let ml = message.to_lowercase();
                    tl.contains("unknown string method") || ml.contains("unknown string method") || tl.contains("unknown array method") || ml.contains("unknown array method")
                };
                if !is_unknown_method {
                    if let Some((_, h)) = analyze_line_for_call_errors(line_text) {
                    out.push_str(&format!("   = {} {}\n", color::help_tag(), h));
                    }
                }
                if title.to_lowercase().contains("expected type") {
                    if let Some((_, h)) = analyze_line_for_type_errors(line_text) {
                        out.push_str(&format!("   = {}\n", h));
                    }
                }
                // Unknown method suggestions are added in from_execution_error; avoid duplication here
            }
        }
    }
    out
}

// Convenience for execution errors
use crate::execution_engine::ExecutionError;

pub fn from_execution_error(
    file_path: &Path,
    source: &str,
    err: &ExecutionError,
) -> String {
    match err {
        ExecutionError::ParserError(pe) => {
            emit_basic(
                &format!("Parser error: {}", pe.message),
                file_path,
                source,
                Some(Span { line: pe.line, column: 0 }),
                None,
                &pe.to_string(),
            )
        }
        ExecutionError::LexerError(le) => {
            emit_basic(
                &format!("Lexer error: {}", le.message),
                file_path,
                source,
                Some(Span { line: le.line, column: 0 }),
                None,
                &format!("Lexer error on line {}: {}", le.line, le.message),
            )
        }
        ExecutionError::SemanticError(se) => {
            let msg = se.to_string();
            // Try to enrich with span and suggestions for unknown symbol or method
            let (mut span_opt, mut extra_help) = enrich_undefined_symbol(source, &msg);
            if span_opt.is_none() {
                if let Some(mname) = extract_unknown_method(&msg) {
                    span_opt = find_method_span(source, &mname);
                    if extra_help.is_none() {
                        let lower = msg.to_lowercase();
                        if lower.contains("unknown array method") {
                            if let Some(help) = suggest_unknown_array_method(&mname) { extra_help = Some(help); }
                        } else if let Some(help) = suggest_unknown_method(&mname) { extra_help = Some(help); }
                    }
                }
            }
            let mut rendered = emit_basic(
                "Semantic error",
                file_path,
                source,
                span_opt,
                None,
                &msg,
            );
            if let Some(help) = extra_help { rendered.push_str(&format!("   = {}\n", help)); }
            rendered
        }
        ExecutionError::InterpreterError(ie) => {
            let msg = ie.to_string();
            let (mut span_opt, mut extra_help) = enrich_undefined_symbol(source, &msg);
            if span_opt.is_none() {
                if let Some(mname) = extract_unknown_method(&msg) {
                    span_opt = find_method_span(source, &mname);
                    if extra_help.is_none() {
                        let lower = msg.to_lowercase();
                        if lower.contains("unknown array method") {
                            if let Some(help) = suggest_unknown_array_method(&mname) { extra_help = Some(help); }
                        } else if let Some(help) = suggest_unknown_method(&mname) { extra_help = Some(help); }
                    }
                }
            }
            let mut rendered = emit_basic(
                "Runtime error",
                file_path,
                source,
                span_opt,
                None,
                &msg,
            );
            if let Some(help) = extra_help { rendered.push_str(&format!("   = {}\n", help)); }
            rendered
        }
        ExecutionError::CCodeGenError(ce) => {
            emit_basic(
                "Code generation error",
                file_path,
                source,
                None,
                None,
                &ce.to_string(),
            )
        }
        ExecutionError::IoError(ioe) => {
            emit_basic(
                "I/O error",
                file_path,
                source,
                None,
                None,
                &ioe.to_string(),
            )
        }
        ExecutionError::NotImplemented { message } => {
            emit_basic(
                "Feature not implemented",
                file_path,
                source,
                None,
                None,
                message,
            )
        }
    }
}

fn extract_unknown_method(msg: &str) -> Option<String> {
    let lower = msg.to_lowercase();
    if let Some(pos) = lower.find("unknown string method:") {
        let name = msg[pos + "Unknown string method:".len()..].trim().to_string();
        let name = name.trim_matches('"').trim().to_string();
        if !name.is_empty() { return Some(name); }
    }
    if let Some(pos) = lower.find("unknown array method:") {
        let name = msg[pos + "Unknown array method:".len()..].trim().to_string();
        let name = name.trim_matches('"').trim().to_string();
        if !name.is_empty() { return Some(name); }
    }
    None
}

fn find_method_span(source: &str, method: &str) -> Option<Span> {
    for (i, line) in source.lines().enumerate() {
        if let Some(dot_pos) = line.find('.') {
            if let Some(mpos) = line[dot_pos + 1..].find(method) {
                return Some(Span { line: i + 1, column: dot_pos + mpos + 2 });
            }
        }
    }
    None
}

fn suggest_unknown_method(bad: &str) -> Option<String> {
    let known = ["upper", "lower", "trim", "contains"]; 
    let thr = if bad.len() <= 3 { 1 } else { 2 };
    let mut best: Option<(&str, usize)> = None;
    for k in known.iter() {
        let d = edit_distance(bad, k);
        if d > 0 && d <= thr {
            match best {
                Some((_prev, pd)) if pd <= d => {}
                _ => best = Some((*k, d)),
            }
        }
    }
    best.map(|(s, _)| format!("help: unknown method. Did you mean: {}?", s))
}

fn suggest_unknown_array_method(bad: &str) -> Option<String> {
    let known = ["len"]; 
    let thr = if bad.len() <= 3 { 1 } else { 2 };
    let mut best: Option<(&str, usize)> = None;
    for k in known.iter() {
        let d = edit_distance(bad, k);
        if d > 0 && d <= thr {
            match best {
                Some((_prev, pd)) if pd <= d => {}
                _ => best = Some((*k, d)),
            }
        }
    }
    best.map(|(s, _)| format!("help: unknown array method. Did you mean: {}?", s))
}

fn enrich_undefined_symbol(source: &str, message: &str) -> (Option<Span>, Option<String>) {
    if let Some((kind, name)) = extract_undefined(message) {
        let span = find_name_span(source, &name);
        let candidates = if let Some(sp) = span.as_ref() { collect_identifiers_in_scope(source, sp.line) } else { collect_identifiers(source) };
        let mut scored: Vec<(usize, String)> = candidates
            .into_iter()
            .map(|c| (edit_distance(&name, &c), c))
            .collect();
        scored.sort_by_key(|(d, _)| *d);
        let mut help = String::new();
        if !scored.is_empty() {
            help.push_str(&format!("help: {} '{}' not found. Did you mean:", kind, name));
            let mut seen: HashSet<String> = HashSet::new();
            let mut top: Vec<String> = Vec::new();
            let threshold = if name.len() <= 3 { 1 } else { 2 };
            for (d, s) in scored.into_iter() {
                if s != name && d <= threshold && s.len() >= 2 {
                    if seen.insert(s.clone()) {
                        top.push(s);
                        if top.len() == 3 { break; }
                    }
                }
            }
            if !top.is_empty() {
                help.push_str(&format!(" {}", top.join(", ")));
            } else {
                help.push_str(" (no close matches)");
            }
        } else {
            help.push_str(&format!("help: {} '{}' not found. Consider declaring it", kind, name));
        }
        (span, Some(help))
    } else {
        (None, None)
    }
}

fn extract_undefined(msg: &str) -> Option<(&'static str, String)> {
    let lower = msg.to_lowercase();
    if let Some(pos) = lower.find("undefined variable:") {
        let name = msg[pos + "Undefined variable:".len()..].trim().to_string();
        return Some(("variable", name));
    }
    if let Some(pos) = lower.find("undefined function") {
        // Handles "Undefined function 'foo'"
        let tail = msg[pos + "Undefined function".len()..].trim().trim_matches(':').trim();
        let name = tail.trim_matches('"').trim_matches('\'').to_string();
        return Some(("function", name));
    }
    if let Some(pos) = lower.find("function not found") {
        let name = msg[pos + "Function not found".len()..].trim().trim_start_matches(':').trim().to_string();
        return Some(("function", name));
    }
    None
}

fn collect_identifiers(source: &str) -> Vec<String> {
    let mut set: HashSet<String> = HashSet::new();
    for line in source.lines() {
        let t = line.trim();
        if t.starts_with("store ") {
            let rest = &t[6..];
            if let Some(name) = rest.split_whitespace().next() {
                if !name.is_empty() { set.insert(name.to_string()); }
            }
        } else if t.starts_with("def ") {
            let rest = &t[4..];
            let name = rest.split('(').next().unwrap_or("").trim();
            if !name.is_empty() { set.insert(name.to_string()); }
            if let Some(params_part) = rest.split('(').nth(1) {
                let params_text = params_part.split(')').next().unwrap_or("");
                for p in params_text.split(',') {
                    let pname = p.trim().split(':').next().unwrap_or("").trim();
                    if !pname.is_empty() { set.insert(pname.to_string()); }
                }
            }
        }
    }
    set.into_iter().collect()
}

fn collect_identifiers_in_scope(source: &str, line: usize) -> Vec<String> {
    let mut set: HashSet<String> = HashSet::new();
    let functions = collect_function_names(source);
    for f in functions.iter() { set.insert(f.clone()); }
    if let Some((_fname, start, end)) = find_enclosing_function_range(source, line) {
        for (idx, l) in source.lines().enumerate() {
            if idx + 1 >= start && idx + 1 <= end {
                let t = l.trim();
                if t.starts_with("store ") {
                    let rest = &t[6..];
                    if let Some(name) = rest.split_whitespace().next() {
                        if !name.is_empty() { set.insert(name.to_string()); }
                    }
                } else if t.starts_with("def ") {
                    let rest = &t[4..];
                    let name = rest.split('(').next().unwrap_or("").trim();
                    if !name.is_empty() { set.insert(name.to_string()); }
                    if let Some(params_part) = rest.split('(').nth(1) {
                        let params_text = params_part.split(')').next().unwrap_or("");
                        for p in params_text.split(',') {
                            let pname = p.trim().split(':').next().unwrap_or("").trim();
                            if !pname.is_empty() { set.insert(pname.to_string()); }
                        }
                    }
                }
            }
        }
        return set.into_iter().collect();
    }
    collect_identifiers(source)
}

fn collect_function_names(source: &str) -> Vec<String> {
    let mut v = Vec::new();
    for line in source.lines() {
        let t = line.trim();
        if t.starts_with("def ") {
            let rest = &t[4..];
            let name = rest.split('(').next().unwrap_or("").trim();
            if !name.is_empty() { v.push(name.to_string()); }
        }
    }
    v
}

fn find_enclosing_function_range(source: &str, line: usize) -> Option<(String, usize, usize)> {
    let mut current: Option<(String, usize)> = None;
    let mut depth = 0i32;
    for (idx, l) in source.lines().enumerate() {
        let t = l.trim();
        if t.starts_with("def ") {
            let rest = &t[4..];
            let name = rest.split('(').next().unwrap_or("").trim().to_string();
            current = Some((name, idx + 1));
            depth = 0;
        }
        for ch in l.chars() {
            if ch == '{' { depth += 1; }
            else if ch == '}' { depth -= 1; }
        }
        if let Some((ref fname, start_line)) = current {
            if depth <= 0 && idx + 1 > start_line {
                if line >= start_line && line <= idx + 1 { return Some((fname.clone(), start_line, idx + 1)); }
                current = None;
            }
        }
    }
    None
}

fn find_name_span(source: &str, name: &str) -> Option<Span> {
    for (i, line) in source.lines().enumerate() {
        let bytes = line.as_bytes();
        let mut idx = 0;
        while idx < bytes.len() {
            let ch = bytes[idx] as char;
            let is_start = ch.is_ascii_alphabetic() || ch == '_';
            if is_start {
                let start = idx;
                idx += 1;
                while idx < bytes.len() {
                    let c = bytes[idx] as char;
                    if c.is_ascii_alphanumeric() || c == '_' { idx += 1; } else { break; }
                }
                let ident = &line[start..idx];
                if ident == name { return Some(Span { line: i + 1, column: start + 1 }); }
            } else {
                idx += 1;
            }
        }
    }
    None
}

fn edit_distance(a: &str, b: &str) -> usize {
    let mut dp = vec![vec![0; b.len() + 1]; a.len() + 1];
    for i in 0..=a.len() { dp[i][0] = i; }
    for j in 0..=b.len() { dp[0][j] = j; }
    let ab = a.as_bytes();
    let bb = b.as_bytes();
    for i in 1..=a.len() {
        for j in 1..=b.len() {
            let cost = if ab[i - 1] == bb[j - 1] { 0 } else { 1 };
            dp[i][j] = (dp[i - 1][j] + 1).min(dp[i][j - 1] + 1).min(dp[i - 1][j - 1] + cost);
        }
    }
    dp[a.len()][b.len()]
}