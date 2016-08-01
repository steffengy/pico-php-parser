/// expression (boxed)
macro_rules! eb {
    ($s:expr, $end:expr, $e:expr) => {Box::new(enb!($s, $end, $e))};
}

/// expression (not boxed)
macro_rules! enb {
    // 6 is the sizeof "<?php "
    ($s:expr, $end:expr, $e:expr) => {rnb!($s+6, $end+6, $e)};
}

/// statement expression (not boxed)
macro_rules! st {
    ($s:expr, $end:expr, $st:expr) => {rsnb!($s+6, $end+6, $st)};
}

/// statement expression (wrapping a not-boxed expr)
macro_rules! senb {
    ($s:expr, $end:expr, $e:expr) => {st!($s, $end+1, Stmt_::Expr(enb!($s, $end, $e)))}
}

/// raw spanned expression (not boxed)
macro_rules! rnb {
    ($s:expr, $end:expr, $e:expr) => {Expr($e, Span { start:($s) as u32, end:($end) as u32, ..Span::new() })};
}

/// raw spanned statement (not boxed)
macro_rules! rsnb {
    ($s:expr, $end:expr, $st:expr) => {Stmt($st, Span { start: $s, end: $end, ..Span::new()})};
}

macro_rules! constant {
    (true) => {Expr_::Path(Path::identifier(false, "true".into()))};
    (false) => {Expr_::Path(Path::identifier(false, "false".into()))};
    (null) => {Expr_::Path(Path::identifier(false, "null".into()))};
}

mod expr;
mod file;
mod stmt;
