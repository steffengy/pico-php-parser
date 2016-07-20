macro_rules! eb {
    (None, $e:expr) => {Box::new(enb!(None, $e))};
    ($s:expr, $end:expr, $e:expr) => {Box::new(enb!($s, $end, $e))};
}

macro_rules! enb {
    // 6 is the sizeof "<?php "
    (None, $e:expr) => {rnb!(0,0, $e)};
    ($s:expr, None, $e:expr) => {rnb!($s+6, 0, $e)};
    ($s:expr, $end:expr, $e:expr) => {rnb!($s+6, $end+6, $e)};
}

macro_rules! rb {
    (None, $e:expr) => {Box::new(rnb!(None, $e))};
    ($s:expr, $end:expr, $e:expr) => {Box::new(rnb!($s, $end, $e))};
}

macro_rules! rnb {
    (None, $e:expr) => {Expr($e, Span::new())};
    ($s:expr, None, $e:expr) => {Expr($e, Span { start: $s, ..Span::new()})};
    ($s:expr, $end:expr, $e:expr) => {Expr($e, Span { start:($s) as u32, end:($end) as u32, ..Span::new() })};
}

macro_rules! constant {
    (true) => {Expr_::Path(Path::Identifier("true".into()))};
    (false) => {Expr_::Path(Path::Identifier("false".into()))};
    (null) => {Expr_::Path(Path::Identifier("null".into()))};
}

mod expr;
mod file;
mod stmt;
