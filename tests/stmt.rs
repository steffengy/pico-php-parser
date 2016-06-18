extern crate pesty_php;

use pesty_php::*;

#[test]
fn parse_stmt_echo() {
    assert_eq!(process_stmt("echo 1;"), Expr::Echo(vec![Expr::Int(1)]));
}

#[test]
fn parse_stmt_return() {
    assert_eq!(process_stmt("return true;"), Expr::Return(Box::new(Expr::True)));
    assert_eq!(process_stmt("return;"), Expr::Return(Box::new(Expr::None)));
}

#[test]
fn parse_stmt_assignment() {
    assert_eq!(process_stmt(r#"$test=4;"#), Expr::Assign(Box::new(Expr::Variable("test".into())), Box::new(Expr::Int(4))));
    assert_eq!(process_stmt(r#"$test["a"]=4+$b;"#), Expr::Assign(Box::new(
        Expr::ArrayIdx(
            Box::new(Expr::Variable("test".into())),
            vec![Expr::String("a".into())]
        )),
        Box::new(Expr::BinaryOp(Op::Add, Box::new(Expr::Int(4)), Box::new(Expr::Variable("b".into()))))
    ));
}

#[test]
fn parse_stmt_if_while() {
    let expr_var = Expr::Variable("a".into());
    let block_stmt = Expr::Call(Box::new(Expr::Identifier("b".into())), vec![]);
    let block_expr = Expr::Block(vec![block_stmt.clone()]);
    let args = [
        ("if", Expr::If(Box::new(expr_var.clone()), Box::new(block_expr.clone()), None)),
        ("while", Expr::While(Box::new(expr_var.clone()), Box::new(block_expr)))
    ];
    for arg in &args {
        assert_eq!(process_stmt(&(arg.0.to_owned() + r#" ($a) { b(); }"#)), arg.1);
    }
    let args = [
        ("if", Expr::If(Box::new(expr_var.clone()), Box::new(block_stmt.clone()), None)),
        ("while", Expr::While(Box::new(expr_var.clone()), Box::new(block_stmt)))
    ];
    for arg in &args {
        assert_eq!(process_stmt(&(arg.0.to_owned() + r#"($a) b();"#)), arg.1);
    }
}

#[test]
fn parse_stmt_if_else() {
    let make_result = |block_expr, else_expr| Expr::If(Box::new(Expr::Variable("a".into())), Box::new(block_expr), Some(Box::new(else_expr)));
    let main_call_expr = Expr::Call(Box::new(Expr::Identifier("a".into())), vec![]);
    let block_expr = Expr::Block(vec![main_call_expr.clone()]);
    let call_expr = Expr::Call(Box::new(Expr::Identifier("b".into())), vec![]);
    let else_expr = Expr::Block(vec![call_expr.clone()]);

    assert_eq!(process_stmt("if ($a) { a(); } else { b(); }"), make_result(block_expr, else_expr));
    assert_eq!(process_stmt("if ($a) a(); else b();"), make_result(main_call_expr, call_expr));
    assert_eq!(process_stmt("if ($a) a(); else if ($b) b(); else c();"), Expr::If(
        Box::new(Expr::Variable("a".into())),
        Box::new(Expr::Call(Box::new(Expr::Identifier("a".into())), vec![])),
        Some(Box::new(
            Expr::If(Box::new(Expr::Variable("b".into())),
                Box::new(Expr::Call(Box::new(Expr::Identifier("b".into())), vec![])),
                Some(Box::new(Expr::Call(Box::new(Expr::Identifier("c".into())), vec![])))
            )
        ))
    ));
}

#[test]
fn parse_stmt_do_while() {
    assert_eq!(process_stmt("do { test(); } while(count($a));"), Expr::DoWhile(
        Box::new(Expr::Block(vec![Expr::Call(Box::new(Expr::Identifier("test".into())), vec![])])),
        Box::new(Expr::Call(Box::new(Expr::Identifier("count".into())), vec![Expr::Variable("a".into())]))
    ));
}

#[test]
fn parse_stmt_foreach() {
    assert_eq!(process_stmt("foreach ($test as $v) { ok(); }"), Expr::ForEach(
        Box::new(Expr::Variable("test".into())),
        None, // key
        Some(Box::new(Expr::Variable("v".into()))), // value
        Box::new(Expr::Block(vec![Expr::Call(Box::new(Expr::Identifier("ok".into())), vec![])])) //body
    ));
    assert_eq!(process_stmt("foreach ($test as $k => $v) { ok(); }"), Expr::ForEach(
        Box::new(Expr::Variable("test".into())),
        Some(Box::new(Expr::Variable("k".into()))), // key
        Some(Box::new(Expr::Variable("v".into()))), // value
        Box::new(Expr::Block(vec![Expr::Call(Box::new(Expr::Identifier("ok".into())), vec![])])) //body
    ));
}
