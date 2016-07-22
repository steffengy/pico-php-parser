use parser::*;

fn process_script(input: &str) -> Vec<Stmt> {
    Parser::parse_str(&input).unwrap()
}

#[test]
fn parse_simple_file_echos() {
    assert_eq!(process_script(r#"before<?php echo "test"; ?>after1<?php echo "end"; ?>end"#), vec![
        rsnb!(0,6, Stmt_::Echo(vec![ rnb!(0,6, Expr_::String("before".into())) ])),
        rsnb!(12,24, Stmt_::Echo(vec![ rnb!(17,23, Expr_::String("test".into())) ])),
        rsnb!(27,33, Stmt_::Echo(vec![ rnb!(27,33, Expr_::String("after1".into())) ])),
        rsnb!(39,50, Stmt_::Echo(vec![ rnb!(44,49, Expr_::String("end".into())) ])),
        rsnb!(53,56, Stmt_::Echo(vec![ rnb!(53,56, Expr_::String("end".into())) ])),
    ]);
}

// TEST invalid cases TODO: like <?php echo "test" (missing semicolon, should actually parse?)
