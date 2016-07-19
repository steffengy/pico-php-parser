use parser::*;

fn process_script(input: &str) -> Vec<Expr> {
    Parser::parse_str(&input).unwrap()
}

#[test]
fn parse_simple_file_without_end_tag() {
    let result = vec![rnb!(6, 10, Expr_::Echo(vec![ rnb!(11,17, Expr_::String("test".into())) ]))];
    assert_eq!(process_script("<?php echo \"test\";"), result);
    assert_eq!(process_script("<?php echo \"test\";\n"), result);
}

#[test]
fn parse_simple_file_echos() {
    assert_eq!(process_script(r#"before<?php echo "test"; ?>after1<?php echo "end"; ?>end"#), vec![
        rnb!(0,0, Expr_::Echo(vec![ rnb!(0,6, Expr_::String("before".into())) ])),
        rnb!(12,16, Expr_::Echo(vec![ rnb!(17,23, Expr_::String("test".into())) ])),
        rnb!(0,0, Expr_::Echo(vec![ rnb!(27,33, Expr_::String("after1".into())) ])),
        rnb!(39,43, Expr_::Echo(vec![ rnb!(44,49, Expr_::String("end".into())) ])),
        rnb!(0,0, Expr_::Echo(vec![ rnb!(53,56, Expr_::String("end".into())) ])),
    ]);
}

/*
#[test]
fn parse_simple_fn_decl() {
    assert_eq!(process_script(r#"<?php function hello_world() { echo "hello world"; } hello_world();"#), vec![
        ParsedItem::CodeBlock(vec![Expr::Decl(Decl::GlobalFunction("hello_world".into(), FunctionDecl {
                params: vec![],
                body: vec![Expr::Echo(vec![Expr::String("hello world".into())])], usev: vec![], ret_ref: false,
            })), Expr::Call(Box::new(Expr::Path(Path::Identifier("hello_world".into()))), vec![])
        ])
    ]);
}
*/
// TEST invalid cases TODO: like <?php echo "test" (missing semicolon, should actually parse?)
