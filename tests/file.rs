extern crate pesty_php;

use pesty_php::*;

#[test]
fn parse_simple_file_without_end_tag() {
    let result = vec![ParsedItem::CodeBlock(
        vec![Expr::Echo(vec![Expr::String("test".into())])]
    )];
    assert_eq!(process_script("<?php echo \"test\";"), result);
    assert_eq!(process_script("<?php echo \"test\";\n"), result);
    assert_eq!(process_script("<?php echo \"test\";\n"), result);
}

#[test]
fn parse_simple_file_echos() {
    assert_eq!(process_script("before<?php echo \"test\"; ?>after1<?php echo \"end\"; ?>end"), vec![
        ParsedItem::Text("before".into()),
        ParsedItem::CodeBlock(vec![Expr::Echo(vec![Expr::String("test".into())])]),
        ParsedItem::Text("after1".into()),
        ParsedItem::CodeBlock(vec![Expr::Echo(vec![Expr::String("end".into())])]),
        ParsedItem::Text("end".into())
    ]);
}

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
// TEST invalid cases TODO: like <?php echo "test" (missing semicolon, should actually parse?)