extern crate pesty_php;

use pesty_php::*;

fn process_decl(input: &str) -> Decl {
    let mut parser = Rdp::new(StringInput::new(input));
    assert!(parser.decls());
    println!("{:?}", parser.queue());
    assert!(parser.end());
    let mut result = parser.process().unwrap();
    assert_eq!(result.len(), 1);
    result.pop().unwrap()
}

#[test]
fn parse_function() {
    assert_eq!(process_decl("function test($a) { b(); }"), Decl::GlobalFunction("test".into(), FunctionDecl {
        params: vec![Expr::Identifier("a".into())],
        body: vec![Expr::Call(Box::new(Expr::Identifier("b".into())), vec![])]
    }));
}
