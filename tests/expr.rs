extern crate pesty_php;

use pesty_php::*;

fn assert_comment(input: &str, is_end: bool) {
    let mut parser = Rdp::new(StringInput::new(input));
    assert!(parser.comment());
    assert_eq!(parser.end(), is_end);
}

#[test]
fn parse_expr_op() {
    assert_eq!(process_expr(r#"1+2"#), Expr::BinaryOp(Op::Add, Box::new(Expr::Int(1)), Box::new(Expr::Int(2))));
    assert_eq!(process_expr(r#"1+2*3"#), Expr::BinaryOp(Op::Add, Box::new(Expr::Int(1)), Box::new(Expr::BinaryOp(Op::Mul, Box::new(Expr::Int(2)), Box::new(Expr::Int(3))))));
    assert_eq!(process_expr(r#"2+$d**$c**$d"#), Expr::BinaryOp(Op::Add, Box::new(Expr::Int(2)),
        Box::new(Expr::BinaryOp(
            Op::Pow,
            Box::new(Expr::Variable("d".into())),
            Box::new(Expr::BinaryOp(Op::Pow, Box::new(Expr::Variable("c".into())), Box::new(Expr::Variable("d".into()))))
        ))
    ));
    assert_eq!(process_expr(r#"$g["a"]-$g["b"]/3"#), Expr::BinaryOp(
        Op::Sub,
        Box::new(Expr::ArrayIdx(Box::new(Expr::Variable("g".into())), vec![Expr::String("a".into())])),
        Box::new(Expr::BinaryOp(Op::Div, Box::new(Expr::ArrayIdx(Box::new(Expr::Variable("g".into())), vec![Expr::String("b".into())])), Box::new(Expr::Int(3))))
    ));
}

#[test]
fn parse_expr_logical() {
    assert_eq!(process_expr(r#"$a||$b"#), Expr::BinaryOp(Op::Or, Box::new(Expr::Variable("a".into())), Box::new(Expr::Variable("b".into()))));
    assert_eq!(process_expr(r#"$a&&true"#), Expr::BinaryOp(Op::And, Box::new(Expr::Variable("a".into())), Box::new(Expr::True)));
    assert_eq!(process_expr(r#"!$a"#), Expr::UnaryOp(UnaryOp::Not, Box::new(Expr::Variable("a".into()))));
}

#[test]
fn parse_expr_parens() {
    assert_eq!(process_expr(r#"(1+2)*3"#), Expr::BinaryOp(Op::Mul, Box::new(Expr::BinaryOp(Op::Add, Box::new(Expr::Int(1)), Box::new(Expr::Int(2)))), Box::new(Expr::Int(3))));
    assert_eq!(process_expr(r#"(true||false)&&true"#), Expr::BinaryOp(Op::And, Box::new(Expr::BinaryOp(Op::Or, Box::new(Expr::True), Box::new(Expr::False))), Box::new(Expr::True)));
}

#[test]
fn parse_expr_string() {
    assert_eq!(process_expr(r#""t\nest\tsss\"os\"haha""#), Expr::String("t\nest\tsss\"os\"haha".into()));
    assert_eq!(process_expr(r#""\xe7\x9a\x84""#), Expr::String("的".into()));
    assert_eq!(process_expr(r#""a\142\143d""#), Expr::String("abcd".into()));
    assert_eq!(process_expr(r#""a\"b\\\"c\\\"d\"e""#), Expr::String(r#"a"b\"c\"d"e"#.into()));
}

#[test]
fn parse_expr_char_string() {
    assert_eq!(process_expr(r#"'\ntest\142'"#), Expr::String("\\ntest\\142".into()));
    assert_eq!(process_expr(r#"'a\'b\'c'"#), Expr::String("a'b'c".into()));
    assert_eq!(process_expr(r#"'d\'e\\\'f\\\'\'g'"#), Expr::String("d\'e\\\'f\\\'\'g".into()));
}

#[test]
fn parse_expr_array_idx() {
    assert_eq!(process_expr(r#"$test["a"]"#), Expr::ArrayIdx(Box::new(Expr::Variable("test".into())), vec![Expr::String("a".into())]));
    assert_eq!(process_expr(r#"$test["a"]['b\n']"#), Expr::ArrayIdx(Box::new(Expr::Variable("test".into())), vec![
        Expr::String("a".into()), Expr::String("b\\n".into())
    ]));
    assert_eq!(process_expr(r#"$test[$g["a"]]["b"]["c"]"#), Expr::ArrayIdx(Box::new(Expr::Variable("test".into())), vec![
        Expr::ArrayIdx(Box::new(Expr::Variable("g".into())), vec![Expr::String("a".into())]),
        Expr::String("b".into()),
        Expr::String("c".into())
    ]));
}

#[test]
fn parse_expr_array_append() {
    // for now we support append-expressions like that, TODO: figure out error reporting (AST_Node -> Position in source file)
    assert_eq!(process_expr(r#"$test[]=1"#), Expr::Assign(Box::new(Expr::ArrayIdx(Box::new(Expr::Variable("test".into())), vec![Expr::None])), Box::new(Expr::Int(1))));
}

#[test]
fn parse_expr_func_call() {
    assert_eq!(process_expr(r#"func_x(1, 2)"#), Expr::Call(Box::new(Expr::Path(Path::Identifier("func_x".into()))), vec![Expr::Int(1), Expr::Int(2)]));
    assert_eq!(process_expr(r#"func_x(abc(1), 2)"#), Expr::Call(Box::new(Expr::Path(Path::Identifier("func_x".into()))), vec![
        Expr::Call(Box::new(Expr::Path(Path::Identifier("abc".into()))), vec![Expr::Int(1)]),
        Expr::Int(2)
    ]));
    assert_eq!(process_expr(r#"$g[0]()"#), Expr::Call(Box::new(Expr::ArrayIdx(Box::new(Expr::Variable("g".into())), vec![Expr::Int(0)])), vec![]));
    assert_eq!(process_expr(r#"$g[0]()[1](true)"#), Expr::Call(
        Box::new(Expr::ArrayIdx(
            Box::new(Expr::Call(
                Box::new(Expr::ArrayIdx(Box::new(Expr::Variable("g".into())), vec![Expr::Int(0)])),
                vec![]
            )), vec![Expr::Int(1)]
        )), vec![Expr::True]
    ));
}

#[test]
fn parse_expr_object_property() {
    assert_eq!(process_expr(r#"$obj->prop"#), Expr::ObjMember(Box::new(Expr::Variable("obj".into())), vec![Expr::Path(Path::Identifier("prop".into())) ]));
    assert_eq!(process_expr(r#"$obj->$a->b"#), Expr::ObjMember(Box::new(Expr::Variable("obj".into())), vec![Expr::Variable("a".into()), Expr::Path(Path::Identifier("b".into())) ]));
    assert_eq!(process_expr(r#"$obj->a->b->c->d"#), Expr::ObjMember(Box::new(Expr::Variable("obj".into())),
        vec![Expr::Path(Path::Identifier("a".into())), Expr::Path(Path::Identifier("b".into())), Expr::Path(Path::Identifier("c".into())), Expr::Path(Path::Identifier("d".into())) ])
    );
    assert_eq!(process_expr("$obj->{$obj->b}->c"), Expr::ObjMember(Box::new(Expr::Variable("obj".into())), vec![
        Expr::ObjMember(Box::new(Expr::Variable("obj".into())), vec![Expr::Path(Path::Identifier("b".into())) ]), Expr::Path(Path::Identifier("c".into())) ])
    );
    assert_eq!(process_expr("$obj->{a->{b->c}->d}->e"), Expr::ObjMember(Box::new(Expr::Variable("obj".into())), vec![
        Expr::ObjMember(Box::new(Expr::Path(Path::Identifier("a".into()))), vec![
            Expr::ObjMember(Box::new(Expr::Path(Path::Identifier("b".into()))), vec![Expr::Path(Path::Identifier("c".into()))]), Expr::Path(Path::Identifier("d".into()))
        ]), Expr::Path(Path::Identifier("e".into()))
    ]));
    assert_eq!(process_expr(r#"$obj->$a->b()"#), Expr::Call(Box::new(Expr::ObjMember(
        Box::new(Expr::Variable("obj".into())),
        vec![ Expr::Variable("a".into()), Expr::Path(Path::Identifier("b".into())) ]
    )), vec![]));
}

#[test]
fn parse_expr_static_property() {
    assert_eq!(process_expr(r#"Obj::$test"#), Expr::StaticMember(Box::new(Expr::Path(Path::Identifier("Obj".into()))), vec![Expr::Variable("test".into())]));
    assert_eq!(process_expr(r#"Obj::$a::$b"#), Expr::StaticMember(Box::new(Expr::Path(Path::Identifier("Obj".into()))), vec![Expr::Variable("a".into()), Expr::Variable("b".into())]));
}

#[test]
fn parse_expr_comment() {
    assert_comment("//test", true);
    assert_comment("/*test*/", true);
    assert_comment("//test\ns", false);
    assert_comment("/*test*/s", false);
}

#[test]
fn parse_expr_post_pre_dec_inc() {
    assert_eq!(process_expr("$c++"), Expr::UnaryOp(UnaryOp::PostInc, Box::new(Expr::Variable("c".into()))));
    assert_eq!(process_expr("$c--"), Expr::UnaryOp(UnaryOp::PostDec, Box::new(Expr::Variable("c".into()))));
    assert_eq!(process_expr("++$c"), Expr::UnaryOp(UnaryOp::PreInc, Box::new(Expr::Variable("c".into()))));
    assert_eq!(process_expr("--$c"), Expr::UnaryOp(UnaryOp::PreDec, Box::new(Expr::Variable("c".into()))));
}

#[test]
fn parse_expr_closure() {
    assert_eq!(process_expr("function () { c(); }"), Expr::Function(FunctionDecl {
        params: vec![],
        body: vec![Expr::Call(Box::new(Expr::Path(Path::Identifier("c".into()))), vec![])], usev: vec![], ret_ref: false,
    }));
    assert_eq!(process_expr(r#"(new Factory)->test"#), Expr::ObjMember(Box::new(Expr::New(Path::Identifier("Factory".into()), vec![])),
        vec![Expr::Path(Path::Identifier("test".into()))])
    );
}

#[test]
fn parse_ns_identifier() {
    assert_eq!(process_expr("Test"), Expr::Path(Path::Identifier("Test".into())));
    assert_eq!(process_expr("Test\\Abc"), Expr::Path(Path::NsIdentifier("Test".into(), "Abc".into())));
    assert_eq!(process_expr("Test\\Ns1\\Ns2"), Expr::Path(Path::NsIdentifier("Test\\Ns1".into(), "Ns2".into())));
    assert_eq!(process_expr("\\Test\\Ns1\\Ns2\\Ns3"), Expr::Path(Path::NsIdentifier("Test\\Ns1\\Ns2".into(), "Ns3".into())));
}

#[test]
fn parse_expr_new() {
    assert_eq!(process_expr("new TestA()"), Expr::New(Path::Identifier("TestA".into()), vec![]));
    assert_eq!(process_expr("new Foo\\Bar()"), Expr::New(Path::NsIdentifier("Foo".into(), "Bar".into()), vec![]));
    assert_eq!(process_expr("new Foo"), Expr::New(Path::Identifier("Foo".into()), vec![]));
}

#[test]
fn parse_expr_array() {
    assert_eq!(process_expr("[]"), Expr::Array(vec![]));
    assert_eq!(process_expr("array()"), Expr::Array(vec![]));
    assert_eq!(process_expr("[1, 2]"), Expr::Array(vec![box_array_elem(Expr::None, Expr::Int(1)), box_array_elem(Expr::None, Expr::Int(2))]));
    assert_eq!(process_expr("[1, [2, 3], 3]"), Expr::Array(vec![
        box_array_elem(Expr::None, Expr::Int(1)), box_array_elem(Expr::None, Expr::Array(vec![
            box_array_elem(Expr::None, Expr::Int(2)), box_array_elem(Expr::None, Expr::Int(3)), box_array_elem(Expr::None, Expr::Int(3))
        ])
    )]));
}

fn box_array_elem<'a>(a: Expr<'a>, b: Expr<'a>) -> (Box<Expr<'a>>, Box<Expr<'a>>) {
    (Box::new(a), Box::new(b))
}

#[test]
fn parse_expr_reference() {
    assert_eq!(process_expr("&$test"), Expr::Reference(Box::new(Expr::Variable("test".into()))));
}

#[test]
fn parse_expr_ternary() {
    assert_eq!(process_expr("$test?true:false"), Expr::TernaryIf(Box::new(Expr::Variable("test".into())), Box::new(Expr::True), Box::new(Expr::False)));
}

#[test]
fn parse_expr_cast() {
    assert_eq!(process_expr("(bool) $test"), Expr::Cast(Ty::Bool, Box::new(Expr::Variable("test".into()))));
}

#[test]
fn parse_expr_assign() {
    assert_eq!(process_expr("($b=4)"), Expr::Assign(Box::new(Expr::Variable("b".into())), Box::new(Expr::Int(4))));
    let negate_assign_result = Expr::UnaryOp(UnaryOp::Not, Box::new(Expr::Assign(Box::new(Expr::Variable("b".into())), Box::new(Expr::Int(1)))));
    assert_eq!(process_expr("!($b=1)"), negate_assign_result);
    assert_eq!(process_expr("!$b=1"), negate_assign_result);
}

#[test]
fn parse_expr_isset() {
    assert_eq!(process_expr("isset($b)"), Expr::Isset(vec![Expr::Variable("b".into())]));
}

#[test]
fn parse_expr_empty() {
    assert_eq!(process_expr("empty($b)"), Expr::Empty(Box::new(Expr::Variable("b".into()))));
}

#[test]
fn parse_expr_error_control() {
    assert_eq!(process_expr("@test()"), Expr::ErrorControl(Box::new(Expr::Call(Box::new(Expr::Path(Path::Identifier("test".into()))), vec![]))));
}
