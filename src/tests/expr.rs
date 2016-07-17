use parser::*;

/*
fn assert_comment(input: &str, is_end: bool) {
    let mut parser = Parser::parse_str(input);
    let mut parser = Rdp::new(StringInput::new(input));
    //TODO assert!(parser.comment());
    //TODO: assert_eq!(parser.end(), is_end);
}
*/

fn process_expr(input: &str) -> Expr {
    let str_ = "<?php ".to_owned() + input + ";";
    let mut result = Parser::parse_str(&str_);
    assert_eq!(result.len(), 1);
    result.pop().unwrap()
}

macro_rules! eb {
    (None, $e:expr) => {Box::new(enb!(None, $e))};
    ($s:expr, $end:expr, $e:expr) => {Box::new(enb!($s, $end, $e))};
}

macro_rules! enb {
    // 6 is the sizeof "<?php "
    (None, $e:expr) => {Expr($e, Span::new())};
    ($s:expr, $end:expr, $e:expr) => {Expr($e, Span { start:$s+6, end:$end+6, ..Span::new() })};
}

macro_rules! constant {
    (true) => {Expr_::Path(Path::Identifier("true".into()))};
    (false) => {Expr_::Path(Path::Identifier("false".into()))};
    (null) => {Expr_::Path(Path::Identifier("null".into()))};
}

#[test]
fn parse_expr_op() {
    assert_eq!(process_expr(r#"1+2"#), enb!(1,2, Expr_::BinaryOp(Op::Add, eb!(0, 1, Expr_::Int(1)), eb!(2, 3, Expr_::Int(2)))));
    assert_eq!(process_expr(r#"1+2*3"#), enb!(1,2, Expr_::BinaryOp(Op::Add, eb!(0,1, Expr_::Int(1)), eb!(3,4, Expr_::BinaryOp(Op::Mul, eb!(2,3, Expr_::Int(2)), eb!(4, 5, Expr_::Int(3)))))));
    assert_eq!(process_expr(r#"2+$d**$c**$d"#), enb!(1, 2, Expr_::BinaryOp(Op::Add, eb!(0, 1, Expr_::Int(2)),
        eb!(4, 6, Expr_::BinaryOp(
            Op::Pow,
            eb!(2, 4, Expr_::Variable("d".into())),
            eb!(8, 10, Expr_::BinaryOp(Op::Pow, eb!(6, 8, Expr_::Variable("c".into())), eb!(10, 12, Expr_::Variable("d".into()))))
        ))
    )));
    /*assert_eq!(process_expr(r#"$g["a"]-$g["b"]/3"#), Expr::BinaryOp(
        Op::Sub,
        Box::new(Expr::ArrayIdx(Box::new(Expr::Variable("g".into())), vec![Expr::String("a".into())])),
        Box::new(Expr::BinaryOp(Op::Div, Box::new(Expr::ArrayIdx(Box::new(Expr::Variable("g".into())), vec![Expr::String("b".into())])), Box::new(Expr::Int(3))))
    ));*/
}

#[test]
fn parse_expr_logical() {
    assert_eq!(process_expr(r#"$a||$b"#), enb!(2,4, Expr_::BinaryOp(Op::Or, eb!(0,2, Expr_::Variable("a".into())), eb!(4,6, Expr_::Variable("b".into())))));
    assert_eq!(process_expr(r#"$a&&true"#), enb!(2,4, Expr_::BinaryOp(Op::And, eb!(0,2, Expr_::Variable("a".into())), eb!(4,8, constant!(true)))));
    assert_eq!(process_expr(r#"!$a"#), enb!(0,1, Expr_::UnaryOp(UnaryOp::Not, eb!(1,3, Expr_::Variable("a".into())))));
}

#[test]
fn parse_expr_parens() {
    assert_eq!(process_expr(r#"(1+2)*3"#), enb!(5,6, Expr_::BinaryOp(Op::Mul, eb!(2,3, Expr_::BinaryOp(Op::Add, eb!(1,2, Expr_::Int(1)), eb!(3,4, Expr_::Int(2)))), eb!(6,7, Expr_::Int(3)))));
    assert_eq!(process_expr(r#"(true||false)&&true"#), enb!(13,15, Expr_::BinaryOp(Op::And, eb!(5, 7,
        Expr_::BinaryOp(Op::Or, eb!(1,5, constant!(true)), eb!(7,12,constant!(false)))), eb!(15,19, constant!(true))
    )));
}

#[test]
fn parse_expr_string() {
    // TODO: fix line numbers of fragments (complex strings containing variables, etc.) (not tested yet)
    assert_eq!(process_expr(r#""t\nest\tsss\"os\"haha""#), enb!(0, 23, Expr_::String("t\nest\tsss\"os\"haha".into())));
    //assert_eq!(process_expr(r#""\xe7\x9a\x84""#), enb!(0, 3, Expr_::String("的".into())));
    //assert_eq!(process_expr(r#""a\142\143d""#), Expr_::String("abcd".into()));
    assert_eq!(process_expr(r#""a\"b\\\"c\\\"d\"e""#), enb!(0, 19, Expr_::String(r#"a"b\"c\"d"e"#.into())));
    assert_eq!(process_expr(r#""abc\ClassName""#), enb!(0, 15, Expr_::String("abc\\ClassName".into())));
}

#[test]
fn parse_expr_char_string() {
    assert_eq!(process_expr(r#"'\ntest\142'"#), enb!(0, 12, Expr_::String("\\ntest\\142".into())));
    //assert_eq!(process_expr(r#"'a\'b\'c'"#), Expr::String("a'b'c".into()));
    //assert_eq!(process_expr(r#"'d\'e\\\'f\\\'\'g'"#), Expr::String("d\'e\\\'f\\\'\'g".into()));
}

#[test]
fn parse_ns_identifier() {
    assert_eq!(process_expr("Test"), enb!(0, 4, Expr_::Path(Path::Identifier("Test".into()))));
    assert_eq!(process_expr("Test\\Abc"), enb!(0, 8, Expr_::Path(Path::NsIdentifier("Test".into(), "Abc".into()))));
    assert_eq!(process_expr("Test\\Ns1\\Ns2"), enb!(0, 12, Expr_::Path(Path::NsIdentifier("Test\\Ns1".into(), "Ns2".into()))));
    //TODO: FQ!!
    //assert_eq!(process_expr("\\Test\\Ns1\\Ns2\\Ns3"), enb!(0, 17, Expr_::Path(Path::NsIdentifier("Test\\Ns1\\Ns2".into(), "Ns3".into()))));
}

#[test]
fn parse_expr_object_property() {
    //TODO: fix line numbers
    assert_eq!(process_expr(r#"$obj->prop"#), enb!(None, Expr_::ObjMember(eb!(0,4, Expr_::Variable("obj".into())), vec![enb!(6,10, Expr_::Path(Path::Identifier("prop".into()))) ])));
    assert_eq!(process_expr(r#"$obj->a->b->c->d"#), enb!(None, Expr_::ObjMember(eb!(0,4, Expr_::Variable("obj".into())),
        vec![enb!(6,7, Expr_::Path(Path::Identifier("a".into()))), enb!(9,10, Expr_::Path(Path::Identifier("b".into()))),
            enb!(12,13, Expr_::Path(Path::Identifier("c".into()))), enb!(15,16, Expr_::Path(Path::Identifier("d".into())))])
    ));
    assert_eq!(process_expr(r#"$obj->$a->b"#), enb!(None, Expr_::ObjMember(eb!(0,4, Expr_::Variable("obj".into())), vec![
        enb!(6,8, Expr_::Variable("a".into())), enb!(10,11, Expr_::Path(Path::Identifier("b".into()))) ])
    ));
    assert_eq!(process_expr("$obj->{$obj->b}->c"), enb!(None, Expr_::ObjMember(eb!(0,4, Expr_::Variable("obj".into())), vec![
        enb!(None, Expr_::ObjMember(eb!(7,11, Expr_::Variable("obj".into())), vec![ enb!(13,14, Expr_::Path(Path::Identifier("b".into()))) ])),
            enb!(17,18, Expr_::Path(Path::Identifier("c".into())))
        ]))
    );
    assert_eq!(process_expr("$obj->{$a->{$b->c}->d}->e"), enb!(None, Expr_::ObjMember(eb!(0,4, Expr_::Variable("obj".into())), vec![
        enb!(None, Expr_::ObjMember(eb!(7,9, Expr_::Variable("a".into())), vec![
            enb!(None, Expr_::ObjMember(eb!(12,14, Expr_::Variable("b".into())), vec![ enb!(16,17, Expr_::Path(Path::Identifier("c".into()))) ])),
            enb!(20,21, Expr_::Path(Path::Identifier("d".into())))
        ])), enb!(24,25, Expr_::Path(Path::Identifier("e".into())))
    ])));
    /*assert_eq!(process_expr(r#"$obj->$a->b()"#), enb!(None, Expr_::Call(eb!(None, Expr_::ObjMember(
        eb!(0,4, Expr_::Variable("obj".into())),
        vec![ enb!(6,8, Expr_::Variable("a".into())), enb!(10,11, Expr_::Path(Path::Identifier("b".into()))) ]
    )), vec![])));*/
}

#[test]
fn parse_expr_array_idx() {
    assert_eq!(process_expr(r#"$test["a"]"#), enb!(None, Expr_::ArrayIdx(eb!(0,5, Expr_::Variable("test".into())), vec![ enb!(6,9, Expr_::String("a".into())) ])));
    assert_eq!(process_expr(r#"$test[9]"#), enb!(None, Expr_::ArrayIdx(eb!(0,5, Expr_::Variable("test".into())), vec![ enb!(6,7, Expr_::Int(9)) ])));
    assert_eq!(process_expr(r#"$test["a"]['b\n']"#), enb!(None, Expr_::ArrayIdx(eb!(0,5, Expr_::Variable("test".into())), vec![
        enb!(6,9, Expr_::String("a".into())), enb!(11,16, Expr_::String("b\\n".into()))
    ])));
    assert_eq!(process_expr(r#"$test[$g["a"]]["b"]["c"]"#), enb!(None, Expr_::ArrayIdx(eb!(0,5, Expr_::Variable("test".into())), vec![
        enb!(None, Expr_::ArrayIdx(eb!(6, 8, Expr_::Variable("g".into())), vec![ enb!(9,12,Expr_::String("a".into())) ] )),
        enb!(15,18, Expr_::String("b".into())),
        enb!(20,23, Expr_::String("c".into()))
    ])));
}

#[test]
fn parse_expr_func_call() {
    assert_eq!(process_expr(r#"test()"#), enb!(None, Expr_::Call(eb!(0,4, Expr_::Path(Path::Identifier("test".into()))), vec![])));
    assert_eq!(process_expr(r#"func_x(1, 2)"#), enb!(None, Expr_::Call(eb!(0,6, Expr_::Path(Path::Identifier("func_x".into()))),
        vec![ enb!(7,8, Expr_::Int(1)), enb!(10,11, Expr_::Int(2)) ]
    )));
    assert_eq!(process_expr(r#"func_x(abc(1), 2)"#), enb!(None, Expr_::Call(eb!(0,6, Expr_::Path(Path::Identifier("func_x".into()))), vec![
        enb!(None, Expr_::Call(eb!(7,10, Expr_::Path(Path::Identifier("abc".into()))), vec![ enb!(11,12, Expr_::Int(1)) ])),
        enb!(15, 16, Expr_::Int(2))
    ])));
    assert_eq!(process_expr(r#"$g[0]()"#), enb!(None, Expr_::Call(eb!(None, Expr_::ArrayIdx(eb!(0,2, Expr_::Variable("g".into())), vec![ enb!(3,4, Expr_::Int(0)) ])), vec![])));
    assert_eq!(process_expr(r#"$g[0]()[1](true)"#), enb!(None, Expr_::Call(
        eb!(None, Expr_::ArrayIdx(
            eb!(None, Expr_::Call(
                eb!(None, Expr_::ArrayIdx(eb!(0,2, Expr_::Variable("g".into())), vec![ enb!(3,4, Expr_::Int(0)) ])),
                vec![]
            )), vec![ enb!(8,9, Expr_::Int(1)) ]
        )), vec![ enb!(11,15, constant!(true)) ]
    )));
}

#[test]
fn parse_expr_require() {
    assert_eq!(process_expr("abc(require $path)"), enb!(None, Expr_::Call(eb!(0,3, Expr_::Path(Path::Identifier("abc".into()))),
        vec![ enb!(4,11, Expr_::Include(IncludeTy::Require, eb!(12,17, Expr_::Variable("path".into())))) ])
    ));
}

#[test]
fn parse_expr_isset() {
    assert_eq!(process_expr("isset($b)"), enb!(0,5, Expr_::Isset(vec![ enb!(6,8, Expr_::Variable("b".into())) ])));
}

#[test]
fn parse_expr_empty() {
    assert_eq!(process_expr("empty($b)"), enb!(0,5, Expr_::Empty(eb!(6,8, Expr_::Variable("b".into())))));
}

#[test]
fn parse_expr_cast() {
    assert_eq!(process_expr("(bool) $test"), enb!(0, 6, Expr_::Cast(Ty::Bool, eb!(7,12, Expr_::Variable("test".into())))));
}

#[test]
fn parse_expr_error_control() {
    assert_eq!(process_expr("@test()"), enb!(0,1, Expr_::UnaryOp(UnaryOp::SilenceErrors, eb!(None, Expr_::Call(eb!(1,5, Expr_::Path(Path::Identifier("test".into()))), vec![])))));
}

#[test]
fn parse_expr_post_pre_dec_inc() {
    assert_eq!(process_expr("$c++"), enb!(2,4, Expr_::UnaryOp(UnaryOp::PostInc, eb!(0,2, Expr_::Variable("c".into())))));
    assert_eq!(process_expr("$c--"), enb!(2,4, Expr_::UnaryOp(UnaryOp::PostDec, eb!(0,2, Expr_::Variable("c".into())))));
    assert_eq!(process_expr("++$c"), enb!(0,2, Expr_::UnaryOp(UnaryOp::PreInc, eb!(2,4, Expr_::Variable("c".into())))));
    assert_eq!(process_expr("--$c"), enb!(0,2, Expr_::UnaryOp(UnaryOp::PreDec, eb!(2,4, Expr_::Variable("c".into())))));
}

/*
#[test]
fn parse_expr_array_append() {
    // for now we support append-expressions like that, TODO: figure out error reporting (AST_Node -> Position in source file)
    assert_eq!(process_expr(r#"$test[]=1"#), Expr::Assign(Box::new(Expr::ArrayIdx(Box::new(Expr::Variable("test".into())), vec![Expr::None])), Box::new(Expr::Int(1))));
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
fn parse_expr_closure() {
    assert_eq!(process_expr("function () { c(); }"), Expr::Function(FunctionDecl {
        params: vec![],
        body: vec![Expr::Call(Box::new(Expr::Path(Path::Identifier("c".into()))), vec![])], usev: vec![], ret_ref: false,
    }));
    assert_eq!(process_expr(r#"(new Factory)->test"#), Expr::ObjMember(Box::new(Expr::New(Box::new(Expr::Path(Path::Identifier("Factory".into()))), vec![])),
        vec![Expr::Path(Path::Identifier("test".into()))])
    );
}

#[test]
fn parse_expr_new() {
    assert_eq!(process_expr("new TestA()"), Expr::New(Box::new(Expr::Path(Path::Identifier("TestA".into()))), vec![]));
    assert_eq!(process_expr("new Foo\\Bar()"), Expr::New(Box::new(Expr::Path(Path::NsIdentifier("Foo".into(), "Bar".into()))), vec![]));
    assert_eq!(process_expr("new Foo"), Expr::New(Box::new(Expr::Path(Path::Identifier("Foo".into()))), vec![]));
}

#[test]
fn parse_expr_array() {
    assert_eq!(process_expr("[]"), Expr::Array(vec![]));
    assert_eq!(process_expr("[1,]"), Expr::Array(vec![(Expr::None, Expr::Int(1)) ]));
    assert_eq!(process_expr("array()"), Expr::Array(vec![]));
    assert_eq!(process_expr("[1, 2]"), Expr::Array(vec![(Expr::None, Expr::Int(1)), (Expr::None, Expr::Int(2))]));
    assert_eq!(process_expr("[1, [2, 3], 3]"), Expr::Array(vec![
        (Expr::None, Expr::Int(1)), (Expr::None, Expr::Array(vec![
            (Expr::None, Expr::Int(2)), (Expr::None, Expr::Int(3))
        ])
    ), (Expr::None, Expr::Int(3))]));
}

#[test]
fn parse_expr_reference() {
    assert_eq!(process_expr("&$test"), Expr::Reference(Box::new(Expr::Variable("test".into()))));
}

#[test]
fn parse_expr_ternary() {
    assert_eq!(process_expr("$test?true:false"), Expr::TernaryIf(Box::new(Expr::Variable("test".into())), Box::new(Expr::True), Box::new(Expr::False)));
    assert_eq!(process_expr("!$test?true:false"), Expr::TernaryIf(Box::new(Expr::UnaryOp(UnaryOp::Not,
        Box::new(Expr::Variable("test".into())))), Box::new(Expr::True), Box::new(Expr::False))
    );
}

#[test]
fn parse_expr_assign() {
    assert_eq!(process_expr("($b=4)"), Expr::Assign(Box::new(Expr::Variable("b".into())), Box::new(Expr::Int(4))));
    let negate_assign_result = Expr::UnaryOp(UnaryOp::Not, Box::new(Expr::Assign(Box::new(Expr::Variable("b".into())), Box::new(Expr::Int(1)))));
    assert_eq!(process_expr("!($b=1)"), negate_assign_result);
    assert_eq!(process_expr("!$b=1"), negate_assign_result);
}

#[test]
fn parse_expr_clone() {
    assert_eq!(process_expr("clone $test"), Expr::Clone(Box::new(Expr::Variable("test".into()))));
}

#[test]
fn parse_expr_priority_parents_call() {
    assert_eq!(process_expr("(new $obj)->method()"), Expr::Call(Box::new(Expr::ObjMember(Box::new(Expr::New(Box::new(Expr::Variable("obj".into())), vec![])),
        vec![Expr::Path(Path::Identifier("method".into()))]
    )), vec![]));
}
*/