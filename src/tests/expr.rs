use parser::*;

fn process_expr(input: &str) -> Expr {
    let str_ = "<?php ".to_owned() + input + ";";
    let mut result = Parser::parse_str(&str_).unwrap();
    assert_eq!(result.len(), 1);
    match result.pop().unwrap() {
        Stmt(Stmt_::Expr(expr), _) => expr,
        _ => unreachable!(),
    }
}

#[test]
fn parse_expr_comment() {
    assert_eq!(process_expr("1/*test*/+/*test*/2"), enb!(0,19, Expr_::BinaryOp(Op::Add, eb!(0, 1, Expr_::Int(1)), eb!(18, 19, Expr_::Int(2)))));
    //TODO: doc comment tests
}

#[test]
fn parse_expr_op() {
    assert_eq!(process_expr(r#"1+2"#), enb!(0,3, Expr_::BinaryOp(Op::Add, eb!(0, 1, Expr_::Int(1)), eb!(2, 3, Expr_::Int(2)))));
    assert_eq!(process_expr(r#"1+2*3"#), enb!(0,5, Expr_::BinaryOp(Op::Add, eb!(0,1, Expr_::Int(1)), eb!(2,5, Expr_::BinaryOp(Op::Mul,
        eb!(2,3, Expr_::Int(2)), eb!(4, 5, Expr_::Int(3))))))
    );
    assert_eq!(process_expr(r#"2+$d**$c**$d"#), enb!(0, 12, Expr_::BinaryOp(Op::Add, eb!(0, 1, Expr_::Int(2)),
        eb!(2, 12, Expr_::BinaryOp(
            Op::Pow,
            eb!(2, 4, Expr_::Variable("d".into())),
            eb!(6, 12, Expr_::BinaryOp(Op::Pow, eb!(6, 8, Expr_::Variable("c".into())), eb!(10, 12, Expr_::Variable("d".into()))))
        ))
    )));
    assert_eq!(process_expr(r#"$g["a"]-$g["b"]/3"#), enb!(0,17, Expr_::BinaryOp(
        Op::Sub,
        eb!(0,7, Expr_::ArrayIdx(eb!(0,2, Expr_::Variable("g".into())), vec![ Some(enb!(3,6, Expr_::String("a".into()))) ])),
        eb!(8,17, Expr_::BinaryOp(Op::Div, eb!(8,15, Expr_::ArrayIdx(eb!(8,10, Expr_::Variable("g".into())), vec![
            Some(enb!(11,14, Expr_::String("b".into()))) ])), eb!(16,17, Expr_::Int(3)))
        )
    )));
}

#[test]
fn parse_expr_logical() {
    assert_eq!(process_expr("$a||$b"), enb!(0,6, Expr_::BinaryOp(Op::Or, eb!(0,2, Expr_::Variable("a".into())), eb!(4,6, Expr_::Variable("b".into())))));
    assert_eq!(process_expr("$a&&true"), enb!(0,8, Expr_::BinaryOp(Op::And, eb!(0,2, Expr_::Variable("a".into())), eb!(4,8, constant!(true)))));
    assert_eq!(process_expr("!$a"), enb!(0,3, Expr_::UnaryOp(UnaryOp::Not, eb!(1,3, Expr_::Variable("a".into())))));
    assert_eq!(process_expr("$a?false:true or $b"), enb!(0,19, Expr_::BinaryOp(Op::Or,
        eb!(0,13, Expr_::TernaryIf(eb!(0,2, Expr_::Variable("a".into())), Some(eb!(3,8, constant!(false))), eb!(9,13, constant!(true)))), eb!(17,19, Expr_::Variable("b".into()))
    )));
}

#[test]
fn parse_expr_parens() {
    assert_eq!(process_expr(r#"(1+2)*3"#), enb!(0,7, Expr_::BinaryOp(Op::Mul, eb!(0,5, Expr_::BinaryOp(Op::Add, eb!(1,2, Expr_::Int(1)), eb!(3,4, Expr_::Int(2)))), eb!(6,7, Expr_::Int(3)))));
    assert_eq!(process_expr(r#"(true||false)&&true"#), enb!(0,19, Expr_::BinaryOp(Op::And, eb!(0, 13,
        Expr_::BinaryOp(Op::Or, eb!(1,5, constant!(true)), eb!(7,12,constant!(false)))), eb!(15,19, constant!(true))
    )));
}

#[test]
fn parse_expr_string() {
    // TODO: fix line numbers of fragments (complex strings containing variables, etc.) (not tested yet)
    assert_eq!(process_expr(r#""""#), enb!(0, 2, Expr_::String("".into())));
    assert_eq!(process_expr(r#""t\nest\tsss\"os\"haha""#), enb!(0, 23, Expr_::String("t\nest\tsss\"os\"haha".into())));
    assert_eq!(process_expr(r#""\xe7\x9a\x84""#), enb!(0, 14, Expr_::String("的".into())));
    //assert_eq!(process_expr(r#""a\142\143d""#), Expr_::String("abcd".into()));
    assert_eq!(process_expr(r#""a\"b\\\"c\\\"d\"e""#), enb!(0, 19, Expr_::String(r#"a"b\"c\"d"e"#.into())));
    assert_eq!(process_expr(r#""abc\ClassName""#), enb!(0, 15, Expr_::String("abc\\ClassName".into())));
}

#[test]
fn parse_expr_string_fragmented() {
    assert_eq!(process_expr(r#""hello $world""#), enb!(0,14, Expr_::BinaryOp(Op::Concat, eb!(1,7, Expr_::String("hello ".into())), eb!(7,13, Expr_::Variable("world".into())))));
    assert_eq!(process_expr(r#""hello {$world}""#), enb!(0,16, Expr_::BinaryOp(Op::Concat, eb!(1,7, Expr_::String("hello ".into())), eb!(8,14, Expr_::Variable("world".into())))));
    assert_eq!(process_expr(r#""hello $wor->ld""#), enb!(0,16, Expr_::BinaryOp(Op::Concat, eb!(1,7, Expr_::String("hello ".into())), eb!(7,15, Expr_::ObjMember(
        eb!(7,11, Expr_::Variable("wor".into())), vec![ enb!(13,15, Expr_::Path(Path::identifier(false, "ld".into()))) ]
    )))));
    assert_eq!(process_expr(r#""hello ${world}""#), enb!(0,16, Expr_::BinaryOp(Op::Concat, eb!(1,7, Expr_::String("hello ".into())), eb!(7,15, Expr_::Variable("world".into())))));
}

#[test]
fn parse_expr_char_string() {
    assert_eq!(process_expr(r#"'\ntest\142'"#), enb!(0, 12, Expr_::String("\\ntest\\142".into())));
    assert_eq!(process_expr(r#"'a\'b\'c'"#), enb!(0,9, Expr_::String("a'b'c".into())));
    assert_eq!(process_expr(r#"'d\'e\\\'f\\\'\'g'"#), enb!(0,18, Expr_::String("d\'e\\\'f\\\'\'g".into())));
}

#[test]
fn parse_ns_identifier() {
    assert_eq!(process_expr("Test"), enb!(0, 4, Expr_::Path(Path::identifier(false, "Test".into()))));
    assert_eq!(process_expr("Test\\Abc"), enb!(0, 8, Expr_::Path(Path::ns_identifier(false, "Test".into(), "Abc".into()))));
    assert_eq!(process_expr("Test\\Ns1\\Ns2"), enb!(0, 12, Expr_::Path(Path::ns_identifier(false, "Test\\Ns1".into(), "Ns2".into()))));
    assert_eq!(process_expr("\\Test\\Ns1\\Ns2\\Ns3"), enb!(0, 17, Expr_::Path(Path::ns_identifier(true, "Test\\Ns1\\Ns2".into(), "Ns3".into()))));
}

#[test]
fn parse_expr_object_property() {
    //TODO: fix line numbers
    assert_eq!(process_expr(r#"$obj->prop"#), enb!(0,10, Expr_::ObjMember(eb!(0,4, Expr_::Variable("obj".into())), vec![
        enb!(6,10, Expr_::Path(Path::identifier(false, "prop".into())))
    ])));
    assert_eq!(process_expr(r#"$obj->a->b->c->d"#), enb!(0,16, Expr_::ObjMember(eb!(0,4, Expr_::Variable("obj".into())),
        vec![enb!(6,7, Expr_::Path(Path::identifier(false, "a".into()))), enb!(9,10, Expr_::Path(Path::identifier(false, "b".into()))),
            enb!(12,13, Expr_::Path(Path::identifier(false, "c".into()))), enb!(15,16, Expr_::Path(Path::identifier(false, "d".into())))])
    ));
    assert_eq!(process_expr(r#"$obj->$a->b"#), enb!(0,11, Expr_::ObjMember(eb!(0,4, Expr_::Variable("obj".into())), vec![
        enb!(6,8, Expr_::Variable("a".into())), enb!(10,11, Expr_::Path(Path::identifier(false, "b".into()))) ])
    ));
    assert_eq!(process_expr("$obj->{$obj->b}->c"), enb!(0,18, Expr_::ObjMember(eb!(0,4, Expr_::Variable("obj".into())), vec![
        enb!(7,14, Expr_::ObjMember(eb!(7,11, Expr_::Variable("obj".into())), vec![ enb!(13,14, Expr_::Path(Path::identifier(false, "b".into()))) ])),
            enb!(17,18, Expr_::Path(Path::identifier(false, "c".into())))
        ]))
    );
    assert_eq!(process_expr("$obj->{$a->{$b->c}->d}->e"), enb!(0,25, Expr_::ObjMember(eb!(0,4, Expr_::Variable("obj".into())), vec![
        enb!(7,21, Expr_::ObjMember(eb!(7,9, Expr_::Variable("a".into())), vec![
            enb!(12,17, Expr_::ObjMember(eb!(12,14, Expr_::Variable("b".into())), vec![ enb!(16,17, Expr_::Path(Path::identifier(false, "c".into()))) ])),
            enb!(20,21, Expr_::Path(Path::identifier(false, "d".into())))
        ])), enb!(24,25, Expr_::Path(Path::identifier(false, "e".into())))
    ])));
    assert_eq!(process_expr(r#"$obj->$a->b()"#), enb!(0,13, Expr_::Call(eb!(0,11, Expr_::ObjMember(
        eb!(0,4, Expr_::Variable("obj".into())),
        vec![ enb!(6,8, Expr_::Variable("a".into())), enb!(10,11, Expr_::Path(Path::identifier(false, "b".into()))) ]
    )), vec![])));
}

#[test]
fn parse_expr_array_idx() {
    assert_eq!(process_expr(r#"$test["a"]"#), enb!(0,10, Expr_::ArrayIdx(eb!(0,5, Expr_::Variable("test".into())), vec![ Some(enb!(6,9, Expr_::String("a".into()))) ])));
    assert_eq!(process_expr(r#"$test[9]"#), enb!(0,8, Expr_::ArrayIdx(eb!(0,5, Expr_::Variable("test".into())), vec![ Some(enb!(6,7, Expr_::Int(9))) ])));
    assert_eq!(process_expr(r#"$test["a"]['b\n']"#), enb!(0,17, Expr_::ArrayIdx(eb!(0,5, Expr_::Variable("test".into())), vec![
        Some(enb!(6,9, Expr_::String("a".into()))), Some(enb!(11,16, Expr_::String("b\\n".into())))
    ])));
    assert_eq!(process_expr(r#"$test[$g["a"]]["b"]["c"]"#), enb!(0,24, Expr_::ArrayIdx(eb!(0,5, Expr_::Variable("test".into())), vec![
        Some(enb!(6,13, Expr_::ArrayIdx(eb!(6, 8, Expr_::Variable("g".into())), vec![ Some(enb!(9,12,Expr_::String("a".into()))) ] ))),
        Some(enb!(15,18, Expr_::String("b".into()))),
        Some(enb!(20,23, Expr_::String("c".into())))
    ])));
}

#[test]
fn parse_expr_func_call() {
    assert_eq!(process_expr(r#"test()"#), enb!(0,6, Expr_::Call(eb!(0,4, Expr_::Path(Path::identifier(false, "test".into()))), vec![])));
    assert_eq!(process_expr(r#"array_()"#), enb!(0,8, Expr_::Call(eb!(0,6, Expr_::Path(Path::identifier(false, "array_".into()))), vec![])));
    assert_eq!(process_expr(r#"func_x(1, 2)"#), enb!(0,12, Expr_::Call(eb!(0,6, Expr_::Path(Path::identifier(false, "func_x".into()))),
        vec![ enb!(7,8, Expr_::Int(1)), enb!(10,11, Expr_::Int(2)) ]
    )));
    assert_eq!(process_expr(r#"func_x(abc(1), 2)"#), enb!(0,17, Expr_::Call(eb!(0,6, Expr_::Path(Path::identifier(false, "func_x".into()))), vec![
        enb!(7,13, Expr_::Call(eb!(7,10, Expr_::Path(Path::identifier(false, "abc".into()))), vec![ enb!(11,12, Expr_::Int(1)) ])),
        enb!(15, 16, Expr_::Int(2))
    ])));
    assert_eq!(process_expr(r#"$g[0]()"#), enb!(0,7, Expr_::Call(eb!(0,5, Expr_::ArrayIdx(eb!(0,2, Expr_::Variable("g".into())), vec![
        Some(enb!(3,4, Expr_::Int(0))) ])), vec![])));
    assert_eq!(process_expr(r#"$g[0]()[1](true)"#), enb!(0,16, Expr_::Call(
        eb!(0,10, Expr_::ArrayIdx(
            eb!(0,7, Expr_::Call(
                eb!(0,5, Expr_::ArrayIdx(eb!(0,2, Expr_::Variable("g".into())), vec![ Some(enb!(3,4, Expr_::Int(0))) ])),
                vec![]
            )), vec![ Some(enb!(8,9, Expr_::Int(1))) ]
        )), vec![ enb!(11,15, constant!(true)) ]
    )));
}

#[test]
fn parse_expr_require() {
    assert_eq!(process_expr("abc(require $path)"), enb!(0,18, Expr_::Call(eb!(0,3, Expr_::Path(Path::identifier(false, "abc".into()))),
        vec![ enb!(4,17, Expr_::Include(IncludeTy::Require, eb!(12,17, Expr_::Variable("path".into())))) ])
    ));
}

#[test]
fn parse_expr_isset() {
    assert_eq!(process_expr("isset($b)"), enb!(0,9, Expr_::Isset(vec![ enb!(6,8, Expr_::Variable("b".into())) ])));
}

#[test]
fn parse_expr_empty() {
    assert_eq!(process_expr("empty($b)"), enb!(0,9, Expr_::Empty(eb!(6,8, Expr_::Variable("b".into())))));
}

#[test]
fn parse_expr_cast() {
    assert_eq!(process_expr("(bool) $test"), enb!(0, 12, Expr_::Cast(Ty::Bool, eb!(7,12, Expr_::Variable("test".into())))));
    assert_eq!(process_expr("(int)$a['a']"), enb!(0, 12, Expr_::Cast(Ty::Int, eb!(5,12, Expr_::ArrayIdx(eb!(5,7, Expr_::Variable("a".into())), vec![
        Some(enb!(8,11, Expr_::String("a".into())))
    ])))));
    assert_eq!(process_expr("(int)ab()['c']"), enb!(0,14, Expr_::Cast(Ty::Int, eb!(5,14, Expr_::ArrayIdx(
        eb!(5,9, Expr_::Call(eb!(5,7, Expr_::Path(Path::identifier(false, "ab".into()))), vec![])),
        vec![ Some(enb!(10,13, Expr_::String("c".into()))) ]
    )))));
}

#[test]
fn parse_expr_error_control() {
    assert_eq!(process_expr("@test()"), enb!(0,7, Expr_::UnaryOp(UnaryOp::SilenceErrors, eb!(1,7, Expr_::Call(eb!(1,5, Expr_::Path(Path::identifier(false, "test".into()))), vec![])))));
}

#[test]
fn parse_expr_post_pre_dec_inc() {
    assert_eq!(process_expr("$c++"), enb!(0,4, Expr_::UnaryOp(UnaryOp::PostInc, eb!(0,2, Expr_::Variable("c".into())))));
    assert_eq!(process_expr("$c--"), enb!(0,4, Expr_::UnaryOp(UnaryOp::PostDec, eb!(0,2, Expr_::Variable("c".into())))));
    assert_eq!(process_expr("++$c"), enb!(0,4, Expr_::UnaryOp(UnaryOp::PreInc, eb!(2,4, Expr_::Variable("c".into())))));
    assert_eq!(process_expr("--$c"), enb!(0,4, Expr_::UnaryOp(UnaryOp::PreDec, eb!(2,4, Expr_::Variable("c".into())))));
}

#[test]
fn parse_expr_static_const() {
    assert_eq!(process_expr(r#"Obj::test"#), enb!(0,9, Expr_::StaticMember(eb!(0,3, Expr_::Path(Path::identifier(false, "Obj".into()))), vec![ enb!(5,9, Expr_::Path(
        Path::identifier(false, "test".into()))) ]))
    );
}

#[test]
fn parse_expr_static_property() {
    assert_eq!(process_expr("Obj::$test"), enb!(0,10, Expr_::StaticMember(eb!(0,3, Expr_::Path(Path::identifier(false, "Obj".into()))), vec![ enb!(5,10, Expr_::Variable("test".into())) ])));
    assert_eq!(process_expr("Obj::$a::$b"), enb!(0,11, Expr_::StaticMember(eb!(0,3, Expr_::Path(Path::identifier(false, "Obj".into()))), vec![
        enb!(5,7, Expr_::Variable("a".into())), enb!(9,11, Expr_::Variable("b".into()))
    ])));
    assert_eq!(process_expr("Obj::test()"), enb!(0,11, Expr_::Call(eb!(0,9, Expr_::StaticMember(eb!(0,3, Expr_::Path(Path::identifier(false, "Obj".into()))), vec![
        enb!(5,9, Expr_::Path(Path::identifier(false, "test".into())))
    ])), vec![])));
    assert_eq!(process_expr("static::$abc"), enb!(0,12, Expr_::StaticMember(eb!(0,6, Expr_::Path(Path::identifier(false, "static".into()))), vec![ enb!(8,12, Expr_::Variable("abc".into())) ])));
}

#[test]
fn parse_expr_ternary() {
    assert_eq!(process_expr("$test?true:false"), enb!(0,16, Expr_::TernaryIf(eb!(0,5, Expr_::Variable("test".into())),
        Some(eb!(6,10, constant!(true))), eb!(11,16, constant!(false)))
    ));
    assert_eq!(process_expr("!$test?true:false"), enb!(0,17, Expr_::TernaryIf(eb!(0,6, Expr_::UnaryOp(UnaryOp::Not,
        eb!(1,6, Expr_::Variable("test".into())))), Some(eb!(7,11, constant!(true))), eb!(12,17, constant!(false)))
    ));
    assert_eq!(process_expr("true?'y':false?'n':'y'"), enb!(0,22, Expr_::TernaryIf(eb!(0,14, Expr_::TernaryIf(
        eb!(0,4, constant!(true)),
        Some(eb!(5,8, Expr_::String("y".into()))),
        eb!(9,14, constant!(false)),
    )),
        Some(eb!(15,18, Expr_::String("n".into()))),
        eb!(19,22, Expr_::String("y".into())),
    )));
}

#[test]
fn parse_expr_new() {
    assert_eq!(process_expr("new TestA()"), enb!(0,11, Expr_::New(eb!(4,9, Expr_::Path(Path::identifier(false, "TestA".into()))), vec![])));
    assert_eq!(process_expr("new Foo\\Bar()"), enb!(0,13, Expr_::New(eb!(4,11, Expr_::Path(Path::ns_identifier(false, "Foo".into(), "Bar".into()))), vec![])));
    assert_eq!(process_expr("new Foo"), enb!(0,7, Expr_::New(eb!(4,7, Expr_::Path(Path::identifier(false, "Foo".into()))), vec![])));
}

#[test]
fn parse_expr_clone() {
    assert_eq!(process_expr("clone $test"), enb!(0,5, Expr_::Clone(eb!(6,11, Expr_::Variable("test".into())))));
}

#[test]
fn parse_expr_array_append() {
    // for now we support append-expressions like that, TODO: figure out error reporting (AST_Node -> Position in source file)
    assert_eq!(process_expr(r#"$test[]=1"#), enb!(0,9, Expr_::Assign(eb!(0,7, Expr_::ArrayIdx(eb!(0,5, Expr_::Variable("test".into())), vec![ None ])),
        eb!(8,9, Expr_::Int(1))
    )));
}

#[test]
fn parse_expr_assign() {
    assert_eq!(process_expr("($b=4)"), enb!(0,6, Expr_::Assign(eb!(1,3, Expr_::Variable("b".into())), eb!(4,5, Expr_::Int(4)))));
    assert_eq!(process_expr("!($b=1)"), enb!(0,7, Expr_::UnaryOp(UnaryOp::Not, eb!(1,7, Expr_::Assign(eb!(2,4, Expr_::Variable("b".into())),
        eb!(5,6, Expr_::Int(1)))))));
    assert_eq!(process_expr("!$b=1"), enb!(0,5, Expr_::UnaryOp(UnaryOp::Not, eb!(1,5, Expr_::Assign(eb!(1,3, Expr_::Variable("b".into())),
        eb!(4,5, Expr_::Int(1)))))));
}

#[test]
fn parse_expr_array() {
    assert_eq!(process_expr("[]"), enb!(0,2, Expr_::Array(vec![])));
    assert_eq!(process_expr("[1,]"), enb!(0,4, Expr_::Array(vec![ (None, enb!(1,2, Expr_::Int(1))) ])));
    assert_eq!(process_expr("[1, 2]"), enb!(0,6, Expr_::Array(vec![
        (None, enb!(1,2, Expr_::Int(1))), (None, enb!(4,5, Expr_::Int(2)))
    ])));
    assert_eq!(process_expr("[1, [2, 3], 3]"), enb!(0,14, Expr_::Array(vec![
        (None, enb!(1,2, Expr_::Int(1))), (None, enb!(4,10, Expr_::Array(vec![
            (None, enb!(5,6, Expr_::Int(2))), (None, enb!(8,9, Expr_::Int(3)))
        ]))),
        (None, enb!(12,13, Expr_::Int(3)))
    ])));
    assert_eq!(process_expr("array()"), enb!(0,7, Expr_::Array(vec![])));
}

#[test]
fn parse_expr_assoc_array() {
    assert_eq!(process_expr("['a' => 'b']"), enb!(0,12, Expr_::Array(vec![
        (Some(enb!(1,4, Expr_::String("a".into()))), enb!(8,11, Expr_::String("b".into())))
    ])));
}

#[test]
fn parse_expr_closure() {
    assert_eq!(process_expr("function () { c(); }"), enb!(0,20, Expr_::Function(FunctionDecl {
        params: vec![],
        body: Some(Block(vec![ senb!(14,17, Expr_::Call(eb!(14,15, Expr_::Path(Path::identifier(false, "c".into()))), vec![])) ])), usev: vec![], ret_ref: false, ret_type : None
    })));
}

#[test]
fn parse_expr_priority_parents_call() {
    assert_eq!(process_expr("(new Factory)->test"), enb!(0,19, Expr_::ObjMember(eb!(0,13, Expr_::New(eb!(5,12, Expr_::Path(Path::identifier(false, "Factory".into()))), vec![])),
        vec![ enb!(15,19, Expr_::Path(Path::identifier(false, "test".into()))) ]
    )));
    assert_eq!(process_expr("(new $obj)->method()"), enb!(0,20, Expr_::Call(eb!(0,18, Expr_::ObjMember(eb!(0,10, Expr_::New(eb!(5,9, Expr_::Variable("obj".into())), vec![])),
        vec![ enb!(12,18, Expr_::Path(Path::identifier(false, "method".into()))) ]
    )), vec![])));
}

#[test]
fn parse_expr_unpack() {
    assert_eq!(process_expr("t(...$email)"), enb!(0,12, Expr_::Call(eb!(0,1, Expr_::Path(Path::identifier(false, "t".into()))), vec![
        enb!(2,11, Expr_::Unpack(eb!(5,11, Expr_::Variable("email".into()))))
    ])));
}
