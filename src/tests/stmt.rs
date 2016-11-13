use parser::*;

fn process_stmt(input: &str) -> Stmt {
    let str_ = "<?php ".to_owned() + input;
    let mut result = Parser::parse_str(&str_).unwrap();
    assert_eq!(result.len(), 1);
    result.pop().unwrap()
}

#[test]
fn parse_stmt_heredoc() {
    assert_eq!(process_stmt("<<<EOT\ntest\nEOT;\n"), senb!(0,15, Expr_::String("test".into())));
}

#[test]
fn parse_stmt_echo() {
    assert_eq!(process_stmt("echo 1;"), st!(0,7, Stmt_::Echo(vec![ enb!(5,6, Expr_::Int(1)) ])));
}

#[test]
fn parse_stmt_return() {
    assert_eq!(process_stmt("return true;"), st!(0,12, Stmt_::Return(Some(eb!(7,11, constant!(true))))));
    assert_eq!(process_stmt("return;"), st!(0,7, Stmt_::Return(None)));
}

#[test]
fn parse_stmt_continue() {
    assert_eq!(process_stmt("continue;"), st!(0,9, Stmt_::Continue(None)));
    assert_eq!(process_stmt("continue 2;"), st!(0,11, Stmt_::Continue(Some(eb!(9,10, Expr_::Int(2))))));
}

#[test]
fn parse_stmt_break() {
    assert_eq!(process_stmt("break;"), st!(0,6, Stmt_::Break(None)));
    assert_eq!(process_stmt("break 2;"), st!(0,8, Stmt_::Break(Some(eb!(6,7, Expr_::Int(2))))));
}

#[test]
fn parse_stmt_unset() {
    assert_eq!(process_stmt("unset($a);"), st!(0,10, Stmt_::Unset(vec![ enb!(6,8, Expr_::Variable("a".into())) ])));
}

#[test]
fn parse_stmt_new() {
    /*assert_eq!(process_stmt("return new $var($this);"), enb!(0,6, Expr_::Return(Some(eb!(7,10, Expr_::New(eb!(11,15, Expr_::Variable("var".into())),
        vec![ enb!(16,21, Expr_::Variable("this".into())) ]))
    ))));*/
}

#[test]
fn parse_stmt_throw() {
    assert_eq!(process_stmt(r#"throw new Exception("test");"#), st!(0,28, Stmt_::Throw(eb!(6,27, Expr_::New(eb!(10,19, Expr_::Path(Path::identifier(false, "Exception".into()))),
        vec![ enb!(20,26, Expr_::String("test".into())) ])))
    ));
    assert_eq!(process_stmt(r#"throw new Exception;"#), st!(0,20, Stmt_::Throw(eb!(6,19, Expr_::New(eb!(10,19, Expr_::Path(Path::identifier(false, "Exception".into()))), vec![])))));
}

#[test]
fn parse_stmt_include() {
    assert_eq!(process_stmt("include $test;"), senb!(0,13, Expr_::Include(IncludeTy::Include, eb!(8,13, Expr_::Variable("test".into())))));
    assert_eq!(process_stmt("include_once($test);"), senb!(0,19, Expr_::Include(IncludeTy::IncludeOnce, eb!(12,19, Expr_::Variable("test".into())))));
    assert_eq!(process_stmt("require($test);"), senb!(0,14, Expr_::Include(IncludeTy::Require, eb!(7,14, Expr_::Variable("test".into())))));
    assert_eq!(process_stmt("require_once $test;"), senb!(0,18, Expr_::Include(IncludeTy::RequireOnce, eb!(13,18, Expr_::Variable("test".into())))));
}

#[test]
fn parse_stmt_exit() {
    assert_eq!(process_stmt("exit;"), senb!(0,4, Expr_::Exit(None)));
    // TODO: Test that this fails (as it should): assert_eq!(process_stmt("exit true;"), Expr_::Exit(Box::new(Expr_::True)));
    assert_eq!(process_stmt("exit(true);"), senb!(0,10, Expr_::Exit(Some(eb!(5,9, constant!(true))))));
}

#[test]
fn parse_stmt_assignment() {
    assert_eq!(process_stmt(r#"$test=4;"#), senb!(0,7, Expr_::Assign(eb!(0,5, Expr_::Variable("test".into())), eb!(6,7, Expr_::Int(4)))));
    assert_eq!(process_stmt(r#"$test["a"]=4+$b;"#), senb!(0,15, Expr_::Assign(eb!(0,10,
        Expr_::ArrayIdx(
            eb!(0,5, Expr_::Variable("test".into())),
            vec![ Some(enb!(6,9, Expr_::String("a".into()))) ]
        )),
        eb!(11,15, Expr_::BinaryOp(Op::Add, eb!(11, 12, Expr_::Int(4)), eb!(13,15, Expr_::Variable("b".into()))))
    )));
}

#[test]
fn parse_stmt_return_ternary_assign() {
    assert_eq!(process_stmt("return isset($a) ? $b : $c = $d;"), st!(0,32, Stmt_::Return(Some(eb!(7,31, Expr_::TernaryIf(
        eb!(7,16, Expr_::Isset(vec![ enb!(13,15, Expr_::Variable("a".into())) ])), Some(eb!(19,21, Expr_::Variable("b".into()))),
        eb!(24,31, Expr_::Assign(eb!(24,26, Expr_::Variable("c".into())), eb!(29,31, Expr_::Variable("d".into()))))
    ))))));
}

#[test]
fn parse_stmt_compound_assignment() {
    assert_eq!(process_stmt("$test+=4;"), senb!(0,8, Expr_::CompoundAssign(eb!(0,5, Expr_::Variable("test".into())), Op::Add, eb!(7,8, Expr_::Int(4)))));
    assert_eq!(process_stmt("$test|=0;"), senb!(0,8, Expr_::CompoundAssign(eb!(0,5, Expr_::Variable("test".into())), Op::Or, eb!(7,8, Expr_::Int(0)))));
}

#[test]
fn parse_stmt_if_assign() {
    assert_eq!(process_stmt("if (! $a && $b = $c) { echo 1; }"), st!(0,32, Stmt_::If(eb!(4,19, Expr_::BinaryOp(Op::And,
        eb!(4,8, Expr_::UnaryOp(UnaryOp::Not, eb!(6,8, Expr_::Variable("a".into())))),
        eb!(12,19, Expr_::Assign(eb!(12,14, Expr_::Variable("b".into())), eb!(17,19, Expr_::Variable("c".into())))))
    ), Block(vec![ st!(23,30, Stmt_::Echo(vec![ enb!(28,29, Expr_::Int(1)) ])) ]), Block(vec![]))));
}

#[test]
fn parse_stmt_if_while() {
    assert_eq!(process_stmt("if   ($a) { b(); }"), st!(0,18, Stmt_::If(eb!(6,8, Expr_::Variable("a".into())), Block(vec![
        senb!(12,15, Expr_::Call(eb!(12,13, Expr_::Path(Path::identifier(false, "b".into()))), vec![]))
    ]), Block(vec![]))));
    assert_eq!(process_stmt("while($a) b();"), st!(0,14, Stmt_::While(eb!(6,8, Expr_::Variable("a".into())), Block(vec![
        senb!(10,13, Expr_::Call(eb!(10,11, Expr_::Path(Path::identifier(false, "b".into()))), vec![]))
    ]))));
}

#[test]
fn parse_stmt_if_else() {
    //TODO: fix line numbers (`if { } else { }` has the length of the end_position of the 2. } insteadof the 1. }
    assert_eq!(process_stmt("if ($a) { a(); } else { b(); }"), st!(0,30, Stmt_::If(
        eb!(4,6, Expr_::Variable("a".into())),
        Block(vec![ senb!(10,13, Expr_::Call(eb!(10,11, Expr_::Path(Path::identifier(false, "a".into()))), vec![])) ]),
        Block(vec![ senb!(24,27, Expr_::Call(eb!(24,25, Expr_::Path(Path::identifier(false, "b".into()))), vec![])) ]),
    )));
    assert_eq!(process_stmt("if ($a) a(); else b();"), st!(0,22, Stmt_::If(
        eb!(4,6, Expr_::Variable("a".into())),
        Block(vec![ senb!(8,11, Expr_::Call(eb!(8,9, Expr_::Path(Path::identifier(false, "a".into()))), vec![])) ]),
        Block(vec![ senb!(18,21, Expr_::Call(eb!(18,19, Expr_::Path(Path::identifier(false, "b".into()))), vec![])) ]),
    )));
    //if, elseif, else
    assert_eq!(process_stmt("if ($a) a(); else if ($b) b(); else c();"), st!(0,40, Stmt_::If(
        eb!(4,6, Expr_::Variable("a".into())),
        Block(vec![ senb!(8,11, Expr_::Call(eb!(8,9, Expr_::Path(Path::identifier(false, "a".into()))), vec![])) ]),
        Block(vec![ st!(18,40, Stmt_::If(
            eb!(22,24, Expr_::Variable("b".into())),
            Block(vec![ senb!(26,29, Expr_::Call(eb!(26,27, Expr_::Path(Path::identifier(false, "b".into()))), vec![])) ]),
            Block(vec![ senb!(36,39, Expr_::Call(eb!(36,37, Expr_::Path(Path::identifier(false, "c".into()))), vec![])) ]),
        )) ]),
    )));
    assert_eq!(process_stmt("if ($a) a(); elseif ($b)  b(); else c();"), st!(0,40, Stmt_::If(
        eb!(4,6, Expr_::Variable("a".into())),
        Block(vec![ senb!(8,11, Expr_::Call(eb!(8,9, Expr_::Path(Path::identifier(false, "a".into()))), vec![])) ]),
        Block(vec![ st!(13,40, Stmt_::If(
            eb!(21,23, Expr_::Variable("b".into())),
            Block(vec![ senb!(26,29, Expr_::Call(eb!(26,27, Expr_::Path(Path::identifier(false, "b".into()))), vec![])) ]),
            Block(vec![ senb!(36,39, Expr_::Call(eb!(36,37, Expr_::Path(Path::identifier(false, "c".into()))), vec![])) ]),
        )) ]),
    )));
}

#[test]
fn parse_stmt_do_while() {
    assert_eq!(process_stmt("do { test(); } while(count($a));"), st!(0,32, Stmt_::DoWhile(
        Block(vec![ senb!(5,11, Expr_::Call(eb!(5,9, Expr_::Path(Path::identifier(false, "test".into()))), vec![])) ]),
        eb!(21,30, Expr_::Call(eb!(21,26, Expr_::Path(Path::identifier(false, "count".into()))), vec![ enb!(27,29, Expr_::Variable("a".into())) ]))
    )));
}

#[test]
fn parse_stmt_for() {
    assert_eq!(process_stmt("for ($i = 0; $i < 10; $i++) { echo 1; }"), st!(0,39, Stmt_::For(
        vec![enb!(5,11, Expr_::Assign(eb!(5,7, Expr_::Variable("i".into())), eb!(10,11, Expr_::Int(0))))],
        vec![enb!(13,20, Expr_::BinaryOp(Op::Lt, eb!(13,15, Expr_::Variable("i".into())), eb!(18,20, Expr_::Int(10))))],
        vec![enb!(22,26, Expr_::UnaryOp(UnaryOp::PostInc, eb!(22,24, Expr_::Variable("i".into()))))],
        Block(vec![ st!(30,37, Stmt_::Echo(vec![ enb!(35,36, Expr_::Int(1)) ])) ]),
    )));
}

#[test]
fn parse_stmt_for_exprs() {
    assert_eq!(process_stmt("for ($i = 0, $c=4;;)        { echo 1; }"), st!(0,39, Stmt_::For(
        vec![
            enb!(5,11, Expr_::Assign(eb!(5,7, Expr_::Variable("i".into())), eb!(10,11, Expr_::Int(0)))),
            enb!(13,17, Expr_::Assign(eb!(13,15, Expr_::Variable("c".into())), eb!(16,17, Expr_::Int(4)))),
        ],
        vec![], vec![],
        Block(vec![ st!(30,37, Stmt_::Echo(vec![ enb!(35,36, Expr_::Int(1)) ])) ]),
    )));
}

#[test]
fn parse_stmt_foreach() {
    assert_eq!(process_stmt("foreach ($test as $v) { ok(); }"), st!(0,31, Stmt_::ForEach(
        eb!(9,14, Expr_::Variable("test".into())),
        None, // key
        eb!(18,20, Expr_::Variable("v".into())), // value
        Block(vec![ senb!(24,28, Expr_::Call(eb!(24,26, Expr_::Path(Path::identifier(false, "ok".into()))), vec![])) ]) //body
    )));
    assert_eq!(process_stmt("foreach ($test as $k => $v) { ok(); }"), st!(0,37, Stmt_::ForEach(
        eb!(9,14, Expr_::Variable("test".into())),
        Some(eb!(18,20, Expr_::Variable("k".into()))), // key
        eb!(24,26, Expr_::Variable("v".into())), // value
        Block(vec![ senb!(30,34, Expr_::Call(eb!(30,32, Expr_::Path(Path::identifier(false, "ok".into()))), vec![])) ]) //body
    )));
}

#[test]
fn parse_stmt_instanceof() {
    assert_eq!(process_stmt("if ($result instanceof Response) { return $result; }"), st!(0,52, Stmt_::If(
        eb!(4,31, Expr_::InstanceOf(eb!(4,11, Expr_::Variable("result".into())), eb!(23,31,Expr_::Path(Path::identifier(false, "Response".into()))))),
        Block(vec![ st!(35,50, Stmt_::Return(Some(eb!(42,49,Expr_::Variable("result".into()))))) ]),
        Block::empty(),
    )));
}

#[test]
fn parse_stmt_new_as_param() {
    assert_eq!(process_stmt("r(new Foo);"), senb!(0,10, Expr_::Call(eb!(0,1, Expr_::Path(Path::identifier(false, "r".into()))), vec![
        enb!(2,9, Expr_::New(eb!(6,9, Expr_::Path(Path::identifier(false, "Foo".into()))), vec![]))
    ])));
}

#[test]
fn parse_stmt_try() {
    assert_eq!(process_stmt(r#"try { echo "ok"; } catch (Exception $e) { return false;}"#), st!(0, 56, Stmt_::Try(
        Block(vec![ st!(6,16, Stmt_::Echo(vec![ enb!(11,15, Expr_::String("ok".into())) ])) ]),
        vec![ CatchClause { ty: Path::identifier(false, "Exception".into()), var: "e".into(),
            block: Block(vec![ st!(42,55, Stmt_::Return(Some(eb!(49,54, constant!(false))))) ]),
        } ],
        None,
    )));
    assert_eq!(process_stmt(r#"try { echo "ok"; } catch (Exception $e) { return false; } catch (Throwable $e) { return true; }"#), st!(0,95, Stmt_::Try(
        Block(vec![ st!(6,16, Stmt_::Echo(vec![ enb!(11,15, Expr_::String("ok".into())) ])) ]),
        vec![
            CatchClause { ty: Path::identifier(false, "Exception".into()), var: "e".into(), block: Block(vec![ st!(42,55, Stmt_::Return(Some(eb!(49,54, constant!(false))))) ]) },
            CatchClause { ty: Path::identifier(false, "Throwable".into()), var: "e".into(), block: Block(vec![ st!(81,93, Stmt_::Return(Some(eb!(88,92, constant!(true))))) ]) },
        ],
        None,
    )));
}

#[test]
fn parse_stmt_use() {
    assert_eq!(process_stmt("use Test;"), st!(0,9, Stmt_::Use(vec![ UseClause::QualifiedName(Path::identifier(false, "Test".into()), None) ])));
    assert_eq!(process_stmt(r#"use Ab\Cd\Ef\Gh\Ij as Ga;"#), st!(0, 25, Stmt_::Use(vec![UseClause::QualifiedName(
        Path::ns_identifier(false, "Ab\\Cd\\Ef\\Gh".into(), "Ij".into()),
        Some("Ga".into()))
    ])));
    assert_eq!(process_stmt(r#"use \FQNS\Test;"#), st!(0,15, Stmt_::Use(vec![ UseClause::QualifiedName(Path::ns_identifier(true, "FQNS".into(), "Test".into()), None) ])));
}

#[test]
fn parse_stmt_switch() {
    assert_eq!(process_stmt(r#"switch ($test) { case 1: echo "1"; break; default: echo "2"; }"#), st!(0,62, Stmt_::Switch(
        eb!(8,13, Expr_::Variable("test".into())), vec![
            SwitchCase { default: false, conds: vec![ enb!(22,23, Expr_::Int(1)) ], block: Block(vec![
                st!(25,34, Stmt_::Echo(vec![ enb!(30,33, Expr_::String("1".into())) ])), st!(35,41, Stmt_::Break(None))
            ])},
            SwitchCase { default: true, conds: vec![], block: Block(vec![ st!(51,60, Stmt_::Echo(vec![ enb!(56,59, Expr_::String("2".into())) ])) ]) },
        ]
    )));
    assert_eq!(process_stmt(r#"switch ($test) { case 1: echo "1"; default: echo "2"; }"#), st!(0,55, Stmt_::Switch(eb!(8,13, Expr_::Variable("test".into())),
        vec![
        SwitchCase { default: false, conds: vec![ enb!(22,23, Expr_::Int(1)) ], block: Block(vec![
            st!(25,34, Stmt_::Echo(vec![ enb!(30,33, Expr_::String("1".into())) ]))
        ]) },
        SwitchCase { default: true, conds: vec![], block: Block(vec![ st!(44,53, Stmt_::Echo(vec![ enb!(49,52, Expr_::String("2".into())) ])) ]) }
    ])));
    assert_eq!(process_stmt("switch ($test) { case 1: case 2: echo 1; }"), st!(0,42, Stmt_::Switch(eb!(8,13, Expr_::Variable("test".into())), vec![
        SwitchCase { default: false, conds: vec![ enb!(22,23, Expr_::Int(1)), enb!(30,31, Expr_::Int(2)) ], block: Block(vec![
            st!(33,40, Stmt_::Echo(vec![ enb!(38,39, Expr_::Int(1)) ]))
        ])}
    ])));
    assert_eq!(process_stmt("switch ($test) { case 1: case 2: case 3: case 4: echo 1; }"), st!(0,58, Stmt_::Switch(eb!(8,13, Expr_::Variable("test".into())), vec![
        SwitchCase { default: false,
            conds: vec![ enb!(22,23, Expr_::Int(1)), enb!(30,31, Expr_::Int(2)), enb!(38,39, Expr_::Int(3)), enb!(46,47, Expr_::Int(4)) ],
            block: Block(vec![ st!(49,56, Stmt_::Echo(vec![ enb!(54,55, Expr_::Int(1)) ]))])
        }
    ])));
}

#[test]
fn parse_stmt_goto() {
    assert_eq!(process_stmt("goto hallo_welt;"), st!(0, 16, Stmt_::Goto("hallo_welt".into())));
}

#[test]
fn parse_stmt_func_decl() {
    assert_eq!(process_stmt("function test() { ok(); }"), st!(0,25, Stmt_::Decl(Decl::GlobalFunction("test".into(), FunctionDecl { params: vec![],
        body: Some(Block(vec![ senb!(18,22, Expr_::Call(eb!(18,20, Expr_::Path(Path::identifier(false, "ok".into()))), vec![])) ])), usev: vec![], ret_ref: false, ret_ty: None })
    )));
    assert_eq!(process_stmt("function &test() { ok(); }"), st!(0,26, Stmt_::Decl(Decl::GlobalFunction("test".into(), FunctionDecl { params: vec![],
        body: Some(Block(vec![ senb!(19,23, Expr_::Call(eb!(19,21, Expr_::Path(Path::identifier(false, "ok".into()))), vec![])) ])), usev: vec![], ret_ref: true, ret_ty: None })
    )));
    assert_eq!(process_stmt("function test($a) { ok(); }"), st!(0,27, Stmt_::Decl(Decl::GlobalFunction("test".into(), FunctionDecl {
        params: vec![ParamDefinition { name: "a".into(), as_ref: false, variadic: false, ty: None, default: None }],
        body: Some(Block(vec![ senb!(20,24, Expr_::Call(eb!(20,22, Expr_::Path(Path::identifier(false, "ok".into()))), vec![])) ])), usev: vec![], ret_ref: false, ret_ty: None })
    )));
    assert_eq!(process_stmt("function test($a, $b) { ok(); }"), st!(0,31, Stmt_::Decl(Decl::GlobalFunction("test".into(), FunctionDecl {
        params: vec![
            ParamDefinition { name: "a".into(), as_ref: false, variadic: false, ty: None, default: None },
            ParamDefinition { name: "b".into(), as_ref: false, variadic: false, ty: None, default: None }
        ],
        body: Some(Block(vec![ senb!(24,28, Expr_::Call(eb!(24,26, Expr_::Path(Path::identifier(false, "ok".into()))), vec![])) ])), usev: vec![], ret_ref: false, ret_ty: None })
    )));
    assert_eq!(process_stmt("function test(...$a) { ok(); }"), st!(0,30, Stmt_::Decl(Decl::GlobalFunction("test".into(), FunctionDecl {
        params: vec![ParamDefinition { name: "a".into(), as_ref: false, variadic: true, ty: None, default: None }],
        body: Some(Block(vec![ senb!(23,27, Expr_::Call(eb!(23,25, Expr_::Path(Path::identifier(false, "ok".into()))), vec![])) ])), usev: vec![], ret_ref: false, ret_ty: None })
    )));
}

#[test]
fn parse_stmt_func_decl_ret() {
    assert_eq!(process_stmt("function test() : bool { ok(); }"), st!(0,32, Stmt_::Decl(Decl::GlobalFunction("test".into(), FunctionDecl { params: vec![],
        body: Some(Block(vec![ senb!(25,29, Expr_::Call(eb!(25,27, Expr_::Path(Path::identifier(false, "ok".into()))), vec![])) ])), usev: vec![], ret_ref: false,
            ret_ty: Some(NullableTy::NonNullable(Ty::Bool)) })
    )));
    assert_eq!(process_stmt(r#"function test() : \foo\bar { ok(); }"#), st!(0,36, Stmt_::Decl(Decl::GlobalFunction("test".into(), FunctionDecl { params: vec![],
        body: Some(Block(vec![ senb!(29,33, Expr_::Call(eb!(29,31, Expr_::Path(Path::identifier(false, "ok".into()))), vec![])) ])), usev: vec![], ret_ref: false,
            ret_ty: Some(NullableTy::NonNullable(Ty::Object(Some(Path::ns_identifier(true, "foo".into(), "bar".into()))))) })
    )));
    assert_eq!(process_stmt(r#"function test() : ?FoooBar { ok(); }"#), st!(0,36, Stmt_::Decl(Decl::GlobalFunction("test".into(), FunctionDecl { params: vec![],
        body: Some(Block(vec![ senb!(29,33, Expr_::Call(eb!(29,31, Expr_::Path(Path::identifier(false, "ok".into()))), vec![])) ])), usev: vec![], ret_ref: false,
            ret_ty: Some(NullableTy::Nullable(Ty::Object(Some(Path::identifier(false, "FoooBar".into()))))) })
    )));
}

#[test]
fn parse_func_decl_typehint() {
    assert_eq!(process_stmt("function test(Test $a) { ok(); }"), st!(0,32, Stmt_::Decl(Decl::GlobalFunction("test".into(), FunctionDecl {
        params: vec![ ParamDefinition { name: "a".into(), as_ref: false, variadic: false,
            ty: Some(NullableTy::NonNullable(Ty::Object(Some(Path::identifier(false, "Test".into()))))), default: None }
        ],
        body: Some(Block(vec![ senb!(25,29, Expr_::Call(eb!(25,27, Expr_::Path(Path::identifier(false, "ok".into()))), vec![])) ])), usev: vec![], ret_ref: false, ret_ty: None })
    )));
}

#[test]
fn parse_class_decl() {
    assert_eq!(process_stmt("class Test {}"), st!(0,13, Stmt_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Test".into(), base_class: None, implements: vec![], members: vec![]
    }))));
    assert_eq!(process_stmt("final class Test {}"), st!(0,19, Stmt_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::new(&[ClassModifier::Final]), name: "Test".into(), base_class: None, implements: vec![], members: vec![]
    }))));
    assert_eq!(process_stmt("class Test extends Abc\\Test2 {}"), st!(0,31, Stmt_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Test".into(), base_class: Some(Path::ns_identifier(false, "Abc".into(), "Test2".into())), implements: vec![], members: vec![]
    }))));
    assert_eq!(process_stmt("class Test implements ITest {}"), st!(0,30, Stmt_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Test".into(), base_class: None, implements: vec![Path::identifier(false, "ITest".into())], members: vec![]
    }))));
}

#[test]
fn parse_class_properties() {
    assert_eq!(process_stmt("class Test { public $test; }"), st!(0,28, Stmt_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Test".into(), base_class: None, implements: vec![],
        members: vec![ Member::Property(MemberModifiers::new(&[MemberModifier::Public]), "test".into(), None)],
    }))));
    assert_eq!(process_stmt("class Test { protected $ab = []; }"), st!(0,34, Stmt_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Test".into(), base_class: None, implements: vec![],
        members: vec![ Member::Property(MemberModifiers::new(&[MemberModifier::Protected]), "ab".into(), Some(enb!(29,31, Expr_::Array(vec![])))) ],
    }))));
}

#[test]
fn parse_class_const() {
    assert_eq!(process_stmt("class Test { const C=true; }"), st!(0, 28, Stmt_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Test".into(), base_class: None, implements: vec![],
        members: vec![ Member::Constant(MemberModifiers::none(), "C".into(), enb!(21,25, constant!(true))) ]
    }))));
}

#[test]
fn parse_class_methods() {
    assert_eq!(process_stmt("class Test { public function a() { run(); } }"), st!(0,45, Stmt_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Test".into(), base_class: None, implements: vec![],
        members: vec![ Member::Method(MemberModifiers::new(&[MemberModifier::Public]), "a".into(), FunctionDecl {
            params: vec![], body: Some(Block(vec![ senb!(35,40, Expr_::Call(eb!(35,38, Expr_::Path(Path::identifier(false, "run".into()))), vec![])) ])),
            usev: vec![], ret_ref: false, ret_ty: None,
        })]
    }))));
    assert_eq!(process_stmt("class Test { public function __construct(array $param1 = []) { $this->param = $param1; } }"),
        st!(0,90, Stmt_::Decl(Decl::Class(ClassDecl {
            cmod: ClassModifiers::none(), name: "Test".into(), base_class: None, implements: vec![],
            members: vec![ Member::Method(MemberModifiers::new(&[MemberModifier::Public]), "__construct".into(), FunctionDecl {
                params: vec![ParamDefinition { name: "param1".into(), as_ref: false, variadic: false, ty: Some(NullableTy::NonNullable(Ty::Array)),
                    default: Some(enb!(57,59, Expr_::Array(vec![]))) }
                ],
                body: Some(Block(vec![ senb!(63,85, Expr_::Assign(eb!(63,75, Expr_::ObjMember(eb!(63,68, Expr_::Variable("this".into())), vec![
                    enb!(70,75, Expr_::Path(Path::identifier(false, "param".into()))) ])), eb!(78,85, Expr_::Variable("param1".into()))))
                ])), usev: vec![], ret_ref: false, ret_ty: None,
            })]
        })))
    );
}

#[test]
fn parse_class_trait_use() {
    assert_eq!(process_stmt("class Test { use Abc; }"), st!(0,23, Stmt_::Decl(Decl::Class(ClassDecl { name: "Test".into(), base_class: None, implements: vec![], members: vec![
        Member::TraitUse(vec![Path::identifier(false, "Abc".into())], vec![])
    ], cmod: ClassModifiers::none() }))));
}

#[test]
fn parse_stmt_list() {
    assert_eq!(process_stmt("list($a, $b) = test();"), senb!(0,21, Expr_::Assign(eb!(0,12, Expr_::List(
        vec![ (None, enb!(5,7, Expr_::Variable("a".into()))), (None, enb!(9,11, Expr_::Variable("b".into()))) ]
    )), eb!(15,21, Expr_::Call(eb!(15,19, Expr_::Path(Path::identifier(false, "test".into()))), vec![])))));
}

#[test]
fn parse_trait_decl() {
    assert_eq!(process_stmt("trait Test {}"), st!(0,13, Stmt_::Decl(Decl::Trait("Test".into(), vec![]))));
    // http://php.net/manual/de/language.oop5.traits.php
    assert_eq!(process_stmt("trait HelloWorld {use Hello, World;}"), st!(0,36, Stmt_::Decl(Decl::Trait("HelloWorld".into(), vec![
        Member::TraitUse(vec![Path::identifier(false, "Hello".into()), Path::identifier(false, "World".into())], vec![])
    ]))));
}

#[test]
fn parse_interface_decl() {
    assert_eq!(process_stmt("interface ITest {}"), st!(0,18, Stmt_::Decl(Decl::Interface("ITest".into(), vec![], vec![]))));
    assert_eq!(process_stmt("interface ITest { public function test(); }"), st!(0,43, Stmt_::Decl(
        Decl::Interface("ITest".into(), vec![], vec![ Member::Method(MemberModifiers::new(&[MemberModifier::Public]),
            "test".into(), FunctionDecl {params: vec![], body: None, usev: vec![], ret_ref: false, ret_ty: None})
        ])
    )));
}

#[test]
fn parse_class_use_trait_complex() {
    // http://php.net/manual/de/language.oop5.traits.php
    let code = "class Aliased_Talker {
        use A, B {
            B::smallTalk insteadof A;
            A::bigTalk insteadof B;
            B::bigTalk as talk;
        }
    }";
    assert_eq!(process_stmt(code), st!(0,163, Stmt_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Aliased_Talker".into(), base_class: None, implements: vec![], members: vec![
            Member::TraitUse(vec![Path::identifier(false, "A".into()), Path::identifier(false, "B".into())], vec![
                TraitUse::InsteadOf(Path::identifier(false, "B".into()), "smallTalk".into(), vec![Path::identifier(false, "A".into())]),
                TraitUse::InsteadOf(Path::identifier(false, "A".into()), "bigTalk".into(), vec![Path::identifier(false, "B".into())]),
                TraitUse::As(Some(Path::identifier(false, "B".into())), "bigTalk".into(), MemberModifiers::none(), Some("talk".into())),
            ])
        ]
    }))));
}

#[test]
fn parse_static_decl() {
    assert_eq!(process_stmt("static $t=true;"), st!(0,15, Stmt_::Decl(Decl::StaticVars(vec![ ("t".into(), Some(enb!(10,14, constant!(true)))) ]))));
}

#[test]
fn parse_global_decl() {
    assert_eq!(process_stmt("global $t;"), st!(0,10, Stmt_::Decl(Decl::GlobalVars(vec![ "t".into() ]))));
}

#[test]
fn parse_stmt_closure_use() {
    assert_eq!(process_stmt("return function () use ($t) {};"), st!(0,31, Stmt_::Return(Some(eb!(7,30, Expr_::Function(FunctionDecl {
        params: vec![], body: Some(Block(vec![])), usev: vec![(false, "t".into())], ret_ref: false, ret_ty: None,
    }))))));
}

#[test]
fn parse_namespace_decl() {
    assert_eq!(process_stmt("namespace Foo\\Bar;"), st!(0,17, Stmt_::Decl(Decl::Namespace(Path::ns_identifier(false, "Foo".into(), "Bar".into())))));
}

#[test]
fn parse_label_decl() {
    assert_eq!(process_stmt("test_abc:"), st!(0, 9, Stmt_::Decl(Decl::Label("test_abc".into()))));
}
