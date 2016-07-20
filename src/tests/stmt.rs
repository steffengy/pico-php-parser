use parser::*;

fn process_stmt(input: &str) -> Expr {
    let str_ = "<?php ".to_owned() + input;
    let mut result = Parser::parse_str(&str_).unwrap();
    assert_eq!(result.len(), 1);
    result.pop().unwrap()
}

/*#[test]
fn parse_stmt_heredoc() {
    assert_eq!(process_stmt("<<<EOT\ntest\nEOT;\n$g;; }"), enb!(None, Expr_::String("".into())));
}*/

#[test]
fn parse_stmt_echo() {
    assert_eq!(process_stmt("echo 1;"), enb!(0,4, Expr_::Echo(vec![ enb!(5,6, Expr_::Int(1)) ])));
}

#[test]
fn parse_stmt_return() {
    assert_eq!(process_stmt("return true;"), enb!(0,6, Expr_::Return(Some(eb!(7,11, constant!(true))))));
    assert_eq!(process_stmt("return;"), enb!(0,6, Expr_::Return(None)));
}

#[test]
fn parse_stmt_continue() {
    assert_eq!(process_stmt("continue;"), enb!(0,8, Expr_::Continue(None)));
    assert_eq!(process_stmt("continue 2;"), enb!(0,8, Expr_::Continue(Some(eb!(9,10, Expr_::Int(2))))));
}

#[test]
fn parse_stmt_break() {
    assert_eq!(process_stmt("break;"), enb!(0,5, Expr_::Break(None)));
    assert_eq!(process_stmt("break 2;"), enb!(0,5, Expr_::Break(Some(eb!(6,7, Expr_::Int(2))))));
}

#[test]
fn parse_stmt_unset() {
    assert_eq!(process_stmt("unset($a);"), enb!(0,9, Expr_::Unset(vec![ enb!(6,8, Expr_::Variable("a".into())) ])));
}

#[test]
fn parse_stmt_new() {
    /*assert_eq!(process_stmt("return new $var($this);"), enb!(0,6, Expr_::Return(Some(eb!(7,10, Expr_::New(eb!(11,15, Expr_::Variable("var".into())),
        vec![ enb!(16,21, Expr_::Variable("this".into())) ]))
    ))));*/
}

#[test]
fn parse_stmt_throw() {
    assert_eq!(process_stmt(r#"throw new Exception("test");"#), enb!(0,5, Expr_::Throw(eb!(6,9, Expr_::New(eb!(10,19, Expr_::Path(Path::Identifier("Exception".into()))),
        vec![ enb!(20,26, Expr_::String("test".into())) ])))
    ));
    assert_eq!(process_stmt(r#"throw new Exception;"#), enb!(0,5, Expr_::Throw(eb!(6,9, Expr_::New(eb!(10,19, Expr_::Path(Path::Identifier("Exception".into()))), vec![])))));
}

#[test]
fn parse_stmt_include() {
    assert_eq!(process_stmt("include $test;"), enb!(0,7, Expr_::Include(IncludeTy::Include, eb!(8,13, Expr_::Variable("test".into())))));
    assert_eq!(process_stmt("include_once($test);"), enb!(0,12, Expr_::Include(IncludeTy::IncludeOnce, eb!(13,18, Expr_::Variable("test".into())))));
    assert_eq!(process_stmt("require($test);"), enb!(0,7, Expr_::Include(IncludeTy::Require, eb!(8,13, Expr_::Variable("test".into())))));
    assert_eq!(process_stmt("require_once $test;"), enb!(0,12, Expr_::Include(IncludeTy::RequireOnce, eb!(13,18, Expr_::Variable("test".into())))));
}

#[test]
fn parse_stmt_exit() {
    assert_eq!(process_stmt("exit;"), enb!(0,4, Expr_::Exit(None)));
    // TODO: Test that this fails (as it should): assert_eq!(process_stmt("exit true;"), Expr_::Exit(Box::new(Expr_::True)));
    assert_eq!(process_stmt("exit(true);"), enb!(0,4, Expr_::Exit(Some(eb!(5,9, constant!(true))))));
}

#[test]
fn parse_stmt_assignment() {
    assert_eq!(process_stmt(r#"$test=4;"#), enb!(5,6, Expr_::Assign(eb!(0,5, Expr_::Variable("test".into())), eb!(6,7, Expr_::Int(4)))));
    assert_eq!(process_stmt(r#"$test["a"]=4+$b;"#), enb!(10,11, Expr_::Assign(eb!(None,
        Expr_::ArrayIdx(
            eb!(0,5, Expr_::Variable("test".into())),
            vec![ enb!(6,9, Expr_::String("a".into())) ]
        )),
        eb!(12,13, Expr_::BinaryOp(Op::Add, eb!(11, 12, Expr_::Int(4)), eb!(13,15, Expr_::Variable("b".into()))))
    )));
}

#[test]
fn parse_stmt_return_ternary_assign() {
    assert_eq!(process_stmt("return isset($a) ? $b : $c = $d;"), enb!(0,6, Expr_::Return(Some(eb!(None, Expr_::TernaryIf(
        eb!(7,12, Expr_::Isset(vec![ enb!(13,15, Expr_::Variable("a".into())) ])), Some(eb!(19,21, Expr_::Variable("b".into()))),
        eb!(27,28, Expr_::Assign(eb!(24,26, Expr_::Variable("c".into())), eb!(29,31, Expr_::Variable("d".into()))))
    ))))));
}

#[test]
fn parse_stmt_compound_assignment() {
    assert_eq!(process_stmt("$test+=4;"), enb!(5,7, Expr_::CompoundAssign(eb!(0,5, Expr_::Variable("test".into())), Op::Add, eb!(7,8, Expr_::Int(4)))));
}

#[test]
fn parse_stmt_if_assign() {
    assert_eq!(process_stmt("if (! $a && $b = $c) { echo 1; }"), enb!(0,32, Expr_::If(eb!(9,11, Expr_::BinaryOp(Op::And,
        eb!(4,5, Expr_::UnaryOp(UnaryOp::Not, eb!(6,8, Expr_::Variable("a".into())))),
        eb!(15,16, Expr_::Assign(eb!(12,14, Expr_::Variable("b".into())), eb!(17,19, Expr_::Variable("c".into())))))
    ), eb!(21, 32, Expr_::Block(Block(vec![ enb!(23,27, Expr_::Echo(vec![ enb!(28,29, Expr_::Int(1)) ])) ]))), None)));
}

#[test]
fn parse_stmt_if_while() {
    let get_results = |c: u32| {
        let block_stmt = enb!(None, Expr_::Call(eb!(13,14, Expr_::Path(Path::Identifier("b".into()))), vec![]));
        (
            enb!(7,9, Expr_::Variable("a".into())),
            block_stmt.clone(),
            enb!(11,19-c, Expr_::Block(Block(vec![block_stmt])))
    )};
    let (expr_var, block_stmt, block_expr) = get_results(0);
    let args = [
        ("if   ", enb!(0,19, Expr_::If(Box::new(expr_var.clone()), Box::new(block_expr.clone()), None))),
        ("while", enb!(0,19, Expr_::While(Box::new(expr_var.clone()), Box::new(block_expr))))
    ];
    for arg in &args {
        assert_eq!(process_stmt(&(arg.0.to_owned() + r#" ($a) { b(); }"#)), arg.1);
    }
    let (expr_var, block_stmt, block_expr) = get_results(5);
    let args = [
        // TODO: fix line number (14 insteadof None)
        ("if   ", enb!(0, None, Expr_::If(Box::new(expr_var.clone()), Box::new(block_stmt.clone()), None))),
        ("while", enb!(0, None, Expr_::While(Box::new(expr_var.clone()), Box::new(block_stmt))))
    ];
    for arg in &args {
        assert_eq!(process_stmt(&(arg.0.to_owned() + r#" ($a)   b();  "#)), arg.1);
    }
}

#[test]
fn parse_stmt_if_else() {
    let make_result = |a:u32,b, cond_expr, block_expr, else_expr| eb!(a,b, Expr_::If(cond_expr, block_expr, Some(else_expr)));
    let cond_expr = |a,b| eb!(a,b, Expr_::Variable("a".into()));
    let main_call_expr = |c:u32,d| eb!(None, Expr_::Call(eb!(c,d, Expr_::Path(Path::Identifier("a".into()))), vec![]));
    let block_expr = |a:u32,b:u32,c:u32,d:u32| eb!(a,b, Expr_::Block(Block(vec![*main_call_expr(c,d)])));
    let call_expr = |c:u32,d:u32| eb!(None, Expr_::Call(eb!(c,d, Expr_::Path(Path::Identifier("b".into()))), vec![]));
    let else_expr = |a:u32,b:u32,c:u32,d:u32| eb!(a,b, Expr_::Block(Block(vec![*call_expr(c, d)])));

    //TODO: fix line numbers (`if { } else { }` has the length of the end_position of the 2. } insteadof the 1. }
    assert_eq!(process_stmt("if ($a) { a(); } else { b(); }"), *make_result(0,16/*30*/, cond_expr(4,6), block_expr(8,16, 10,11), else_expr(22,30, 24,25)));
    assert_eq!(process_stmt("if ($a) a(); else b();"), *make_result(0,-6i32 /* this should be 22, @todo */, cond_expr(4,6), main_call_expr(8,9), call_expr(18,19)));
    //if, elseif, else
    let result2 = |c:u32| *make_result(0,-6i32/*40*/, cond_expr(4,6), main_call_expr(8, 9), make_result(18-c,-6i32, eb!(22,24, Expr_::Variable("b".into())),
        call_expr(26, 27),
        eb!(None, Expr_::Call(eb!(36,37, Expr_::Path(Path::Identifier("c".into()))), vec![]))
    ));
    assert_eq!(process_stmt("if ($a) a(); else if ($b) b(); else c();"), result2(0));
    assert_eq!(process_stmt("if ($a) a(); elseif  ($b) b(); else c();"), result2(5));
}

#[test]
fn parse_stmt_do_while() {
    assert_eq!(process_stmt("do { test(); } while(count($a));"), enb!(0,32, Expr_::DoWhile(
        eb!(3,14, Expr_::Block(Block(vec![ enb!(None, Expr_::Call(eb!(5,9, Expr_::Path(Path::Identifier("test".into()))), vec![])) ]))),
        eb!(None, Expr_::Call(eb!(21,26, Expr_::Path(Path::Identifier("count".into()))), vec![ enb!(27,29, Expr_::Variable("a".into())) ]))
    )));
}

#[test]
fn parse_stmt_for() {
    assert_eq!(process_stmt("for ($i = 0; $i < 10; $i++) { echo 1; }"), enb!(0,39, Expr_::For(
        Some(eb!(8,9, Expr_::Assign(eb!(5,7, Expr_::Variable("i".into())), eb!(10,11, Expr_::Int(0))))),
        Some(eb!(16,17, Expr_::BinaryOp(Op::Lt, eb!(13,15, Expr_::Variable("i".into())), eb!(18,20, Expr_::Int(10))))),
        Some(eb!(24,26, Expr_::UnaryOp(UnaryOp::PostInc, eb!(22,24, Expr_::Variable("i".into()))))),
        eb!(28,39, Expr_::Block(Block(vec![ enb!(30,34, Expr_::Echo(vec![ enb!(35,36, Expr_::Int(1)) ])) ])) ),
    )));
}

#[test]
fn parse_stmt_foreach() {
    assert_eq!(process_stmt("foreach ($test as $v) { ok(); }"), enb!(0,31, Expr_::ForEach(
        eb!(9,14, Expr_::Variable("test".into())),
        None, // key
        eb!(18,20, Expr_::Variable("v".into())), // value
        eb!(22,31, Expr_::Block(Block(vec![ enb!(None, Expr_::Call(eb!(24,26, Expr_::Path(Path::Identifier("ok".into()))), vec![])) ]))) //body
    )));
    assert_eq!(process_stmt("foreach ($test as $k => $v) { ok(); }"), enb!(0,37, Expr_::ForEach(
        eb!(9,14, Expr_::Variable("test".into())),
        Some(eb!(18,20, Expr_::Variable("k".into()))), // key
        eb!(24,26, Expr_::Variable("v".into())), // value
        eb!(28,37, Expr_::Block(Block(vec![ enb!(None, Expr_::Call(eb!(30,32, Expr_::Path(Path::Identifier("ok".into()))), vec![])) ]))) //body
    )));
}

#[test]
fn parse_stmt_instanceof() {
    assert_eq!(process_stmt("if ($result instanceof Response) { return $result; }"), enb!(0,52, Expr_::If(
        eb!(12,22, Expr_::InstanceOf(eb!(4,11, Expr_::Variable("result".into())), eb!(23,31,Expr_::Path(Path::Identifier("Response".into()))))),
        eb!(33,52, Expr_::Block(Block(vec![ enb!(35,41, Expr_::Return(Some(eb!(42,49,Expr_::Variable("result".into()))))) ]))), None,
    )));
}

#[test]
fn parse_stmt_new_as_param() {
    assert_eq!(process_stmt("r(new Foo);"), enb!(None, Expr_::Call(eb!(0,1, Expr_::Path(Path::Identifier("r".into()))), vec![
        enb!(2,5, Expr_::New(eb!(6,9, Expr_::Path(Path::Identifier("Foo".into()))), vec![]))
    ])));
}


#[test]
fn parse_stmt_try() {
    assert_eq!(process_stmt(r#"try { echo "ok"; } catch (Exception $e) { return false;}"#), enb!(0, 56, Expr_::Try(
        Block(vec![ enb!(6,10, Expr_::Echo(vec![ enb!(11,15, Expr_::String("ok".into())) ])) ]),
        vec![ CatchClause { ty: Path::Identifier("Exception".into()), var: "e".into(),
            block: Block(vec![ enb!(42,48, Expr_::Return(Some(eb!(49,54, constant!(false))))) ]),
        } ],
        None,
    )));
    assert_eq!(process_stmt(r#"try { echo "ok"; } catch (Exception $e) { return false; } catch (Throwable $e) { return true; }"#), enb!(0,95, Expr_::Try(
        Block(vec![ enb!(6,10, Expr_::Echo(vec![ enb!(11,15, Expr_::String("ok".into())) ])) ]),
        vec![
            CatchClause { ty: Path::Identifier("Exception".into()), var: "e".into(), block: Block(vec![ enb!(42,48, Expr_::Return(Some(eb!(49,54, constant!(false))))) ]) },
            CatchClause { ty: Path::Identifier("Throwable".into()), var: "e".into(), block: Block(vec![ enb!(81,87, Expr_::Return(Some(eb!(88,92, constant!(true))))) ]) },
        ],
        None,
    )));
}

#[test]
fn parse_stmt_use() {
    assert_eq!(process_stmt("use Test;"), enb!(0,9, Expr_::Use(vec![ UseClause::QualifiedName(Path::Identifier("Test".into()), None) ])));
    assert_eq!(process_stmt(r#"use Ab\Cd\Ef\Gh\Ij as Ga;"#), enb!(0, 25, Expr_::Use(vec![UseClause::QualifiedName(
        Path::NsIdentifier("Ab\\Cd\\Ef\\Gh".into(), "Ij".into()),
        Some("Ga".into()))
    ])));
}

#[test]
fn parse_stmt_switch() {
    assert_eq!(process_stmt(r#"switch ($test) { case 1: echo "1"; break; default: echo "2"; }"#), enb!(0,62, Expr_::Switch(
        eb!(8,13, Expr_::Variable("test".into())), vec![
            SwitchCase { default: false, conds: vec![ enb!(22,23, Expr_::Int(1)) ], block: Block(vec![
                enb!(25,29, Expr_::Echo(vec![ enb!(30,33, Expr_::String("1".into())) ])), enb!(35,40, Expr_::Break(None))
            ])},
            SwitchCase { default: true, conds: vec![], block: Block(vec![ enb!(51,55, Expr_::Echo(vec![ enb!(56,59, Expr_::String("2".into())) ])) ]) },
        ]
    )));
    assert_eq!(process_stmt(r#"switch ($test) { case 1: echo "1"; default: echo "2"; }"#), enb!(0,55, Expr_::Switch(eb!(8,13, Expr_::Variable("test".into())),
        vec![
        SwitchCase { default: false, conds: vec![ enb!(22,23, Expr_::Int(1)) ], block: Block(vec![
            enb!(25,29, Expr_::Echo(vec![ enb!(30,33, Expr_::String("1".into())) ]))
        ]) },
        SwitchCase { default: true, conds: vec![], block: Block(vec![ enb!(44,48, Expr_::Echo(vec![ enb!(49,52, Expr_::String("2".into())) ])) ]) }
    ])));
    assert_eq!(process_stmt("switch ($test) { case 1: case 2: echo 1; }"), enb!(0,42, Expr_::Switch(eb!(8,13, Expr_::Variable("test".into())), vec![
        SwitchCase { default: false, conds: vec![ enb!(22,23, Expr_::Int(1)), enb!(30,31, Expr_::Int(2)) ], block: Block(vec![
            enb!(33,37, Expr_::Echo(vec![ enb!(38,39, Expr_::Int(1)) ]))
        ])}
    ])));
    assert_eq!(process_stmt("switch ($test) { case 1: case 2: case 3: case 4: echo 1; }"), enb!(0,58, Expr_::Switch(eb!(8,13, Expr_::Variable("test".into())), vec![
        SwitchCase { default: false,
            conds: vec![ enb!(22,23, Expr_::Int(1)), enb!(30,31, Expr_::Int(2)), enb!(38,39, Expr_::Int(3)), enb!(46,47, Expr_::Int(4)) ],
            block: Block(vec![ enb!(49,53, Expr_::Echo(vec![ enb!(54,55, Expr_::Int(1)) ]))])
        }
    ])));
}

#[test]
fn parse_stmt_func_decl() {
    assert_eq!(process_stmt("function test() { ok(); }"), enb!(0,25, Expr_::Decl(Decl::GlobalFunction("test".into(), FunctionDecl { params: vec![],
        body: Block(vec![ enb!(None, Expr_::Call(eb!(18,20, Expr_::Path(Path::Identifier("ok".into()))), vec![])) ]), usev: vec![], ret_ref: false, })
    )));
    assert_eq!(process_stmt("function &test() { ok(); }"), enb!(0,26, Expr_::Decl(Decl::GlobalFunction("test".into(), FunctionDecl { params: vec![],
        body: Block(vec![ enb!(None, Expr_::Call(eb!(19,21, Expr_::Path(Path::Identifier("ok".into()))), vec![])) ]), usev: vec![], ret_ref: true, })
    )));
    assert_eq!(process_stmt("function test($a) { ok(); }"), enb!(0,27, Expr_::Decl(Decl::GlobalFunction("test".into(), FunctionDecl {
        params: vec![ParamDefinition { name: "a".into(), as_ref: false, ty: None, default: None }],
        body: Block(vec![ enb!(None, Expr_::Call(eb!(20,22, Expr_::Path(Path::Identifier("ok".into()))), vec![])) ]), usev: vec![], ret_ref: false, })
    )));
    assert_eq!(process_stmt("function test($a, $b) { ok(); }"), enb!(0,31, Expr_::Decl(Decl::GlobalFunction("test".into(), FunctionDecl {
        params: vec![ParamDefinition { name: "a".into(), as_ref: false, ty: None, default: None }, ParamDefinition { name: "b".into(), as_ref: false, ty: None, default: None }],
        body: Block(vec![ enb!(None, Expr_::Call(eb!(24,26, Expr_::Path(Path::Identifier("ok".into()))), vec![])) ]), usev: vec![], ret_ref: false, })
    )));
}

#[test]
fn parse_func_decl_typehint() {
    assert_eq!(process_stmt("function test(Test $a) { ok(); }"), enb!(0,32, Expr_::Decl(Decl::GlobalFunction("test".into(), FunctionDecl {
        params: vec![ ParamDefinition { name: "a".into(), as_ref: false, ty: Some(Ty::Object(Some(Path::Identifier("Test".into())))), default: None } ],
        body: Block(vec![ enb!(None, Expr_::Call(eb!(25,27, Expr_::Path(Path::Identifier("ok".into()))), vec![])) ]), usev: vec![], ret_ref: false, })
    )));
}

#[test]
fn parse_class_decl() {
    assert_eq!(process_stmt("class Test {}"), enb!(0,13, Expr_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Test".into(), base_class: None, implements: vec![], members: vec![]
    }))));
    assert_eq!(process_stmt("final class Test {}"), enb!(0,19, Expr_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::new(&[ClassModifier::Final]), name: "Test".into(), base_class: None, implements: vec![], members: vec![]
    }))));
    assert_eq!(process_stmt("class Test extends Abc\\Test2 {}"), enb!(0,31, Expr_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Test".into(), base_class: Some(Path::NsIdentifier("Abc".into(), "Test2".into())), implements: vec![], members: vec![]
    }))));
    assert_eq!(process_stmt("class Test implements ITest {}"), enb!(0,30, Expr_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Test".into(), base_class: None, implements: vec![Path::Identifier("ITest".into())], members: vec![]
    }))));
}

#[test]
fn parse_class_properties() {
    assert_eq!(process_stmt("class Test { public $test; }"), enb!(0,28, Expr_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Test".into(), base_class: None, implements: vec![],
        members: vec![ Member::Property(MemberModifiers::new(&[MemberModifier::Public]), "test".into(), None)],
    }))));
    assert_eq!(process_stmt("class Test { protected $ab = []; }"), enb!(0,34, Expr_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Test".into(), base_class: None, implements: vec![],
        members: vec![ Member::Property(MemberModifiers::new(&[MemberModifier::Protected]), "ab".into(), Some(enb!(29,31, Expr_::Array(vec![])))) ],
    }))));
}

#[test]
fn parse_class_const() {
    assert_eq!(process_stmt("class Test { const C=true; }"), enb!(0, 28, Expr_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Test".into(), base_class: None, implements: vec![],
        members: vec![ Member::Constant(MemberModifiers::none(), "C".into(), enb!(21,25, constant!(true))) ]
    }))));
}

#[test]
fn parse_class_methods() {
    assert_eq!(process_stmt("class Test { public function a() { run(); } }"), enb!(0,45, Expr_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Test".into(), base_class: None, implements: vec![],
        members: vec![ Member::Method(MemberModifiers::new(&[MemberModifier::Public]), "a".into(), FunctionDecl {
            params: vec![], body: Block(vec![ enb!(None, Expr_::Call(eb!(35,38, Expr_::Path(Path::Identifier("run".into()))), vec![])) ]),
            usev: vec![], ret_ref: false,
        })]
    }))));
    assert_eq!(process_stmt("class Test { public function __construct(array $param1 = []) { $this->param = $param1; } }"),
        enb!(0,90, Expr_::Decl(Decl::Class(ClassDecl {
            cmod: ClassModifiers::none(), name: "Test".into(), base_class: None, implements: vec![],
            members: vec![ Member::Method(MemberModifiers::new(&[MemberModifier::Public]), "__construct".into(), FunctionDecl {
                params: vec![ParamDefinition { name: "param1".into(), as_ref: false, ty: Some(Ty::Array), default: Some(enb!(57,59, Expr_::Array(vec![]))) }],
                body: Block(vec![ enb!(76,77, Expr_::Assign(eb!(68,70, Expr_::ObjMember(eb!(63,68, Expr_::Variable("this".into())), vec![
                    enb!(70,75, Expr_::Path(Path::Identifier("param".into()))) ])), eb!(78,85, Expr_::Variable("param1".into()))))
                ]), usev: vec![], ret_ref: false,
            })]
        })))
    );
}

#[test]
fn parse_class_trait_use() {
    assert_eq!(process_stmt("class Test { use Abc; }"), enb!(0,23, Expr_::Decl(Decl::Class(ClassDecl { name: "Test".into(), base_class: None, implements: vec![], members: vec![
        Member::TraitUse(vec![Path::Identifier("Abc".into())], vec![])
    ], cmod: ClassModifiers::none() }))));
}

#[test]
fn parse_stmt_list() {
    assert_eq!(process_stmt("list($a, $b) = test();"), enb!(13,14, Expr_::Assign(eb!(0,12, Expr_::List(
        vec![ (None, enb!(5,7, Expr_::Variable("a".into()))), (None, enb!(9,11, Expr_::Variable("b".into()))) ]
    )), eb!(None, Expr_::Call(eb!(15,19, Expr_::Path(Path::Identifier("test".into()))), vec![])))));
}

#[test]
fn parse_trait_decl() {
    assert_eq!(process_stmt("trait Test {}"), enb!(0,13, Expr_::Decl(Decl::Trait("Test".into(), vec![]))));
    // http://php.net/manual/de/language.oop5.traits.php
    assert_eq!(process_stmt("trait HelloWorld {use Hello, World;}"), enb!(0,36, Expr_::Decl(Decl::Trait("HelloWorld".into(), vec![
        Member::TraitUse(vec![Path::Identifier("Hello".into()), Path::Identifier("World".into())], vec![])
    ]))));
}

#[test]
fn parse_interface_decl() {
    assert_eq!(process_stmt("interface ITest {}"), enb!(0,18, Expr_::Decl(Decl::Interface("ITest".into(), vec![], vec![]))));
    assert_eq!(process_stmt("interface ITest { public function test(); }"), enb!(0,43, Expr_::Decl(
        Decl::Interface("ITest".into(), vec![], vec![ Member::Method(MemberModifiers::new(&[MemberModifier::Public]),
            "test".into(), FunctionDecl {params: vec![], body: Block(vec![]), usev: vec![], ret_ref: false})
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
    assert_eq!(process_stmt(code), enb!(0,163, Expr_::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifiers::none(), name: "Aliased_Talker".into(), base_class: None, implements: vec![], members: vec![
            Member::TraitUse(vec![Path::Identifier("A".into()), Path::Identifier("B".into())], vec![
                TraitUse::InsteadOf(Path::Identifier("B".into()), "smallTalk".into(), vec![Path::Identifier("A".into())]),
                TraitUse::InsteadOf(Path::Identifier("A".into()), "bigTalk".into(), vec![Path::Identifier("B".into())]),
                TraitUse::As(Path::Identifier("B".into()), "bigTalk".into(), MemberModifiers::none(), Some("talk".into())),
            ])
        ]
    }))));
}

#[test]
fn parse_static_decl() {
    assert_eq!(process_stmt("static $t=true;"), enb!(0,14, Expr_::Decl(Decl::StaticVars(vec![ ("t".into(), Some(enb!(10,14, constant!(true)))) ]))));
}

#[test]
fn parse_stmt_closure_use() {
    assert_eq!(process_stmt("return function () use ($t) {};"), enb!(0,6, Expr_::Return(Some(eb!(7,30, Expr_::Function(FunctionDecl {
        params: vec![], body: Block(vec![]), usev: vec![(false, "t".into())], ret_ref: false,
    }))))));
}

#[test]
fn parse_namespace_decl() {
    assert_eq!(process_stmt("namespace Foo\\Bar;"), enb!(0,17, Expr_::Decl(Decl::Namespace(Path::NsIdentifier("Foo".into(), "Bar".into())))));
}
