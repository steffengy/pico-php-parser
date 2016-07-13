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
fn parse_stmt_compound_assignment() {
    assert_eq!(process_stmt("$test+=4;"), Expr::CompoundAssign(Box::new(Expr::Variable("test".into())), Op::Add, Box::new(Expr::Int(4))));
}

#[test]
fn parse_stmt_if_while() {
    let expr_var = Expr::Variable("a".into());
    let block_stmt = Expr::Call(Box::new(Expr::Path(Path::Identifier("b".into()))), vec![]);
    let block_expr = Expr::Block(vec![block_stmt.clone()]);
    let args = [
        ("if", Expr::If(Box::new(expr_var.clone()), Box::new(block_expr.clone()), Box::new(Expr::None))),
        ("while", Expr::While(Box::new(expr_var.clone()), Box::new(block_expr)))
    ];
    for arg in &args {
        assert_eq!(process_stmt(&(arg.0.to_owned() + r#" ($a) { b(); }"#)), arg.1);
    }
    let args = [
        ("if", Expr::If(Box::new(expr_var.clone()), Box::new(block_stmt.clone()), Box::new(Expr::None))),
        ("while", Expr::While(Box::new(expr_var.clone()), Box::new(block_stmt)))
    ];
    for arg in &args {
        assert_eq!(process_stmt(&(arg.0.to_owned() + r#"($a) b();"#)), arg.1);
    }
}

#[test]
fn parse_stmt_if_else() {
    let make_result = |block_expr, else_expr| Expr::If(Box::new(Expr::Variable("a".into())), Box::new(block_expr), Box::new(else_expr));
    let main_call_expr = Expr::Call(Box::new(Expr::Path(Path::Identifier("a".into()))), vec![]);
    let block_expr = Expr::Block(vec![main_call_expr.clone()]);
    let call_expr = Expr::Call(Box::new(Expr::Path(Path::Identifier("b".into()))), vec![]);
    let else_expr = Expr::Block(vec![call_expr.clone()]);

    assert_eq!(process_stmt("if ($a) { a(); } else { b(); }"), make_result(block_expr, else_expr));
    assert_eq!(process_stmt("if ($a) a(); else b();"), make_result(main_call_expr, call_expr));
    //if, elseif, else
    let result2 = Expr::If(
        Box::new(Expr::Variable("a".into())),
        Box::new(Expr::Call(Box::new(Expr::Path(Path::Identifier("a".into()))), vec![])),
        Box::new(
            Expr::If(Box::new(Expr::Variable("b".into())),
                Box::new(Expr::Call(Box::new(Expr::Path(Path::Identifier("b".into()))), vec![])),
                Box::new(Expr::Call(Box::new(Expr::Path(Path::Identifier("c".into()))), vec![]))
            )
        )
    );
    assert_eq!(process_stmt("if ($a) a(); else if ($b) b(); else c();"), result2);
    assert_eq!(process_stmt("if ($a) a(); elseif ($b) b(); else c();"), result2);
}

#[test]
fn parse_stmt_do_while() {
    assert_eq!(process_stmt("do { test(); } while(count($a));"), Expr::DoWhile(
        Box::new(Expr::Block(vec![Expr::Call(Box::new(Expr::Path(Path::Identifier("test".into()))), vec![])])),
        Box::new(Expr::Call(Box::new(Expr::Path(Path::Identifier("count".into()))), vec![Expr::Variable("a".into())]))
    ));
}

#[test]
fn parse_stmt_foreach() {
    assert_eq!(process_stmt("foreach ($test as $v) { ok(); }"), Expr::ForEach(
        Box::new(Expr::Variable("test".into())),
        Box::new(Expr::None), // key
        Box::new(Expr::Variable("v".into())), // value
        Box::new(Expr::Block(vec![Expr::Call(Box::new(Expr::Path(Path::Identifier("ok".into()))), vec![])])) //body
    ));
    assert_eq!(process_stmt("foreach ($test as $k => $v) { ok(); }"), Expr::ForEach(
        Box::new(Expr::Variable("test".into())),
        Box::new(Expr::Variable("k".into())), // key
        Box::new(Expr::Variable("v".into())), // value
        Box::new(Expr::Block(vec![Expr::Call(Box::new(Expr::Path(Path::Identifier("ok".into()))), vec![])])) //body
    ));
}

#[test]
fn parse_func_decl() {
    assert_eq!(process_stmt("function test() { ok(); }"), Expr::Decl(Decl::GlobalFunction("test".into(), FunctionDecl { params: vec![],
        body: vec![Expr::Call(Box::new(Expr::Path(Path::Identifier("ok".into()))), vec![])], usev: vec![], ret_ref: false, })
    ));
    assert_eq!(process_stmt("function &test() { ok(); }"), Expr::Decl(Decl::GlobalFunction("test".into(), FunctionDecl { params: vec![],
        body: vec![Expr::Call(Box::new(Expr::Path(Path::Identifier("ok".into()))), vec![])], usev: vec![], ret_ref: true, })
    ));
    assert_eq!(process_stmt("function test($a) { ok(); }"), Expr::Decl(Decl::GlobalFunction("test".into(), FunctionDecl {
        params: vec![ParamDefinition { name: "a".into(), as_ref: false, ty: None, default: Expr::None }],
        body: vec![Expr::Call(Box::new(Expr::Path(Path::Identifier("ok".into()))), vec![])], usev: vec![], ret_ref: false, })
    ));
    assert_eq!(process_stmt("function test($a, $b) { ok(); }"), Expr::Decl(Decl::GlobalFunction("test".into(), FunctionDecl {
        params: vec![ParamDefinition { name: "a".into(), as_ref: false, ty: None, default: Expr::None }, ParamDefinition { name: "b".into(), as_ref: false, ty: None, default: Expr::None }],
        body: vec![Expr::Call(Box::new(Expr::Path(Path::Identifier("ok".into()))), vec![])], usev: vec![], ret_ref: false, })
    ));
}

#[test]
fn parse_func_decl_typehint() {
    assert_eq!(process_stmt("function test(Test $a) { ok(); }"), Expr::Decl(Decl::GlobalFunction("test".into(), FunctionDecl {
        params: vec![ParamDefinition { name: "a".into(), as_ref: false, ty: Some(Ty::Object(Some(Path::Identifier("Test".into())))), default: Expr::None }],
        body: vec![Expr::Call(Box::new(Expr::Path(Path::Identifier("ok".into()))), vec![])], usev: vec![], ret_ref: false, })
    ));
}

#[test]
fn parse_class_decl() {
    assert_eq!(process_stmt("class Test {}"), Expr::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifier::None, name: "Test".into(), base_class: None, implements: vec![], members: vec![]
    })));
    assert_eq!(process_stmt("final class Test {}"), Expr::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifier::Final, name: "Test".into(), base_class: None, implements: vec![], members: vec![]
    })));
    assert_eq!(process_stmt("class Test extends Abc\\Test2 {}"), Expr::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifier::None, name: "Test".into(), base_class: Some(Path::NsIdentifier("Abc".into(), "Test2".into())), implements: vec![], members: vec![]
    })));
    assert_eq!(process_stmt("class Test implements ITest {}"), Expr::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifier::None, name: "Test".into(), base_class: None, implements: vec![Path::Identifier("ITest".into())], members: vec![]
    })));
}

#[test]
fn parse_class_properties() {
    assert_eq!(process_stmt("class Test { public $test; }"), Expr::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifier::None, name: "Test".into(), base_class: None, implements: vec![],
        members: vec![ClassMember::Property(Modifiers(false, Visibility::Public, ClassModifier::None), "test".into(), Expr::None)],
    })));
    assert_eq!(process_stmt("class Test { protected $ab = []; }"), Expr::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifier::None, name: "Test".into(), base_class: None, implements: vec![],
        members: vec![ClassMember::Property(Modifiers(false, Visibility::Protected, ClassModifier::None), "ab".into(), Expr::Array(vec![])) ],
    })));
}

#[test]
fn parse_class_const() {
    assert_eq!(process_stmt("class Test { const C=true; }"), Expr::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifier::None, name: "Test".into(), base_class: None, implements: vec![],
        members: vec![ClassMember::Constant("C".into(), Expr::True)]
    })));
}

#[test]
fn parse_class_methods() {
    assert_eq!(process_stmt("class Test { public function a() { run(); } }"), Expr::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifier::None, name: "Test".into(), base_class: None, implements: vec![],
        members: vec![ClassMember::Method(Modifiers(false, Visibility::Public, ClassModifier::None), "a".into(), FunctionDecl {
            params: vec![], body: vec![Expr::Call(Box::new(Expr::Path(Path::Identifier("run".into()))), vec![])], usev: vec![], ret_ref: false,
        })]
    })));
    assert_eq!(process_stmt("class Test { public function __construct(array $param1 = []) { $this->param = $param1; } }"), Expr::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifier::None, name: "Test".into(), base_class: None, implements: vec![],
        members: vec![ClassMember::Method(Modifiers(false, Visibility::Public, ClassModifier::None), "__construct".into(), FunctionDecl {
            params: vec![ParamDefinition { name: "param1".into(), as_ref: false, ty: Some(Ty::Array), default: Expr::Array(vec![]) }],
            body: vec![Expr::Assign(Box::new(Expr::ObjMember(Box::new(Expr::Variable("this".into())), vec![
                Expr::Path(Path::Identifier("param".into())) ])), Box::new(Expr::Variable("param1".into())))
            ], usev: vec![], ret_ref: false,
        })]
    })));
}

#[test]
fn parse_trait_decl() {
    assert_eq!(process_stmt("trait Test {}"), Expr::Decl(Decl::Trait("Test".into(), vec![])));
    // http://php.net/manual/de/language.oop5.traits.php
    assert_eq!(process_stmt("trait HelloWorld {use Hello, World;}"), Expr::Decl(Decl::Trait("HelloWorld".into(), vec![
        ClassMember::TraitUse(vec![Path::Identifier("Hello".into()), Path::Identifier("World".into())], vec![])
    ])));
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
    assert_eq!(process_stmt(code), Expr::Decl(Decl::Class(ClassDecl {
        cmod: ClassModifier::None, name: "Aliased_Talker".into(), base_class: None, implements: vec![], members: vec![
            ClassMember::TraitUse(vec![Path::Identifier("A".into()), Path::Identifier("B".into())], vec![
                TraitUse::InsteadOf( (Path::Identifier("B".into()), Path::Identifier("smallTalk".into())), vec![Path::Identifier("A".into())] ),
                TraitUse::InsteadOf( (Path::Identifier("A".into()), Path::Identifier("bigTalk".into())), vec![Path::Identifier("B".into())] ),
                TraitUse::As( (Path::Identifier("B".into()), Path::Identifier("bigTalk".into())), Visibility::None, Some("talk".into()) ),
            ])
        ]
    })));
}

#[test]
fn parse_interface_decl() {
    assert_eq!(process_stmt("interface ITest {}"), Expr::Decl(Decl::Interface("ITest".into(), vec![], vec![])));
    assert_eq!(process_stmt("interface ITest { public function test(); }"), Expr::Decl(
        Decl::Interface("ITest".into(), vec![], vec![ClassMember::Method(Modifiers(false, Visibility::Public, ClassModifier::None),
            "test".into(), FunctionDecl {params: vec![], body: vec![], usev: vec![], ret_ref: false})
        ])
    ));
}

#[test]
fn parse_static_decl() {
    assert_eq!(process_stmt("static $t=true;"), Expr::Decl(Decl::StaticVars(vec![("t".into(), Expr::True)])));
}

#[test]
fn parse_class_trait_use() {
    assert_eq!(process_stmt("class Test { use Abc; }"), Expr::Decl(Decl::Class(ClassDecl { name: "Test".into(), base_class: None, implements: vec![], members: vec![
        ClassMember::TraitUse(vec![Path::Identifier("Abc".into())], vec![])
    ], cmod: ClassModifier::None})));
}

#[test]
fn parse_stmt_closure_use() {
    assert_eq!(process_stmt("return function () use ($t) {};"), Expr::Return(Box::new(Expr::Function(FunctionDecl {
        params: vec![], body: vec![], usev: vec!["t".into()], ret_ref: false,
    }))));
}

#[test]
fn parse_stmt_instanceof() {
    assert_eq!(process_stmt("if ($result instanceof Response) { return $result; }"), Expr::If(
        Box::new(Expr::BinaryOp(Op::Instanceof, Box::new(Expr::Variable("result".into())), Box::new(Expr::Path(Path::Identifier("Response".into()))))),
        Box::new(Expr::Block(vec![Expr::Return(Box::new(Expr::Variable("result".into())))])), Box::new(Expr::None),
    ));
}

#[test]
fn parse_stmt_try() {
    assert_eq!(process_stmt("try { echo \"ok\"; } catch (Exception $e) { return false;}"), Expr::Try(
        Box::new(Expr::Block(vec![Expr::Echo(vec![Expr::String("ok".into())])])),
        vec![CatchClause { ty: Path::Identifier("Exception".into()), var: "e".into(), block: Expr::Block(vec![Expr::Return(Box::new(Expr::False))]) }],
        Box::new(Expr::None),
    ));
    assert_eq!(process_stmt("try { echo \"ok\"; } catch (Exception $e) { return false; } catch (Throwable $e) { return true; }"), Expr::Try(
        Box::new(Expr::Block(vec![Expr::Echo(vec![Expr::String("ok".into())])])),
        vec![CatchClause { ty: Path::Identifier("Exception".into()), var: "e".into(), block: Expr::Block(vec![Expr::Return(Box::new(Expr::False))]) },
            CatchClause { ty: Path::Identifier("Throwable".into()), var: "e".into(), block: Expr::Block(vec![Expr::Return(Box::new(Expr::True))]) }],
        Box::new(Expr::None),
    ));
}

#[test]
fn parse_namespace_decl() {
    assert_eq!(process_stmt("namespace Foo\\Bar;"), Expr::Decl(Decl::Namespace(vec!["Foo".into(), "Bar".into()])));
}

#[test]
fn parse_use_statement() {
    assert_eq!(process_stmt("use Test;"), Expr::Use(vec![UseClause::QualifiedName(Path::Identifier("Test".into()), None) ]));
    assert_eq!(process_stmt("use Ab\\Cd\\Ef\\Gh\\Ij as Ga;"), Expr::Use(vec![UseClause::QualifiedName(
        Path::NsIdentifier("Ab\\Cd\\Ef\\Gh".into(), "Ij".into()),
        Some("Ga".into()))
    ]));
}

#[test]
fn parse_continue_statement() {
    assert_eq!(process_stmt("continue;"), Expr::Continue(1));
    assert_eq!(process_stmt("continue 2;"), Expr::Continue(2));
}

#[test]
fn parse_break_statement() {
    assert_eq!(process_stmt("break;"), Expr::Break(1));
    assert_eq!(process_stmt("break 2;"), Expr::Break(2));
}

#[test]
fn parse_switch_statement() {
    assert_eq!(process_stmt(r#"switch ($test) { case 1: echo "1"; break; default: echo "2"; }"#), Expr::Switch(Box::new(Expr::Variable("test".into())),
        vec![ (vec![Expr::Int(1)], Expr::Block(vec![Expr::Echo(vec![Expr::String("1".into())]), Expr::Break(1)])), (vec![Expr::None], Expr::Echo(vec![Expr::String("2".into())])) ]));
    assert_eq!(process_stmt(r#"switch ($test) { case 1: echo "1"; default: echo "2"; }"#), Expr::Switch(Box::new(Expr::Variable("test".into())),
        vec![ (vec![Expr::Int(1)], Expr::Echo(vec![Expr::String("1".into())])), (vec![Expr::None], Expr::Echo(vec![Expr::String("2".into())])) ]));
    assert_eq!(process_stmt("switch ($test) { case 1: case 2: echo 1; }"), Expr::Switch(Box::new(Expr::Variable("test".into())),
        vec![(vec![Expr::Int(1), Expr::Int(2)], Expr::Echo(vec![Expr::Int(1)])) ]));
    assert_eq!(process_stmt("switch ($test) { case 1: case 2: case 3: case 4: echo 1; }"), Expr::Switch(Box::new(Expr::Variable("test".into())),
        vec![(vec![Expr::Int(1), Expr::Int(2), Expr::Int(3), Expr::Int(4)], Expr::Echo(vec![Expr::Int(1)])) ]));
    assert_eq!(process_stmt("{ switch ($test) { case 1: echo 2; } echo 3; }"), Expr::Block(vec![Expr::Switch(Box::new(Expr::Variable("test".into())),
        vec![ (vec![Expr::Int(1)], Expr::Echo(vec![Expr::Int(2)])) ]),
        Expr::Echo(vec![Expr::Int(3)])
    ]));
}

#[test]
fn parse_list_statement() {
    assert_eq!(process_stmt("list($a, $b) = test();"), Expr::Assign(Box::new(Expr::List(
        vec![(Expr::None, Expr::Variable("a".into())), (Expr::None, Expr::Variable("b".into()))]
    )), Box::new(Expr::Call(Box::new(Expr::Path(Path::Identifier("test".into()))), vec![]))));
}

#[test]
fn parse_unset_statement() {
    assert_eq!(process_stmt("unset($a);"), Expr::Unset(vec![Expr::Variable("a".into())]));
}

#[test]
fn parse_stmt_throw() {
    assert_eq!(process_stmt(r#"throw new Exception("test");"#), Expr::Throw(Box::new(Expr::New(Box::new(Expr::Path(Path::Identifier("Exception".into()))),
        vec![Expr::String("test".into())])))
    );
    assert_eq!(process_stmt(r#"throw new Exception;"#), Expr::Throw(Box::new(Expr::New(Box::new(Expr::Path(Path::Identifier("Exception".into()))), vec![]))));
}

#[test]
fn parse_stmt_include() {
    assert_eq!(process_stmt("include $test;"), Expr::Include(IncludeTy::Include, Box::new(Expr::Variable("test".into()))));
    assert_eq!(process_stmt("include_once($test);"), Expr::Include(IncludeTy::IncludeOnce, Box::new(Expr::Variable("test".into()))));
    assert_eq!(process_stmt("require($test);"), Expr::Include(IncludeTy::Require, Box::new(Expr::Variable("test".into()))));
    assert_eq!(process_stmt("require_once $test;"), Expr::Include(IncludeTy::RequireOnce, Box::new(Expr::Variable("test".into()))));
}

#[test]
fn parse_stmt_exit() {
    assert_eq!(process_stmt("exit;"), Expr::Exit(Box::new(Expr::None)));
    assert_eq!(process_stmt("exit true;"), Expr::Exit(Box::new(Expr::True)));
    assert_eq!(process_stmt("exit(true);"), Expr::Exit(Box::new(Expr::True)));
}

#[test]
fn parse_stmt_new_as_param() {
    assert_eq!(process_stmt("r(new Foo);"), Expr::Call(Box::new(Expr::Path(Path::Identifier("r".into()))), vec![
        Expr::New(Box::new(Expr::Path(Path::Identifier("Foo".into()))), vec![])])
    );
}

#[test]
fn parse_stmt_new() {
    assert_eq!(process_stmt("return new $var($this);"), Expr::Return(Box::new(Expr::New(Box::new(Expr::Variable("var".into())), vec![Expr::Variable("this".into())]))));
}
