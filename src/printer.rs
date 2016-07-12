use std::fmt::{self, Write};
use ast::{ClassDecl, ClassMember, ClassModifier, Decl, Expr, FunctionDecl, Modifiers, ParamDefinition, Path, ParsedItem, Ty, UseClause, Op, Visibility};

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expr::None => panic!("printing a none-expression"),
            Expr::True => write!(f, "true"),
            Expr::False => write!(f, "false"),
            Expr::Null => write!(f, "null"),

            Expr::Path(ref p) => write!(f, "{}", p),
            Expr::String(ref str_) => write!(f, "{:?}", str_),
            Expr::Int(ref i) => write!(f, "{}", i),
            Expr::Array(ref items) => {
                try!(write!(f, "["));
                for (i, item) in items.iter().enumerate() {
                    try!(write_comma_separator(f, i));
                    match *item.0 {
                        Expr::None => (),
                        ref x => try!(write!(f, "{} => ", x)),
                    }
                    try!(write!(f, "{}", item.1))
                }
                write!(f, "]")
            },
            Expr::Variable(ref var) => write!(f, "${}", var),
            Expr::Reference(ref expr) => write!(f, "&{}", expr),
            Expr::Block(ref block) => {
                try!(write!(f, "{{\n"));
                let mut i_str = String::new();
                for item in block {
                    try!(write!(i_str, "{};\n", item));
                }
                try!(write!(f, "{};\n", ident_str(i_str)));
                write!(f, "}}\n")
            },
            Expr::Use(ref items) => {
                for item in items {
                    try!(write!(f, "use {};", item));
                }
                Ok(())
            },
            Expr::Echo(ref args) => {
                try!(write!(f, "echo "));
                try!(write_args(f, args));
                write!(f, ";")
            },
            Expr::Isset(ref args) => {
                try!(write!(f, "isset("));
                try!(write_args(f, args));
                write!(f, ")")
            },
            Expr::Unset(ref args) => {
                try!(write!(f, "unset("));
                try!(write_args(f, args));
                write!(f, ")")
            },
            Expr::Return(ref arg) => {
                try!(write!(f, "return"));
                match **arg {
                    Expr::None => (),
                    ref arg => {
                        try!(write!(f, " {}", arg));
                    }
                }
                Ok(())
            },
            Expr::Throw(ref arg) => {
                write!(f, "throw {}", arg)
            },
            Expr::Break(ref amount) => {
                match *amount {
                    1 => write!(f, "break;"),
                    _ => write!(f, "break {};", amount),
                }
            },
            Expr::Continue(ref amount) => {
                match *amount {
                    1 => write!(f, "continue;"),
                    _ => write!(f, "continue {};", amount),
                }
            },
            Expr::ArrayIdx(ref obj, ref items) => {
                try!(write!(f, "{}", obj));
                for item in items {
                    // for now we tolerate it, TODO: rework & verify that Expr::None is toleratable/valid at this point (on the parsing side)
                    if let Expr::None = *item {
                        try!(write!(f, "[]"));
                    } else {
                        try!(write!(f, "[{}]", item));
                    }
                }
                Ok(())
            },
            Expr::ObjMember(ref obj, ref props) => {
                try!(write!(f, "{}", obj));
                for prop in props {
                    try!(write!(f, "->{}", prop));
                }
                Ok(())
            },
            Expr::StaticMember(ref obj, ref props) => {
                try!(write!(f, "{}", obj));
                for prop in props {
                    try!(write!(f, "::{}", prop));
                }
                Ok(())
            },
            Expr::Call(ref obj, ref args) => {
                try!(write!(f, "{}(", obj));
                try!(write_args(f, args));
                write!(f, ")")
            },
            Expr::New(ref path, ref args) => {
                try!(write!(f, "new {}(", path));
                try!(write_args(f, args));
                write!(f, ")")
            },
            Expr::UnaryOp(ref operator, ref expr) => {
                let op_str = match *operator {
                    Op::Not => "!",
                    _ => panic!("op {:?} unsupported (unary)", operator)
                };
                write!(f, "{}({})", op_str, expr)
            },
            Expr::BinaryOp(ref operator, ref op1, ref op2) => {
                if let Op::Instanceof = *operator{
                    try!(write!(f, "({}) instanceof {}", op1, op2));
                    return Ok(())
                }
                let op_str = match *operator {
                    Op::And => "&&",
                    Op::Identical => "===",
                    Op::NotIdentical => "!==",
                    Op::Eq => "==",
                    Op::Neq => "!=",
                    _ => panic!("op {:?} unsupported (binary)", operator)
                };
                write!(f, "({}) {} ({})", op1, op_str, op2)
            },
            Expr::Cast(ref ty, ref e) => {
                write!(f, "({}){}", ty, e)
            },
            Expr::Function(ref decl) => {
                write!(f, "function {}", decl)
            },
            Expr::Assign(ref obj, ref value) => {
                write!(f, "{} = {}", obj, value)
            },
            Expr::AssignRef(ref obj, ref value) => {
                write!(f, "{} = &({})", obj, value)
            },
            Expr::If(ref condition, ref case_true, ref case_else) => {
                try!(write!(f, "if ({}) {}", condition, case_true));
                match **case_else {
                    Expr::None => (),
                    _ => {
                        try!(write!(f, " else {}", case_else));
                    }
                }
                Ok(())
            },
            Expr::TernaryIf(ref condition, ref case_true, ref case_else) => {
                try!(write!(f, "{}?", condition));
                match **case_true {
                    Expr::None => (),
                    _ => try!(write!(f, "{}", case_true)),
                }
                write!(f, ":{}", case_else)
            },
            Expr::While(ref condition, ref body) => {
                write!(f, "while ({}) {}", condition, body)
            },
            Expr::DoWhile(ref body, ref condition) => {
                write!(f, "do {} while({});", body, condition)
            },
            Expr::ForEach(ref obj, ref k, ref v, ref body) => {
                try!(write!(f, "foreach ({} as ", obj));
                match **k {
                    Expr::None => (),
                    _ => try!(write!(f, "{} => ", k))
                }
                write!(f, "{}) {}", v, body)
            },
            Expr::Switch(ref cond, ref cases) => {
                try!(write!(f, "switch({}) {{", cond));
                for case in cases {
                    for conds in &case.0 {
                        match *conds {
                            Expr::None => try!(write!(f, "default:")),
                            ref case => try!(write!(f, "case {}:", case)),
                        }
                    }
                    try!(write!(f, "{}", case.1));
                }
                write!(f, "}}")
            },
            Expr::List(ref items) => {
                try!(write!(f, "list("));
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        try!(write!(f, ","))
                    }
                    match item.0 {
                        Expr::None => (),
                        ref x => try!(write!(f, "{} => ", x)),
                    }
                    match item.1 {
                        Expr::None => (),
                        ref x => try!(write!(f, "{}", x)),
                    }
                }
                write!(f, ")")
            },
            Expr::Try(ref bl, ref clauses, ref finally) => {
                try!(write!(f, "try {}", bl));
                for clause in clauses {
                    try!(write!(f, "catch ({} ${}) {}", clause.ty, clause.var, clause.block));
                }
                match **finally {
                    Expr::None => Ok(()),
                    ref finally => write!(f, "finally {}", finally)
                }
            },
            Expr::Decl(ref decl) => write!(f, "{}\n", decl)
        }
    }
}

impl<'a> fmt::Display for Ty<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str_ = match *self {
            Ty::Array => "array",
            Ty::Callable => "callable",
            Ty::Bool => "bool",
            Ty::Float => "float",
            Ty::Int => "int",
            Ty::String => "string",
            Ty::Object(ref p) => {
                try!(write!(f, "{}", p));
                ""
            }
        };
        write!(f, "{}", str_)
    }
}

impl<'a> fmt::Display for UseClause<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UseClause::QualifiedName(ref parts, ref asc) => {
                match *asc {
                    None => write!(f, "{}", parts),
                    Some(ref x) => write!(f, "{} as {}", parts, x),
                }
            }
        }
    }
}

impl<'a> fmt::Display for Path<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Path::Identifier(ref ns) => write!(f, "{}", ns),
            Path::NsIdentifier(ref ns, ref cls) => write!(f, "{}\\{}", ns, cls),
        }
    }
}

impl<'a> fmt::Display for Decl<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Decl::Namespace(ref uid) => write!(f, "namespace {};", uid.join("\\")),
            Decl::GlobalFunction(ref name, ref decl) => {
                try!(write!(f, "function {}", name));
                write!(f, "{}", decl)
            },
            Decl::Class(ref decl) => write!(f, "{}", decl),
            Decl::Interface(ref name, ref implements, ref members) => {
                try!(write!(f, "interface {}", name));
                if implements.len() > 0 {
                    try!(write!(f, " extends "));
                    for (i, iface) in implements.iter().enumerate() {
                        try!(write_comma_separator(f, i));
                        try!(write!(f, "{}", iface));
                    }
                }
                try!(write!(f, " {{\n"));
                for member in members {
                    try!(write!(f, "{}\n", member));
                }
                write!(f, "}}\n")
            },
            Decl::Trait(ref name, ref members) => {
                try!(write!(f, "trait {} {{\n", name));
                for member in members {
                    try!(write!(f, "{}\n", member));
                }
                write!(f, "}}\n")
            },
        }
    }
}

impl<'a> fmt::Display for FunctionDecl<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "("));
        for (i, param) in self.params.iter().enumerate() {
            try!(write_comma_separator(f, i));
            try!(write!(f, "{}", param));
        }
        try!(write!(f, ")"));
        if self.usev.len() > 0 {
            try!(write!(f, " use ("));
            for (i, usev) in self.usev.iter().enumerate() {
                try!(write_comma_separator(f, i));
                try!(write!(f, "${}", usev));
            }
            try!(write!(f, ") "));
        }
        try!(write!(f, "{{\n"));
        let mut i_str = String::new();
        for item in &self.body {
            try!(write!(i_str, "{};\n", item));
        }
        try!(write!(f, "{}\n", ident_str(i_str)));
        write!(f, "}}\n")
    }
}

impl<'a> fmt::Display for ParamDefinition<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ty {
            None => (),
            Some(ref ty) => try!(write!(f, "{} ", ty)),
        }
        try!(write!(f, "${}", self.name));
        match self.default {
            Expr::None => Ok(()),
            ref def => write!(f, " = {}", def),
        }
    }
}

impl fmt::Display for Modifiers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut modifiers = vec![];
        if self.0 {
            modifiers.push("static");
        }
        let vis = match self.1 {
            Visibility::None => "",
            Visibility::Public => "public",
            Visibility::Protected => "protected",
            Visibility::Private => "private",
        };
        if !vis.is_empty() {
            modifiers.push(vis);
        }
        let cls = match self.2 {
            ClassModifier::None => "",
            ClassModifier::Abstract => "abstract",
            ClassModifier::Final => "final",
        };
        if !cls.is_empty() {
            modifiers.push(cls);
        }
        write!(f, "{}", modifiers.join(" "))
    }
}

impl<'a> fmt::Display for ClassDecl<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "class {}", self.name));
        if let Some(ref base_class) = self.base_class {
            try!(write!(f, " extends {}", base_class));
        }
        if self.implements.len() > 0 {
            try!(write!(f, " implements "));
            for (i, iface) in self.implements.iter().enumerate() {
                try!(write_comma_separator(f, i));
                try!(write!(f, "{}", iface));
            }
        }
        try!(write!(f, " {{\n"));
        let mut i_str = String::new();
        for member in &self.members {
            try!(write!(i_str, "{}\n", member));
        }
        try!(write!(f, "{}\n", ident_str(i_str)));
        write!(f, "}}\n")
    }
}

impl <'a> fmt::Display for ClassMember<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ClassMember::Constant(ref name, ref value) => {
                write!(f, "const {}={};", name, value)
            },
            ClassMember::Property(ref modifiers, ref name, ref default) => {
                try!(write!(f, "{} ${}", modifiers, name));
                match *default {
                    Expr::None => (),
                    _ => try!(write!(f, " = {}", default))
                }
                write!(f, ";")
            },
            ClassMember::Method(ref modifiers, ref name, ref fdecl) => {
                write!(f, "{} function {} {}", modifiers, name, fdecl)
            },
            ClassMember::TraitUse(ref path, ref unsupported) => {
                assert_eq!(unsupported.len(), 0);
                assert_eq!(path.len(), 1);
                write!(f, "use {};", path[0])
            },
        }
    }
}

impl <'a> fmt::Display for ParsedItem<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParsedItem::Text(ref str_) => write!(f, "{}", str_),
            ParsedItem::CodeBlock(ref exprs) => {
                try!(write!(f, "<?php\n"));
                for expr in exprs {
                    try!(write!(f, "{}\n", expr));
                }
                write!(f, "?>\n")
            }
        }
    }
}

#[inline]
fn write_comma_separator<W: Write>(writer: &mut W, i: usize) -> fmt::Result {
    if i > 0 {
        try!(write!(writer, ", "));
    }
    Ok(())
}

#[inline]
fn write_args<W: Write>(writer: &mut W, args: &[Expr]) -> fmt::Result {
    for (i, arg) in args.iter().enumerate() {
        try!(write_comma_separator(writer, i));
        try!(write!(writer, "{}", arg));
    }
    Ok(())
}

fn ident_str(i_str: String) -> String {
    let ret_str: Vec<String> = i_str.split("\n").map(|x| format!("    {}", x)).collect();
    ret_str.join("\n")
}

pub fn parsed_items_to_code(items: &[ParsedItem]) -> Result<String, fmt::Error> {
    let mut code = String::new();
    for item in items {
        try!(write!(code, "{}", item));
    }
    Ok(code)
}
