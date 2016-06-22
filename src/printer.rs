use std::fmt::{self, Write};
use ast::{ClassDecl, ClassMember, ClassModifier, Decl, Expr, FunctionDecl, Modifiers, ParamDefinition, Path, ParsedItem, UseClause, Visibility};

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expr::None => panic!("printing a none-expression"),
            Expr::True => write!(f, "true"),
            Expr::False => write!(f, "false"),
            Expr::Null => write!(f, "null"),
            Expr::Identifier(ref i) => write!(f, "{}", i),
            Expr::NsIdentifier(ref ni) => write!(f, "{}", ni.join("\\")),
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
                for (i, arg) in args.iter().enumerate() {
                    try!(write_comma_separator(f, i));
                    try!(write!(f, "{}", arg));
                }
                write!(f, ";")
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
            Expr::ArrayIdx(ref obj, ref items) => {
                try!(write!(f, "{}", obj));
                for item in items {
                    try!(write!(f, "[{}]", item));
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
                for (i, arg) in args.iter().enumerate() {
                    try!(write_comma_separator(f, i));
                    try!(write!(f, "{}", arg));
                }
                write!(f, ")")
            },
            Expr::New(ref path, ref args) => {
                try!(write!(f, "new {}(", path));
                for (i, arg) in args.iter().enumerate() {
                    try!(write_comma_separator(f, i));
                    try!(write!(f, "{}", arg));
                }
                write!(f, ")")
            },
            Expr::UnaryOp(ref operator, ref expr) => {
                match operator {
                    _ => panic!("op {:?} unsupported (unary)", operator)
                }
                Ok(())
            },
            Expr::BinaryOp(ref operator, ref op1, ref op2) => {
                match operator {
                    _ => panic!("op {:?} unsupported (binary)", operator)
                }
                Ok(())
            },
            Expr::Function(ref decl) => {
                write!(f, "function {}", decl)
            },
            Expr::Assign(ref obj, ref value) => {
                write!(f, "{} = {};\n", obj, value)
            },
            Expr::AssignRef(ref obj, ref value) => {
                write!(f, "{} = &({});\n", obj, value)
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
            Expr::While(ref condition, ref body) => {
                write!(f, "while ({}) {}", condition, body)
            },
            Expr::DoWhile(ref body, ref condition) => {
                write!(f, "do {} while({});", body, condition)
            },
            Expr::ForEach(ref obj, ref k, ref v, ref body) => {
                try!(write!(f, "foreach ({}", obj));
                match **k {
                    Expr::None => (),
                    _ => try!(write!(f, "{} => ", k))
                }
                write!(f, "{}) {}", v, body)
            },
            Expr::Decl(ref decl) => write!(f, "{}\n", decl)
        }
    }
}

impl<'a> fmt::Display for UseClause<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UseClause::QualifiedName(ref parts) => {
                write!(f, "{}", parts.join("\\"))
            }
        }
    }
}

impl<'a> fmt::Display for Path<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Path::Class(ref ns) => write!(f, "{}", ns),
            Path::NamespacedClass(ref ns, ref cls) => write!(f, "{}\\{}", ns, cls),
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
            Decl::Class(ref decl) => write!(f, "{}", decl)
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
        try!(write!(f, ") {{\n"));
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
        write!(f, "${}", self.name)
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
            }
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
