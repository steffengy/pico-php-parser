use std::fmt::{self, Write};
use std::iter;
use ast::{ClassDecl, ClassMember, ClassModifier, Decl, Expr, FunctionDecl, Modifiers, ParamDefinition, Path,
    ParsedItem, Ty, IncludeTy, TraitUse, UseClause, UnaryOp, Op, Visibility};

pub struct PrettyPrinter<W: Write> {
    indent_level: usize,
    nested_level: usize,
    skip_semicol: bool,
    allow_inject_semi: bool,
    inside_iface: bool,
    writer: W,
}

impl<W: Write> Write for PrettyPrinter<W> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        if self.allow_inject_semi {
            self.allow_inject_semi = false;
            if s == ";" && !self.skip_semicol {
                try!(self.writer.write_str(";\n"));
                return self.write_indent()
            } else {
                try!(self.writer.write_str("\n"));
                try!(self.write_indent());
            }
        }
        if s == "{" {
            self.indent_level += 1;
            self.skip_semicol = false;
            try!(self.writer.write_str(" {\n"));
            try!(self.write_indent());
        } else if s == "}" {
            try!(self.writer.write_str("\n"));
            self.indent_level -= 1;
            self.allow_inject_semi = true;
            try!(self.write_indent());
            try!(self.writer.write_str("}"));
        } else if s == ";" {
            if self.nested_level == 1 && !self.skip_semicol {
                try!(self.writer.write_str(s));
                try!(self.writer.write_char('\n'));
                try!(self.write_indent());
            }
            self.skip_semicol = false;
        } else {
            try!(self.writer.write_str(s));
        }
        Ok(())
    }

    #[inline]
    fn write_char(&mut self, c: char) -> fmt::Result {
        assert!(c != '\n' && c != ';' && c != '}' && c != '{');
        self.writer.write_char(c)
    }

    #[inline]
    fn write_fmt(&mut self, args: fmt::Arguments) -> fmt::Result {
        fmt::write(self, args)
    }
}

impl<W: Write> PrettyPrinter<W> {
    pub fn new(target: W) -> PrettyPrinter<W> {
        PrettyPrinter {
            indent_level: 0,
            nested_level: 0,
            skip_semicol: false,
            allow_inject_semi: false,
            inside_iface: false,
            writer: target,
        }
    }

    pub fn done(self) -> W {
        self.writer
    }

    #[inline]
    fn write_indent(&mut self) -> fmt::Result {
        self.writer.write_str(&iter::repeat("    ").take(self.indent_level).collect::<String>())
    }
}

impl <'a> fmt::Display for ParsedItem<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ParsedItem::Text(ref str_) => write!(f, "{}", str_),
            ParsedItem::CodeBlock(ref exprs) => {
                try!(write!(f, "<?php\n"));
                let mut printer = PrettyPrinter::new(f);
                for expr in exprs {
                    try!(expr.pretty_print(&mut printer));
                }
                write!(printer.done(), "\n?>\n")
            }
        }
    }
}

trait PrettyPrint<W: Write> {
    fn pretty_print(&self, printer: &mut PrettyPrinter<W>) -> fmt::Result;
}

#[inline]
fn needs_parentheses(e: &Expr) -> bool {
    match *e {
        Expr::Assign(_, _) | Expr::AssignRef(_, _) | Expr::CompoundAssign(_, _, _) => true,
        Expr::New(_, _) | Expr::UnaryOp(_, _) | Expr::BinaryOp(_, _, _) | Expr::Cast(_, _) => true,
        _ => false
    }
}

#[inline]
fn needs_curly_braces(e: &Expr) -> bool {
    if needs_parentheses(e) {
        return true
    }
    match *e {
        Expr::StaticMember(_, _) | Expr::ObjMember(_, _) | Expr::Call(_, _) | Expr::TernaryIf(_, _, _) => true,
        _ => false,
    }
}

impl<'a, W: Write> PrettyPrint<W> for Expr<'a> {
    #[inline]
    fn pretty_print(&self, printer: &mut PrettyPrinter<W>) -> fmt::Result {
        printer.nested_level += 1;

        let require_parents = printer.nested_level > 1 && needs_parentheses(self);
        if require_parents {
            try!(write!(printer, "("));
        }

        let ret = match *self {
            Expr::None => panic!("printing a none-expression"),
            Expr::True => write!(printer, "true"),
            Expr::False => write!(printer, "false"),
            Expr::Null => write!(printer, "null"),
            Expr::Path(ref p) => write!(printer, "{}", p),
            // for strings bypas .writer since else a string like "}}" is interpreted as start of a body
            // and might even cause crashes (when indent_level underflows)
            Expr::String(ref str_) => write!(printer.writer, "{:?}", str_),
            Expr::Int(ref i) => write!(printer, "{}", i),
            Expr::Variable(ref var) => write!(printer, "${}", var),
            Expr::Reference(ref expr) => {
                try!(write!(printer, "&"));
                expr.pretty_print(printer)
            },
            Expr::Clone(ref arg) => {
                try!(write!(printer, "clone "));
                arg.pretty_print(printer)
            },
            Expr::Array(ref items) => {
                try!(write!(printer, "["));
                for (i, item) in items.iter().enumerate() {
                    try!(write_comma_separator(printer, i));
                    match *item.0 {
                        Expr::None => (),
                        ref x => {
                            try!(x.pretty_print(printer));
                            try!(write!(printer, " => "));
                        }
                    }
                    try!(item.1.pretty_print(printer))
                }
                write!(printer, "]")
            },
            Expr::Block(ref block) => {
                try!(write!(printer, "{{"));
                let bak = printer.nested_level;
                printer.nested_level = 0;
                for item in block {
                    try!(item.pretty_print(printer));
                }
                printer.nested_level = bak;
                printer.skip_semicol = true;
                write!(printer, "}}")
            },
            Expr::Use(ref items) => {
                for item in items {
                    try!(write!(printer, "use {}", item));
                }
                Ok(())
            },
            Expr::Exit(ref arg) => match **arg {
                Expr::None => write!(printer, "exit()"),
                _ => {
                    try!(write!(printer, "exit("));
                    try!(arg.pretty_print(printer));
                    write!(printer, ")")
                }
            },
            Expr::Echo(ref args) => {
                try!(write!(printer, "echo "));
                write_args(printer, args)
            },
            Expr::Isset(ref args) => {
                try!(write!(printer, "isset("));
                try!(write_args(printer, args));
                write!(printer, ")")
            },
            Expr::Empty(ref arg) => {
                try!(write!(printer, "empty("));
                try!(arg.pretty_print(printer));
                write!(printer, ")")
            },
            Expr::Unset(ref args) => {
                try!(write!(printer, "unset("));
                try!(write_args(printer, args));
                write!(printer, ")")
            },
            Expr::Return(ref arg) => {
                let result = write!(printer, "return");
                match **arg {
                    Expr::None => result,
                    ref arg => {
                        try!(result);
                        try!(write!(printer, " "));
                        arg.pretty_print(printer)
                    }
                }
            },
            Expr::Throw(ref arg) => {
                try!(write!(printer, "throw "));
                arg.pretty_print(printer)
            },
            Expr::Break(ref amount) => {
                match *amount {
                    1 => write!(printer, "break"),
                    _ => write!(printer, "break {}", amount),
                }
            },
            Expr::Continue(ref amount) => {
                match *amount {
                    1 => write!(printer, "continue"),
                    _ => write!(printer, "continue {}", amount),
                }
            },
            Expr::Include(ref ty, ref expr) => {
                let fn_ = match *ty {
                    IncludeTy::Include => "include",
                    IncludeTy::Require => "require",
                    IncludeTy::RequireOnce => "require_once",
                    IncludeTy::IncludeOnce => "include_once",
                };
                try!(write!(printer, "{} ", fn_));
                expr.pretty_print(printer)
            },
            Expr::ArrayIdx(ref obj, ref items) => {
                try!(obj.pretty_print(printer));
                for item in items {
                    // for now we tolerate it, TODO: rework & verify that Expr::None is toleratable/valid at this point (on the parsing side)
                    if let Expr::None = *item {
                        try!(write!(printer, "[]"));
                    } else {
                        try!(write!(printer, "["));
                        try!(item.pretty_print(printer));
                        try!(write!(printer, "]"));
                    }
                }
                Ok(())
            },
            Expr::ObjMember(ref obj, ref props) => {
                try!(obj.pretty_print(printer));
                for prop in props {
                    try!(write!(printer, "->"));
                    let curly_braces = needs_curly_braces(prop);
                    // force a direct-write since we do not want to mess up the indentation
                    if curly_braces {
                        try!(write!(printer.writer, "{{"));
                    }
                    try!(prop.pretty_print(printer));
                    if curly_braces {
                        try!(write!(printer.writer, "}}"));
                    }
                }
                Ok(())
            },
            Expr::StaticMember(ref obj, ref props) => {
                try!(obj.pretty_print(printer));
                for prop in props {
                    try!(write!(printer, "::"));
                    try!(prop.pretty_print(printer));
                }
                Ok(())
            },
            Expr::Call(ref obj, ref args) => {
                try!(obj.pretty_print(printer));
                try!(write!(printer, "("));
                try!(write_args(printer, args));
                write!(printer, ")")
            },
            Expr::New(ref path, ref args) => {
                try!(write!(printer, "new "));
                try!(path.pretty_print(printer));
                try!(write!(printer, "("));
                try!(write_args(printer, args));
                write!(printer, ")")
            },
            Expr::ErrorControl(ref expr) => {
                try!(write!(printer, "@"));
                expr.pretty_print(printer)
            },
            Expr::UnaryOp(ref operator, ref expr) => {
                let post_op = match *operator {
                    UnaryOp::PostInc => "++",
                    UnaryOp::PostDec => "--",
                    _ => ""
                };
                if post_op.len() > 0 {
                    try!(expr.pretty_print(printer));
                    write!(printer, "{}", post_op)
                } else {
                    let op_str = match *operator {
                        UnaryOp::Not => "!",
                        UnaryOp::BitwiseNot => "~",
                        UnaryOp::PreInc => "++",
                        UnaryOp::PreDec => "--",
                        UnaryOp::Positive => "+",
                        UnaryOp::Negative => "-",
                        UnaryOp::PostInc | UnaryOp::PostDec => unreachable!(),
                    };
                    try!(write!(printer, "{}", op_str));
                    expr.pretty_print(printer)
                }
            },
            Expr::BinaryOp(ref operator, ref op1, ref op2) => {
                try!(op1.pretty_print(printer));
                try!(write!(printer, " {} ", operator));
                op2.pretty_print(printer)
            },
            Expr::Cast(ref ty, ref e) => {
                try!(write!(printer, "({})", ty));
                e.pretty_print(printer)
            },
            Expr::Function(ref decl) => {
                try!(write!(printer, "function "));
                decl.pretty_print(printer)
            },
            Expr::Assign(ref obj, ref value) => {
                try!(obj.pretty_print(printer));
                try!(write!(printer, " = "));
                value.pretty_print(printer)
            },
            Expr::CompoundAssign(ref obj, ref op, ref operand) => {
                try!(obj.pretty_print(printer));
                try!(write!(printer, " {}= ", op));
                operand.pretty_print(printer)
            },
            Expr::AssignRef(ref obj, ref value) => {
                try!(obj.pretty_print(printer));
                try!(write!(printer, " = &"));
                value.pretty_print(printer)
            },
            Expr::If(ref condition, ref case_true, ref case_else) => {
                try!(write!(printer, "if ("));
                try!(condition.pretty_print(printer));
                try!(write!(printer, ") "));
                let result = case_true.pretty_print(printer);
                match **case_else {
                    Expr::None => result,
                    _ => {
                        try!(result);
                        try!(write!(printer, "else "));
                        case_else.pretty_print(printer)
                    }
                }
            },
            Expr::TernaryIf(ref condition, ref case_true, ref case_else) => {
                try!(condition.pretty_print(printer));
                try!(write!(printer, "?"));
                match **case_true {
                    Expr::None => (),
                    _ => try!(case_true.pretty_print(printer)),
                }
                try!(write!(printer, ":"));
                case_else.pretty_print(printer)
            },
            Expr::While(ref condition, ref body) => {
                try!(write!(printer, "while ("));
                try!(condition.pretty_print(printer));
                try!(write!(printer, ") "));
                body.pretty_print(printer)
            },
            Expr::DoWhile(ref body, ref condition) => {
                try!(write!(printer, "do "));
                try!(body.pretty_print(printer));
                try!(write!(printer, " while("));
                try!(condition.pretty_print(printer));
                write!(printer, ")")
            },
            Expr::ForEach(ref obj, ref k, ref v, ref body) => {
                try!(write!(printer, "foreach ("));
                try!(obj.pretty_print(printer));
                try!(write!(printer, " as "));
                match **k {
                    Expr::None => (),
                    _ => {
                        try!(k.pretty_print(printer));
                        try!(write!(printer, " => "));
                    }
                }
                try!(v.pretty_print(printer));
                try!(write!(printer, ") "));
                body.pretty_print(printer)
            },
            Expr::Switch(ref cond, ref cases) => {
                try!(write!(printer, "switch("));
                try!(cond.pretty_print(printer));
                try!(write!(printer, ") "));
                try!(write!(printer, "{{"));
                for case in cases {
                    for conds in &case.0 {
                        match *conds {
                            Expr::None => try!(write!(printer, "default:\n")),
                            ref case => {
                                try!(write!(printer, "case "));
                                try!(case.pretty_print(printer));
                                try!(write!(printer, ":\n"));
                            }
                        }
                        try!(printer.write_indent());
                    }
                    let bak = printer.nested_level;
                    printer.nested_level = 0;
                    printer.indent_level += 1;
                    // fix indentation since the last write_indent call
                    try!(write!(printer, "    "));
                    try!(case.1.pretty_print(printer));
                    printer.indent_level -= 1;
                    try!(write!(printer, "\n"));
                    try!(printer.write_indent());
                    printer.nested_level = bak;
                }
                printer.skip_semicol = true;
                write!(printer, "}}")
            },
            Expr::List(ref items) => {
                try!(write!(printer, "list("));
                for (i, item) in items.iter().enumerate() {
                    try!(write_comma_separator(printer, i));
                    match item.0 {
                        Expr::None => (),
                        ref x => {
                            try!(x.pretty_print(printer));
                            try!(write!(printer, " => "));
                        }
                    }
                    match item.1 {
                        Expr::None => (),
                        ref x => try!(x.pretty_print(printer)),
                    }
                }
                write!(printer, ")")
            },
            Expr::Try(ref bl, ref clauses, ref finally) => {
                try!(write!(printer, "try "));
                try!(bl.pretty_print(printer));
                for clause in clauses {
                    try!(write!(printer, "catch ({} ", clause.ty));
                    try!(write!(printer, "${}) ", clause.var));
                    try!(clause.block.pretty_print(printer));
                }
                match **finally {
                    Expr::None => Ok(()),
                    ref finally => {
                        try!(write!(printer, "finally "));
                        finally.pretty_print(printer)
                    }
                }
            },
            Expr::Decl(ref decl) => decl.pretty_print(printer),
        };
        let is_decl = match *self {
            Expr::Decl(_) => true,
            _ => false
        };
        if require_parents{
            try!(write!(printer, ")"));
        } else if printer.nested_level < 2 && !is_decl {
            try!(write!(printer, ";"));
        }


        printer.nested_level -= 1;
        ret
    }
}

impl<'a, W: Write> PrettyPrint<W> for Decl<'a> {
    fn pretty_print(&self, printer: &mut PrettyPrinter<W>) -> fmt::Result {
        match *self {
            Decl::Namespace(ref uid) => write!(printer, "namespace {};", uid.join("\\")),
            Decl::GlobalFunction(ref name, ref decl) => {
                try!(write!(printer, "function {}", name));
                decl.pretty_print(printer)
            },
            Decl::Class(ref decl) => decl.pretty_print(printer),
            Decl::Interface(ref name, ref implements, ref members) => {
                try!(write!(printer, "interface {}", name));
                if implements.len() > 0 {
                    try!(write!(printer, " extends "));
                    for (i, iface) in implements.iter().enumerate() {
                        try!(write_comma_separator(printer, i));
                        try!(write!(printer, "{}", iface));
                    }
                }
                try!(write!(printer, "{{"));
                printer.inside_iface = true;
                for member in members {
                    try!(member.pretty_print(printer));
                    try!(write!(printer, "\n"));
                    try!(printer.write_indent());
                }
                printer.inside_iface = false;
                write!(printer, "}}")
            },
            Decl::Trait(ref name, ref members) => {
                try!(write!(printer, "trait {} ", name));
                try!(write!(printer, "{{"));
                for member in members {
                    try!(member.pretty_print(printer));
                    try!(write!(printer, "\n"));
                    try!(printer.write_indent());
                }
                write!(printer, "}}")
            },
            Decl::StaticVars(ref vars) => {
                try!(write!(printer, "static "));
                for (i, var) in vars.iter().enumerate() {
                    try!(write_comma_separator(printer, i));
                    if let Expr::None = var.1 {
                        try!(write!(printer, "${}", var.0));
                    } else {
                        try!(write!(printer, "${} = ", var.0));
                        try!(var.1.pretty_print(printer));
                    }
                }
                write!(printer, ";")
            }
        }
    }
}

impl<'a, W: Write> PrettyPrint<W> for FunctionDecl<'a> {
    fn pretty_print(&self, printer: &mut PrettyPrinter<W>) -> fmt::Result {
        try!(write!(printer, "("));
        for (i, param) in self.params.iter().enumerate() {
            try!(write_comma_separator(printer, i));
            try!(param.pretty_print(printer));
        }
        try!(write!(printer, ")"));
        if self.usev.len() > 0 {
            try!(write!(printer, " use ("));
            for (i, usev) in self.usev.iter().enumerate() {
                try!(write_comma_separator(printer, i));
                try!(write!(printer, "${}", usev));
            }
            try!(write!(printer, ") "));
        }
        if printer.inside_iface {
            assert_eq!(self.body.len(), 0);
            return write!(printer, ";");
        }

        try!(write!(printer, "{{"));
        let bak = printer.nested_level;
        printer.nested_level = 0;
        for item in &self.body {
            try!(item.pretty_print(printer));
        }
        printer.nested_level = bak;
        write!(printer, "}}")
    }
}

impl<'a, W: Write> PrettyPrint<W> for ParamDefinition<'a> {
    fn pretty_print(&self, printer: &mut PrettyPrinter<W>) -> fmt::Result {
        match self.ty {
            None => (),
            Some(ref ty) => try!(write!(printer, "{} ", ty)),
        }
        let as_ref = match self.as_ref {
            true => "&",
            false => "",
        };
        try!(write!(printer, "{}${}", as_ref, self.name));
        match self.default {
            Expr::None => Ok(()),
            ref def => {
                try!(write!(printer, " = "));
                def.pretty_print(printer)
            },
        }
    }
}

impl<'a, W: Write> PrettyPrint<W> for ClassDecl<'a> {
    fn pretty_print(&self, printer: &mut PrettyPrinter<W>) -> fmt::Result {
        let cmod = match self.cmod {
            ClassModifier::None => "",
            ClassModifier::Abstract => "abstract ",
            ClassModifier::Final => "final ",
        };
        try!(write!(printer, "{}class {}", cmod, self.name));
        if let Some(ref base_class) = self.base_class {
            try!(write!(printer, " extends {}", base_class));
        }
        if self.implements.len() > 0 {
            try!(write!(printer, " implements "));
            for (i, iface) in self.implements.iter().enumerate() {
                try!(write_comma_separator(printer, i));
                try!(write!(printer, "{}", iface));
            }
        }
        try!(write!(printer, "{{"));
        for member in &self.members {
            try!(member.pretty_print(printer));
            try!(write!(printer, "\n"));
            try!(printer.write_indent());
        }
        write!(printer, "}}")
    }
}

impl<'a, W: Write> PrettyPrint<W> for ClassMember<'a> {
    fn pretty_print(&self, printer: &mut PrettyPrinter<W>) -> fmt::Result {
        match *self {
            ClassMember::Constant(ref name, ref value) => {
                try!(write!(printer, "const {}=", name));
                try!(value.pretty_print(printer));
                write!(printer, ";")
            },
            ClassMember::Property(ref modifiers, ref name, ref default) => {
                assert!(!printer.inside_iface);
                try!(write!(printer, "{} ${}", modifiers, name));
                match *default {
                    Expr::None => (),
                    _ => {
                        try!(write!(printer, " = "));
                        try!(default.pretty_print(printer))
                    }
                }
                write!(printer, ";")
            },
            ClassMember::Method(ref modifiers, ref name, ref fdecl) => {
                let is_abstract = match modifiers.2 {
                    ClassModifier::Abstract => true,
                    _ => false,
                };
                if is_abstract {
                    // This assert would be: "Access type for interface method XXX must be omitted", sanity check
                    assert!(!printer.inside_iface);
                    printer.inside_iface = true;
                }
                try!(write!(printer, "{} function {} ", modifiers, name));
                try!(fdecl.pretty_print(printer));
                if is_abstract {
                    printer.inside_iface = false;
                }
                Ok(())
            },
            ClassMember::TraitUse(ref path, ref clauses) => {
                // This assert would be a fatal error in PHP: "Fatal error: Cannot use traits inside of interfaces"
                // sanity check if what we parsed and try to print makes sense
                assert!(!printer.inside_iface);

                try!(write!(printer, "use "));
                for (i, t) in path.iter().enumerate() {
                    try!(write_comma_separator(printer, i));
                    try!(write!(printer, "{}", t));
                }
                if clauses.len() == 0 {
                    return write!(printer, ";");
                }
                try!(write!(printer, "{{"));
                for clause in clauses {
                    try!(write!(printer, "{}", clause));
                }
                write!(printer, "}}")
            },
        }
    }
}

// TODO: maybe reeuse more where a macro takes a format string or similar?
#[inline]
fn write_comma_separator<W: Write>(writer: &mut W, i: usize) -> fmt::Result {
    if i > 0 {
        try!(write!(writer, ", "));
    }
    Ok(())
}

#[inline]
fn write_args<W: Write>(printer: &mut PrettyPrinter<W>, args: &[Expr]) -> fmt::Result {
    for (i, arg) in args.iter().enumerate() {
        try!(write_comma_separator(printer, i));
        try!(arg.pretty_print(printer));
    }
    Ok(())
}

impl<'a> fmt::Display for Ty<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str_ = match *self {
            Ty::Array => "array",
            Ty::Callable => "callable",
            Ty::Bool => "bool",
            Ty::Double => "double",
            Ty::Float => "float",
            Ty::Int => "int",
            Ty::String => "string",
            Ty::Object(ref p) => {
                match *p {
                    Some(ref path) => try!(write!(f, "{}", path)),
                    None => try!(write!(f, "object")),
                }
                ""
            }
        };
        write!(f, "{}", str_)
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Op::Add => "+",
            Op::Sub => "-",
            Op::Concat => ".",
            Op::Mul => "*",
            Op::Div => "/",
            Op::Pow => "**",
            Op::Mod => "%",
            Op::Or => "||",
            Op::And => "&&",
            Op::Identical => "===",
            Op::NotIdentical => "!==",
            Op::Eq => "==",
            Op::Neq => "!=",
            Op::Uneq => "<>",
            Op::Lt => "<",
            Op::Gt => ">",
            Op::Le => "<=",
            Op::Ge => ">=",
            Op::Spaceship => "<=>",
            Op::BitwiseAnd => "&",
            Op::BitwiseInclOr => "|",
            Op::BitwiseExclOr => "^",
            Op::Instanceof => "instanceof",
        })
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

impl Visibility {
    fn to_static_string(&self) -> &'static str {
        match *self {
            Visibility::None => "",
            Visibility::Public => "public",
            Visibility::Protected => "protected",
            Visibility::Private => "private",
        }
    }
}

impl fmt::Display for Modifiers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut modifiers = vec![];
        if self.0 {
            modifiers.push("static");
        }
        let vis = self.1.to_static_string();
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

impl<'a> fmt::Display for TraitUse<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TraitUse::InsteadOf(ref atm, ref paths) => {
                try!(write!(f, "{}::{} insteadof ", atm.0, atm.1));
                for (i, path) in paths.iter().enumerate() {
                    try!(write_comma_separator(f, i));
                    try!(write!(f, "{}", path));
                }
                write!(f, ";")
            },
            TraitUse::As(ref atm, ref visibility, ref path) => {
                try!(write!(f, "{}::{} as {}", atm.0, atm.1, visibility.to_static_string()));
                match *path {
                    None => write!(f, ";"),
                    Some(ref path) => write!(f, " {};", path)
                }
            },
        }
    }
}
