///! a pretty-ish printer
use std::fmt::{self, Write};
use std::borrow::Borrow;
use tokens::Token;
use ast::{Block, Const, ClassModifiers, ClassModifier, Decl, FunctionDecl, Stmt, Stmt_, Expr, Expr_, IncludeTy, Op, Path, UnaryOp, Ty, TraitUse, UseClause};
use ast::{Member, MemberModifiers, MemberModifier};

pub struct PrettyPrinter<W: Write> {
    indentation: usize,
    target: W,
}

impl<W: Write> PrettyPrinter<W> {
    pub fn new(target: W) -> PrettyPrinter<W> {
        PrettyPrinter {
            indentation: 0,
            target: target,
        }
    }

    pub fn print_statements(target: W, stmts: Vec<Stmt>) -> fmt::Result {
        let mut printer = PrettyPrinter::new(target);
        for stmt in stmts {
            try!(printer.print_statement(&stmt));
        }
        Ok(())
    }

    fn writeln(&mut self, text: &str) -> fmt::Result {
        try!(self.write_indented(text));
        write!(self.target, "\n")
    }

    fn write_indented(&mut self, text: &str) -> fmt::Result {
        for _ in 0..self.indentation {
            try!(write!(self.target, "    "));
        }
        write!(self.target, "{}", text)
    }

    fn write(&mut self, text: &str) -> fmt::Result {
        write!(self.target, "{}", text)
    }

    fn print_argument_list(&mut self, args: &[Expr]) -> fmt::Result {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                try!(self.write(", "));
            }
            try!(self.print_expression(arg));
        }
        Ok(())
    }

    fn print_member_body(&mut self, members: &[Member]) -> fmt::Result {
        try!(self.write(" {\n"));
        self.indentation += 1;
        for member in members {
            try!(self.print_member(member));
            try!(self.write("\n"));
        }
        self.indentation -= 1;
        self.write_indented("}")
    }

    fn print_decl(&mut self, decl: &Decl) -> fmt::Result {
        match *decl {
            Decl::Namespace(ref path) => {
                try!(self.write_indented("namespace "));
                try!(write!(self.target, "{}", path));
                self.write(";\n")
            },
            Decl::GlobalFunction(ref name, ref decl) => {
                try!(self.write_indented(""));
                self.print_function(decl, Some(name.borrow()))
            },
            Decl::Class(ref classdecl) => {
                try!(self.write_indented(""));
                try!(write!(self.target, "{}", classdecl.cmod));
                try!(self.write("class "));
                try!(self.write(classdecl.name.borrow()));
                try!(self.write(" "));
                if let Some(ref base_class) = classdecl.base_class {
                    try!(self.write("extends "));
                    try!(write!(self.target, "{} ", base_class));
                }
                if !classdecl.implements.is_empty() {
                    try!(self.write("implements "));
                    for (i, iface) in classdecl.implements.iter().enumerate() {
                        if i > 0 {
                            try!(self.write(", "));
                        }
                        try!(write!(self.target, "{}", iface));
                    }
                }
                self.print_member_body(&classdecl.members)
            },
            Decl::Interface(ref name, ref implements, ref members) => {
                try!(self.write_indented("interface "));
                try!(self.write(name.borrow()));
                if !implements.is_empty() {
                    try!(self.write(" extends "));
                    for (i, iface) in implements.iter().enumerate() {
                        if i > 0 {
                            try!(self.write(", "));
                        }
                        try!(write!(self.target, "{}", iface));
                    }
                }
                self.print_member_body(members)
            },
            Decl::Trait(ref name, ref members) => {
                try!(self.write_indented("trait "));
                try!(self.write(name.borrow()));
                self.print_member_body(members)
            },
            Decl::StaticVars(ref vars) => {
                try!(self.write_indented("static "));
                for (i, &(ref varname, ref value)) in vars.iter().enumerate() {
                    if i > 0 {
                        try!(self.write(", "));
                    }
                    try!(self.write("$"));
                    try!(self.write(varname.borrow()));
                    if let Some(ref value) = *value {
                        try!(self.write("="));
                        try!(self.print_expression(value));
                    }
                }
                self.write(";\n")
            },
        }
    }

    fn print_member(&mut self, member: &Member) -> fmt::Result {
        try!(self.write_indented(""));
        match *member {
            Member::Constant(ref modifiers, ref name, ref value) => {
                try!(write!(self.target, "{} const ", modifiers));
                try!(self.write(name.borrow()));
                try!(self.write("="));
                try!(self.print_expression(value));
                self.write(";")
            },
            Member::Property(ref modifiers, ref name, ref value) => {
                try!(write!(self.target, "{} $", modifiers));
                try!(self.write(name.borrow()));
                if let Some(ref default) = *value {
                    try!(self.write("="));
                    try!(self.print_expression(default));
                }
                self.write(";")
            },
            Member::Method(ref modifiers, ref name, ref decl) => {
                try!(write!(self.target, "{} ", modifiers));
                self.print_function(decl, Some(name.borrow()))
            },
            Member::TraitUse(ref names, ref uses) => {
                try!(self.write("use "));
                for (i, name) in names.iter().enumerate() {
                    if i > 0 {
                        try!(self.write(", "));
                    }
                    try!(write!(self.target, "{}", name));
                }
                if !uses.is_empty() {
                    try!(self.write("{\n"));
                    self.indentation += 1;
                    for use_ in uses {
                        try!(self.write_indented(""));
                        match *use_ {
                            TraitUse::As(ref path, ref method, ref modifiers, ref alias) => {
                                if let Some(ref path) = *path {
                                    try!(write!(self.target, "{}::", path));
                                }
                                try!(self.write(method.borrow()));
                                try!(self.write(" as "));
                                try!(write!(self.target, "{} ", modifiers));
                                if let Some(ref alias) = *alias {
                                    try!(self.write(alias.borrow()));
                                }
                            },
                            TraitUse::InsteadOf(ref path, ref method, ref names) => {
                                try!(write!(self.target, "{}::", path));
                                try!(self.write(method.borrow()));
                                try!(self.write(" insteadof "));
                                for (i, name) in names.iter().enumerate() {
                                    if i > 0 {
                                        try!(self.write(", "));
                                    }
                                    try!(write!(self.target, "{}", name));
                                }
                            },
                        }
                        try!(self.write(";\n"));
                    }
                    self.indentation -= 1;
                    try!(self.write_indented("}\n"));
                } else {
                    try!(self.write(";\n"));
                }
                Ok(())
            }
        }
    }

    fn print_block(&mut self, block: &Block) -> fmt::Result {
        try!(self.write("{\n"));
        self.indentation += 1;
        for stmt in &block.0 {
            try!(self.print_statement(stmt));
        }
        self.indentation -= 1;
        self.write_indented("}\n")
    }

    fn print_function(&mut self, func: &FunctionDecl, name: Option<&str>) -> fmt::Result {
        try!(self.write("function "));
        if func.ret_ref {
            try!(self.write("&"));
        }
        if let Some(name) = name {
            try!(self.write(name));
        }
        try!(self.write("("));
        for (i, param) in func.params.iter().enumerate() {
            if i > 0 {
                try!(self.write(","));
            }
            if let Some(ref ty) = param.ty {
                try!(write!(self.target, "{} ", ty));
            }
            if param.as_ref {
                try!(self.write("&"));
            }
            try!(self.write("$"));
            try!(self.write(param.name.borrow()));
            if let Some(ref default) = param.default {
                try!(self.write("="));
                try!(self.print_expression(default));
            }
        }
        try!(self.write(") "));
        if !func.usev.is_empty() {
            try!(self.write("use ("));
            for (i, &(ref by_ref, ref var)) in func.usev.iter().enumerate() {
                if i > 0 {
                    try!(self.write(", "));
                }
                if *by_ref {
                    try!(self.write("&"));
                }
                try!(self.write("$"));
                try!(self.write(var.borrow()));
            }
            try!(self.write(") "));
        }
        if let Some(ref body) = func.body {
            self.print_block(body)
        } else {
            self.write(";")
        }
    }

    fn print_use(&mut self, clauses: &[UseClause]) -> fmt::Result {
        for clause in clauses {
            try!(self.write_indented("use "));
            match *clause {
                UseClause::QualifiedName(ref path, ref alias) => {
                    try!(write!(self.target, "{}", path));
                    if let Some(ref alias) = *alias {
                        try!(self.write(" as "));
                        try!(self.write(alias.borrow()));
                    }
                }
            }
            try!(self.write(";\n"));
        }
        Ok(())
    }

    fn print_statement(&mut self, stmt: &Stmt) -> fmt::Result {
        match stmt.0 {
            Stmt_::Block(ref block) => {
                try!(self.writeln("{\n"));
                self.indentation += 1;
                for bstmt in &block.0 {
                    try!(self.print_statement(bstmt));
                }
                self.indentation -= 1;
                self.writeln("}\n")
            },
            Stmt_::Decl(ref decl) => self.print_decl(decl),
            Stmt_::Use(ref clauses) => self.print_use(clauses),
            Stmt_::Expr(ref expr) => {
                try!(self.write_indented(""));
                try!(self.print_expression(expr));
                self.write(";\n")
            },
            Stmt_::Echo(ref args) => {
                try!(self.write_indented("echo "));
                try!(self.print_argument_list(args));
                self.write(";\n")
            },
            Stmt_::Return(ref arg) | Stmt_::Break(ref arg) | Stmt_::Continue(ref arg) => {
                try!(self.write_indented(match stmt.0 {
                    Stmt_::Return(_) => "return",
                    Stmt_::Break(_) => "break",
                    Stmt_::Continue(_) => "continue",
                    _ => unreachable!(),
                }));
                if let Some(ref arg) = *arg {
                    try!(self.write(" "));
                    try!(self.print_expression(arg))
                }
                self.write(";\n")
            },
            Stmt_::Unset(ref args) => {
                try!(self.write_indented("unset("));
                try!(self.print_argument_list(args));
                self.write(");\n")
            },
            Stmt_::If(ref cond, ref bl, ref else_bl) => {
                try!(self.write_indented("if ("));
                try!(self.print_expression(cond));
                try!(self.write(") "));
                try!(self.print_block(bl));
                if !else_bl.is_empty() {
                    try!(self.write_indented("else "));
                    try!(self.print_block(else_bl));
                }
                Ok(())
            },
            Stmt_::While(ref cond, ref bl) => {
                try!(self.write_indented("while ("));
                try!(self.print_expression(cond));
                try!(self.write(") "));
                self.print_block(bl)
            },
            Stmt_::DoWhile(ref bl, ref cond) => {
                try!(self.write_indented("do ("));
                try!(self.print_block(bl));
                try!(self.write("while ("));
                try!(self.print_expression(cond));
                self.write(");\n")
            },
            Stmt_::For(ref init, ref looper, ref cond, ref bl) => {
                try!(self.write_indented("for ("));
                try!(self.print_opt_expression(&init.as_ref().map(|x| &**x)));
                try!(self.write("; "));
                try!(self.print_opt_expression(&looper.as_ref().map(|x| &**x)));
                try!(self.write(";"));
                try!(self.print_opt_expression(&cond.as_ref().map(|x| &**x)));
                try!(self.write(") "));
                self.print_block(bl)
            },
            Stmt_::ForEach(ref base, ref k, ref v, ref bl) => {
                try!(self.write_indented("foreach ("));
                try!(self.print_expression(base));
                try!(self.write(" as "));
                if let Some(ref k) = *k {
                    try!(self.print_expression(k));
                    try!(self.write(" => "));
                }
                try!(self.print_expression(v));
                try!(self.write(") "));
                self.print_block(bl)
            },
            Stmt_::Try(ref bl, ref catch, ref finally) => {
                try!(self.write_indented("try "));
                try!(self.print_block(bl));
                for clause in catch {
                    try!(self.write("catch ("));
                    try!(write!(self.target, "{} ${}) ", clause.ty, clause.var.borrow() as &str));
                    try!(self.print_block(&clause.block));
                }
                if let Some(ref finally_bl) = *finally {
                    try!(self.write("finally "));
                    try!(self.print_block(finally_bl));
                }
                Ok(())
            },
            Stmt_::Throw(ref expr) => {
                try!(self.write_indented("throw "));
                try!(self.print_expression(expr));
                self.write(";\n")
            },
            Stmt_::Switch(ref base, ref cases) => {
                try!(self.write_indented("switch ("));
                try!(self.print_expression(base));
                try!(self.write(") {\n"));
                self.indentation += 1;
                for case in cases {
                    for cond in &case.conds {
                        try!(self.write_indented("case "));
                        try!(self.print_expression(cond));
                        try!(self.write(":\n"));
                    }
                    if case.default {
                        try!(self.write("default:\n"));
                    }
                    self.indentation += 1;
                    for stmt in &case.block.0 {
                        try!(self.print_statement(stmt));
                    }
                    self.indentation -= 1;
                }
                self.indentation -= 1;
                self.write("}\n")
            }
        }
    }

    fn print_opt_expression(&mut self, expr: &Option<&Expr>) -> fmt::Result {
        match *expr {
            None => Ok(()),
            Some(ref expr) => self.print_expression(expr),
        }
    }

    fn print_expression_curly_parens(&mut self, expr: &Expr, curly: bool) -> fmt::Result {
        let (parens_open, parens_close) = if curly { ("{", "}") } else { ("(", ")") };

        let wrap_in_parens = match expr.0 {
            Expr_::BinaryOp(_, _, _) | Expr_::UnaryOp(_, _)
            | Expr_::ArrayIdx(_, _) | Expr_::ObjMember(_, _) | Expr_::StaticMember(_, _) | Expr_::Call(_, _)
            | Expr_::New(_, _) | Expr_::Assign(_, _)
            | Expr_::TernaryIf(_, _, _) => true,
            _ => false,
        };

        if wrap_in_parens {
            try!(self.write(parens_open));
        }
        try!(self.print_expression(expr));
        if wrap_in_parens {
            try!(self.write(parens_close));
        }
        Ok(())
    }

    fn print_expression_parens(&mut self, expr: &Expr) -> fmt::Result {
        self.print_expression_curly_parens(expr, false)
    }

    pub fn print_expression(&mut self, expr: &Expr) -> fmt::Result {
        match expr.0 {
            Expr_::Path(ref path) => write!(self.target, "{}", path),
            Expr_::String(ref str_) => {
                let out_str = (str_.borrow() as &str).replace("\\'", "\\\\'").replace('\'', "\\'");
                try!(write!(self.target, "'{}", out_str));
                if out_str.ends_with('\\') {
                    try!(self.write("\\"));
                }
                self.write("'")
            },
            Expr_::BinaryString(ref str_) => unimplemented!(),
            Expr_::Int(ref i) => write!(self.target, "{}", i),
            Expr_::Double(ref d) => write!(self.target, "{}", d),
            Expr_::Array(ref arr) => {
                try!(self.write("["));
                for (i, &(ref k, ref v)) in arr.iter().enumerate() {
                    if i > 0 {
                        try!(self.write(", "));
                    }
                    if let Some(ref k) = *k {
                        try!(self.print_expression(k));
                        try!(self.write(" => "));
                    }
                    try!(self.print_expression(v));
                }
                self.write("]")
            }
            Expr_::Constant(ref const_) => write!(self.target, "{}", const_),
            Expr_::Variable(ref varname) => write!(self.target, "${}", varname.borrow() as &str),
            Expr_::FetchVariable(ref varexpr) => {
                try!(self.write("$"));
                self.print_expression(varexpr)
            },
            Expr_::Reference(ref ref_expr) => {
                try!(self.write("&"));
                self.print_expression(ref_expr)
            },
            Expr_::Isset(ref args) => {
                try!(self.write("isset("));
                try!(self.print_argument_list(args));
                self.write(")")
            },
            Expr_::Empty(ref arg) => {
                try!(self.write("empty("));
                try!(self.print_expression(arg));
                self.write(")")
            },
            Expr_::Exit(ref arg) => {
                try!(self.write("exit("));
                try!(self.print_opt_expression(&arg.as_ref().map(|x| &**x)));
                self.write(")")
            },
            Expr_::Clone(ref arg) => {
                try!(self.write("clone "));
                self.print_expression(arg)
            },
            Expr_::Include(ref ty, ref arg) => {
                try!(self.write(match *ty {
                    IncludeTy::Require => "require",
                    IncludeTy::RequireOnce => "require_once",
                    IncludeTy::Include => "include",
                    IncludeTy::IncludeOnce => "include_once",
                }));
                try!(self.write(" "));
                self.print_expression(arg)
            },
            Expr_::ArrayIdx(ref base, ref idxs) => {
                try!(self.print_expression_parens(base));
                for idx in idxs {
                    try!(self.write("["));
                    try!(self.print_opt_expression(&idx.as_ref()));
                    try!(self.write("]"));
                }
                Ok(())
            },
            Expr_::ObjMember(ref base, ref idxs) => {
                try!(self.print_expression_parens(base));
                for idx in idxs {
                    try!(self.write("->"));
                    try!(self.print_expression_curly_parens(idx, true));
                }
                Ok(())
            },
            Expr_::StaticMember(ref base, ref idxs) => {
                try!(self.print_expression_parens(base));
                for idx in idxs {
                    try!(self.write("::"));
                    try!(self.print_expression_parens(idx));
                }
                Ok(())
            },
            Expr_::Call(ref target, ref args) => {
                try!(self.print_expression(target));
                try!(self.write("("));
                try!(self.print_argument_list(args));
                self.write(")")
            },
            Expr_::New(ref target, ref args) => {
                try!(self.write("new "));
                try!(self.print_expression(target));
                try!(self.write("("));
                try!(self.print_argument_list(args));
                self.write(")")
            },
            Expr_::UnaryOp(ref operator, ref operand) => {
                let (op, can_have_parens) = match *operator {
                    UnaryOp::Positive => ("+", true),
                    UnaryOp::Negative => ("-", true),
                    UnaryOp::Not => ("!", true),
                    UnaryOp::PreInc => ("++", false),
                    UnaryOp::PreDec => ("--", false),
                    UnaryOp::PostInc | UnaryOp::PostDec => ("", false),
                    UnaryOp::BitwiseNot => ("~", true),
                    UnaryOp::SilenceErrors => ("@", true),
                };
                try!(self.write(op));
                if can_have_parens {
                    try!(self.print_expression_parens(operand));
                } else {
                    try!(self.print_expression(operand));
                }
                match *operator {
                    UnaryOp::PostInc => try!(self.write("++")),
                    UnaryOp::PostDec => try!(self.write("--")),
                    _ => ()
                }
                Ok(())
            },
            Expr_::BinaryOp(ref operator, ref op1, ref op2) => {
                try!(self.print_expression_parens(op1));
                try!(write!(self.target, "{}", operator));
                self.print_expression_parens(op2)
            },
            Expr_::InstanceOf(ref op1, ref op2) => {
                try!(self.print_expression(op1));
                try!(self.write(" instanceof "));
                self.print_expression(op2)
            },
            Expr_::Cast(ref ty, ref op) => {
                try!(self.write("("));
                try!(self.write(match *ty {
                    Ty::Array => "array",
                    Ty::Callable => "callable",
                    Ty::Bool => "bool",
                    Ty::Float => "float",
                    Ty::Int => "int",
                    Ty::Double => "double",
                    Ty::String => "string",
                    Ty::Object(None) => "object",
                    _ => unimplemented!(),
                }));
                try!(self.write(")("));
                try!(self.print_expression(op));
                self.write(")")
            },
            Expr_::Yield(ref expr) => {
                try!(self.write("yield "));
                self.print_opt_expression(&expr.as_ref().map(|x| &**x))
            },
            Expr_::Function(ref decl) => self.print_function(decl, None),
            Expr_::Assign(ref target, ref value) => {
                try!(self.print_expression(target));
                try!(self.write("="));
                self.print_expression(value)
            },
            Expr_::CompoundAssign(ref target, ref op, ref value) => {
                try!(self.print_expression(target));
                try!(write!(self.target, "{}=", op));
                self.print_expression(value)
            },
            Expr_::AssignRef(ref target, ref value) => {
                try!(self.print_expression(target));
                try!(self.write("=&"));
                self.print_expression(value)
            },
            Expr_::List(ref parts) => {
                try!(self.write("list("));
                for (i, &(ref key, ref value)) in parts.iter().enumerate() {
                    if i > 0 {
                        try!(self.write(", "));
                    }
                    if let Some(ref key) = *key {
                        try!(self.print_expression(key));
                        try!(self.write(" => "));
                    }
                    try!(self.print_expression(value));
                }
                self.write(")")
            },
            Expr_::TernaryIf(ref base, ref case_true, ref case_else) => {
                try!(self.print_expression_parens(base));
                try!(self.write("?"));
                if let Some(ref expr_) = *case_true {
                    try!(self.print_expression_parens(&**expr_));
                }
                try!(self.write(":"));
                self.print_expression_parens(case_else)
            }
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Op::Concat => ".",
            Op::Add => "+",
            Op::Sub => "-",
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
            Op::Lt => "<",
            Op::Gt => ">",
            Op::Le => "<=",
            Op::Ge => ">=",
            Op::BitwiseAnd => "&",
            Op::BitwiseInclOr => "|",
            Op::BitwiseExclOr => "^",
            Op::Spaceship => "<=>",
            Op::Sl => "<<",
            Op::Sr => ">>",
        })
    }
}

impl fmt::Display for ClassModifiers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.has(ClassModifier::Abstract) {
            try!(write!(f, "abstract "));
        }
        if self.has(ClassModifier::Final) {
            try!(write!(f, "final "));
        }
        Ok(())
    }
}

impl fmt::Display for MemberModifiers {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.has(MemberModifier::Public) {
            try!(write!(f, "public "));
        }
        if self.has(MemberModifier::Protected) {
            try!(write!(f, "protected "));
        }
        if self.has(MemberModifier::Private) {
            try!(write!(f, "private"));
        }
        if self.has(MemberModifier::Static) {
            try!(write!(f, "static"));
        }
        if self.has(MemberModifier::Abstract) {
            try!(write!(f, "abstract "));
        }
        if self.has(MemberModifier::Final) {
            try!(write!(f, "final "));
        }
        Ok(())
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_absolute {
            try!(write!(f, "\\"));
        }
        if let Some(ref namespace) = self.namespace {
            try!(write!(f, "{}\\", namespace.borrow() as &str));
        }
        write!(f, "{}", self.identifier.borrow() as &str)
    }
}


impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ty = match *self {
            Ty::Array => "array",
            Ty::Callable => "callable",
            Ty::Bool => "bool",
            Ty::Float => "float",
            Ty::Int => "int",
            Ty::Double => "double",
            Ty::String => "string",
            Ty::Object(None) => "object",
            Ty::Object(Some(ref path)) => {
                try!(write!(f, "{}", path));
                return Ok(());
            }
        };
        write!(f, "{}", ty)
    }
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            Const::Null => "null",
            Const::True => "true",
            Const::False => "false",
            Const::MagicClass => Token::MagicClass.repr(),
            Const::MagicTrait => Token::MagicTrait.repr(),
            Const::MagicFunction => Token::MagicFunction.repr(),
            Const::MagicMethod => Token::MagicMethod.repr(),
            Const::MagicLine => Token::MagicLine.repr(),
            Const::MagicFile => Token::MagicFile.repr(),
            Const::MagicDir => Token::MagicDir.repr(),
            Const::MagicNamespace => Token::MagicNamespace.repr(),
        })
    }
}
