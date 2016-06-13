use pest::prelude::*;
use std::collections::LinkedList;
use std::num::ParseIntError;
use std::string::FromUtf8Error;
use super::{Expr, Op, Decl, FunctionDecl};

#[derive(Debug)]
pub enum ParseError {
    Utf8(FromUtf8Error),
    ParseInt(ParseIntError),
}

impl From<FromUtf8Error> for ParseError {
    fn from(err: FromUtf8Error) -> ParseError {
        ParseError::Utf8(err)
    }
}

impl From<ParseIntError> for ParseError {
    fn from(err: ParseIntError) -> ParseError {
        ParseError::ParseInt(err)
    }
}

#[derive(Debug)]
enum IdxExpr<'a> {
    ArrayIdx(Expr<'a>),
    Call(Vec<Expr<'a>>),
    ObjProperty(Expr<'a>),
    StaticProperty(Expr<'a>),
}

impl_rdp! {
    grammar! {
        // a decl is the biggest unit within a file, TOOD: maybe rename into Item?
        decls           =  _{ decl* }
        decl            =  { func | block }
        func            =  { ["function"] ~ identifier ~ ["("] ~ func_param* ~ func_params_end ~ stmt_body }
        block           =  { stmt+ }
        // statement
        stmt            =  { ((stmt_construct | stmt_func | assignment_stmt | op_expr) ~ [";"]) | stmt_construct }
        // A stmt which is optionally terminated by a semicolon; most language constructs with {}
        stmt_construct  = _{ if_stmt | while_stmt | do_while_stmt | foreach_stmt }
        stmt_func       = _{ stmt_echo | stmt_return | stmt_return_n }
        stmt_echo       =  { ["echo"] ~ op_expr }
        stmt_return     =  { ["return"] ~ op_expr }
        stmt_return_n   =  { ["return"] }
        assignment_stmt =  { idxing_expr ~ ["="] ~ op_expr }
        if_stmt         =  { ["if"] ~ ["("] ~ op_expr ~ [")"] ~ (stmt_body | stmt) ~ if_else_stmt? }
        if_else_stmt    =  { ["else"] ~ (stmt_body | stmt) }
        while_stmt      =  { ["while"] ~ ["("] ~ op_expr ~ [")"] ~ (stmt_body | stmt) }
        do_while_stmt   =  { ["do"] ~ stmt_body ~ ["while"] ~ ["("] ~ op_expr ~ [")"] }
        stmt_body       =  { ["{"] ~ stmt* ~ stmt_body_end }
        stmt_body_end   =  { ["}"] }
        foreach_stmt    =  { ["foreach"] ~ ["("] ~ op_expr ~ ["as"] ~ ((variable ~ foreach_sep ~ variable) | variable) ~ [")"] ~ (stmt_body | stmt) }
        foreach_sep     =  { ["=>"] }
        // post/prefix expr (these are not stackable e.g ($c++)++ and $c++++ does not work)
        unary_dec       =  { ["--"] }
        unary_inc       =  { ["++"] }
        unary_p         =  { unary_dec | unary_inc }
        expr            =  { (unary_p ~ idxing_expr) | (idxing_expr ~ unary_p) | idxing_expr }
        idxing_expr     =  { (nexpr ~ (array_idx | call | property | static_property)+) | nexpr }
        // a simpler version of an expression, to prevent some infinite-recursion
        nexpr           =  { null | false_ | true_ | number | string | variable | closure | identifier | sub_expr }
        // anonymous functions:
        closure         =  { ["function"] ~ ["("] ~ func_param* ~ func_params_end ~ stmt_body }
        // TODO: return types & ...
        func_param      =  { variable }
        func_params_end  =  { [")"] }
        sub_expr        =  { ["{"] ~ op_expr ~ ["}"] }
        // scalars/literals
        null            =  { ["null"] }
        false_          =  { ["false"] }
        true_           =  { ["true"] }
        string          = @{ normalstring | charstring }
        character       =  { any }
        identifier      = @{ (['a'..'z'] | ['A'..'Z'] | ["_"]) ~ (['a'..'z'] | ['A'..'Z'] | ["_"] | ['0'..'9'])* }
        variable        =  { (["$"] ~ identifier) }
        normalstring    =  { ["\""] ~ (escape_sequence | !(["\""] | ["\\"]) ~ character)* ~ ["\""] }
        charstring      =  { ["'"] ~ (escape_sequence | !(["'"] | ["\\"]) ~ character)* ~ ["'"] }
        escape_sequence =  {
            ["\\"] ~ (
                (["n"] | ["r"] | ["t"] | ["\""] | ["'"] | ["\\"]) |
                (["\r"]? ~ ["\n"])
            ) |
            decimal_escape |
            hex_escape+
        }
        decimal_escape  =  {
            ["\\"] ~ (
                ['0'..'2'] ~ digit ~ digit |
                digit ~ digit |
                digit
            )
        }
        hex_escape      =  { ["\\"] ~ (["x"] | ["X"]) ~ hex_digit ~ hex_digit }
        digit           = _{ ['0'..'9'] }
        hex_digit       = _{ ['0'..'9'] | ['a'..'f'] | ['A'..'F'] }
        number          = @{ hex | float | int }
        int             =  { digits }
        hex             =  { ["0"] ~ (["x"] | ["X"]) ~ hex_digits }
        float           =  {
            digits ~ ["."] ~ digits? ~ exponent? |
            ["."] ~ digits ~ exponent? |
            digits ~ exponent
        }
        exponent        =  { (["e"] | ["E"]) ~ (["+"] | ["-"])? ~ digits }
        digits          =  { digit+ }
        hex_digits      =  { hex_digit+ }

        // array
        array_idx       =  { ["["] ~ expr ~ array_idx_end }
        array_idx_end   =  { ["]"] }
        // call
        call            =  { ["("] ~ call_arg* ~ call_end }
        call_end        =  { [")"] }
        call_arg        =  { op_expr ~ [","]? }
        // object-property and static properties
        property        = { ["->"] ~ nexpr } //nexpr?
        static_property = { ["::"] ~ nexpr }

        // unary/binary operators
        _power          = _{
            { (["("] ~ op_expr ~ [")"]) | expr }
            power = {< op_power }
        }
        _unary          = _{ unary | _power }
        unary           =  { op_unary ~ _unary }
        op_expr         =  {
            { _unary }
            or          = {  op_or }
            and         = {  op_and }
            comparison  = {  op_comparison }
            add_sub     = {  op_add | op_sub }
            mul_div_mod = {  op_mul | op_div | op_mod }
        }
        op_or           =  { ["or"] | ["||"] }
        op_and          =  { ["and"] | ["&&"] }
        op_le           =  { ["<="] }
        op_ge           =  { [">="] }
        op_lt           =  { ["<"] }
        op_gt           =  { [">"] }
        op_eq           =  { ["=="] }
        op_identical    =  { ["==="] }
        op_comparison   = _{ op_le | op_ge | op_lt | op_gt | op_eq | op_identical }
        op_add          =  { ["+"] }
        op_sub          =  { ["-"] }
        op_mul          =  { ["*"] }
        op_div          =  { ["/"] }
        op_mod          =  { ["%"] }
        op_not          =  { (["not"] | ["!"]) }
        op_negate       =  { ["!"] }
        op_unary        = _{ op_not | op_negate }
        op_power        =  { ["**"] }

        verbose_comment = { comment }
        comment = _{
            (["//"] ~ (!(["\r"] | ["\n"]) ~ any)* ~ (["\n"] | ["\r\n"] | ["\r"] | eoi)) |
            (["/*"] ~ (!(["*/"]) ~ any)* ~ ["*/"])
        }

        whitespace = _{ [" "] | ["\t"] | ["\u{000C}"] | ["\r"] | ["\n"] }
    }

    process! {
        // convert linked list -> vec
        main(&self) -> Result<Vec<Decl<'n>>, ParseError> {
            (ret: main_ll()) => Ok(try!(ret).into_iter().collect())
        }

        // main parser (linked list)
        main_ll(&self) -> Result<LinkedList<Decl<'n>>, ParseError> {
            (_: decl, _: func, &name: identifier, params: _func_params(), _: func_params_end, body: _stmt_or_body(), next: main_ll()) => {
                let mut next = try!(next);
                next.push_front(Decl::GlobalFunction(name.into(), FunctionDecl {
                    params: try!(params).into_iter().collect(),
                    body: try!(body)
                }));
                Ok(next)
            },
            (_: decl, _: block, stmts: _stmts(), next: main_ll()) => {
                let mut next = try!(next);
                next.push_front(Decl::Block(try!(stmts).into_iter().collect()));
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        // parse a single statement (preceded by stmt token)
        _single_stmt(&self) -> Result<Expr<'n>, ParseError> {
            (_: stmt, stm: _stmt()) => stm
        }

        // inner stmt (not preceded by stmt token)
        _stmt(&self) -> Result<Expr<'n>, ParseError> {
            // if-else
            (_: if_stmt, cond: _op_expr(), body: _stmt_or_body(), _: if_else_stmt, else_body: _stmt_or_body()) => {
                Ok(Expr::If(Box::new(try!(cond)), try!(body).into_iter().collect(), Some(try!(else_body).into_iter().collect())))
            },
            // if
            (_: if_stmt, cond: _op_expr(), body: _stmt_or_body()) => {
                Ok(Expr::If(Box::new(try!(cond)), try!(body).into_iter().collect(), None))
            },
            // while
            (_: while_stmt, cond: _op_expr(), body: _stmt_or_body()) => {
                Ok(Expr::While(Box::new(try!(cond)), try!(body).into_iter().collect()))
            },
            (_: do_while_stmt, body: _stmt_or_body(), cond: _op_expr()) => {
                Ok(Expr::DoWhile(try!(body).into_iter().collect(), Box::new(try!(cond))))
            },
            (_: assignment_stmt, left: _idxing_expr(), right: _op_expr()) => {
                Ok(Expr::Assign(Box::new(try!(left)), Box::new(try!(right))))
            },
            // foreach (key, value)
            (_: foreach_stmt, arg: _op_expr(), _: variable, &k: identifier, _: foreach_sep, _: variable, &v: identifier, body: _stmt_or_body()) => {
                Ok(Expr::ForEach(Box::new(try!(arg)), Some(k.into()), Some(v.into()), try!(body).into_iter().collect()))
            },
            // foreach (value only)
            (_: foreach_stmt, arg: _op_expr(), _: variable, &v: identifier, body: _stmt_or_body()) => {
                Ok(Expr::ForEach(Box::new(try!(arg)), None, Some(v.into()), try!(body).into_iter().collect()))
            },
            (_: stmt_echo, expr: _op_expr()) => {
                Ok(Expr::Echo(Box::new(try!(expr))))
            },
            // return with return value
            (_: stmt_return, expr: _op_expr()) => {
                Ok(Expr::Return(Box::new(try!(expr))))
            },
            // return without return value
            (_: stmt_return_n) => Ok(Expr::Return(Box::new(Expr::None))),
            (_: op_expr, expr: _op_expr()) => expr
        }

        // parse a single_stmt or a body ("{}") containing statements
        _stmt_or_body(&self) -> Result<Vec<Expr<'n>>, ParseError> {
            (_: stmt_body, body: _stmts(), _: stmt_body_end) => {
                Ok(try!(body).into_iter().collect())
            },
            (stmt: _single_stmt()) => {
                Ok(vec![try!(stmt)])
            }
        }

        // parse multiple stmts
        _stmts(&self) -> Result<LinkedList<Expr<'n>>, ParseError> {
            (_: stmt, st: _stmt(), next: _stmts()) => {
                let mut next = try!(next);
                next.push_front(try!(st));
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        // parse an expression (highest-priority) containing operators, ...
        _op_expr(&self) -> Result<Expr<'n>, ParseError> {
            // unary !
            (_: unary, _: op_not, operand: _op_expr()) => {
                Ok(Expr::UnaryOp(Op::Not, Box::new(try!(operand))))
            },
            // + -
            (_: add_sub, left: _op_expr(), sign, right: _op_expr()) => {
                Ok(Expr::BinaryOp(match sign.rule {
                    Rule::op_add => Op::Add,
                    Rule::op_sub => Op::Sub,
                    _ => unreachable!()
                }, Box::new(try!(left)), Box::new(try!(right))))
            },
            // * / %
            (_: mul_div_mod, left: _op_expr(), sign, right: _op_expr()) => {
                Ok(Expr::BinaryOp(match sign.rule {
                    Rule::op_mul => Op::Mul,
                    Rule::op_div => Op::Div,
                    Rule::op_mod => Op::Mod,
                    _ => unreachable!()
                }, Box::new(try!(left)), Box::new(try!(right))))
            },
            // **
            (_: power, left: _op_expr(), _: op_power, right: _op_expr()) => {
                Ok(Expr::BinaryOp(Op::Pow, Box::new(try!(left)), Box::new(try!(right))))
            },
            // ||
            (_: or, left: _op_expr(), _: op_or, right: _op_expr()) => {
                Ok(Expr::BinaryOp(Op::Or, Box::new(try!(left)), Box::new(try!(right))))
            },
            // &&
            (_: and, left: _op_expr(), _: op_and, right: _op_expr()) => {
                Ok(Expr::BinaryOp(Op::And, Box::new(try!(left)), Box::new(try!(right))))
            },
            (_: op_expr, y: _op_expr()) => y,
            (expr: _expr()) => expr, //TODO: required?
        }

        // parse an expression, which may be preceded/followed by pre/post unary operators (2. highest-priority)
        _expr(&self) -> Result<Expr<'n>, ParseError> {
            //--e ++e
            (_: expr, _: unary_p, sign, e: _idxing_expr()) => {
                Ok(Expr::UnaryOp(match sign.rule {
                    Rule::unary_dec => Op::PreDec,
                    Rule::unary_inc => Op::PreInc,
                    _ => unreachable!()
                }, Box::new(try!(e))))
            },
            //e-- e++
            (_: expr, e: _idxing_expr(), _: unary_p, sign) => {
                Ok(Expr::UnaryOp(match sign.rule {
                    Rule::unary_dec => Op::PostDec,
                    Rule::unary_inc => Op::PostInc,
                    _ => unreachable!()
                }, Box::new(try!(e))))
            },
            //e
            (_: expr, e: _idxing_expr()) => e
        }

        // parse an expression, which may be followed by "indexing" (array, call, obj, static obj, ...)
        _idxing_expr(&self) -> Result<Expr<'n>, ParseError> {
            (_: idxing_expr, idx_expr: _nexpr(), idxs: _idxs()) => {
                // fold the given expressions (constructing proper call/indexing expressions)
                let expr = try!(idxs).into_iter().fold(try!(idx_expr), |initial, elem| {
                    match (initial, elem) {
                        (Expr::ArrayIdx(e, mut elems), IdxExpr::ArrayIdx(ai)) => {
                            elems.push(ai);
                            Expr::ArrayIdx(e, elems)
                        },
                        (Expr::ObjProperty(e, mut elems), IdxExpr::ObjProperty(objp)) => {
                            elems.push(objp);
                            Expr::ObjProperty(e, elems)
                        },
                        (Expr::StaticProperty(e, mut elems), IdxExpr::StaticProperty(stp)) => {
                            elems.push(stp);
                            Expr::StaticProperty(e, elems)
                        },
                        (a, IdxExpr::Call(args)) => {
                            Expr::Call(Box::new(a), args)
                        },
                        (a, IdxExpr::ArrayIdx(idx)) => {
                            Expr::ArrayIdx(Box::new(a), vec![idx])
                        }
                        (a, IdxExpr::ObjProperty(objp)) => {
                            Expr::ObjProperty(Box::new(a), vec![objp])
                        }
                        (a, IdxExpr::StaticProperty(stp)) => {
                            Expr::StaticProperty(Box::new(a), vec![stp])
                        }
                    }
                });
                Ok(expr)
            },
            (_: idxing_expr, expr: _nexpr()) => {
                Ok(try!(expr))
            }
        }

        // extract call args
        _call_args(&self) -> Result<LinkedList<Expr<'n>>, ParseError> {
            (_: call_arg, e: _op_expr(), next: _call_args()) => {
                let mut next = try!(next);
                let expr = try!(e);
                next.push_front(expr);
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        // general indexing operation for anything behaving like an array_idx (call, property-fetch, ...)
        _idxs(&self) -> Result<LinkedList<IdxExpr<'n>>, ParseError> {
            (_: array_idx, e: _expr(), _: array_idx_end, next: _idxs()) => {
                let mut next = try!(next);
                next.push_front(IdxExpr::ArrayIdx(try!(e)));
                Ok(next)
            },
            (_: call, args: _call_args(), _: call_end, next: _idxs()) => {
                let mut next = try!(next);
                next.push_front(IdxExpr::Call(try!(args).into_iter().collect()));
                Ok(next)
            },
            (_: property, e: _nexpr(), next: _idxs()) => {
                let mut next = try!(next);
                next.push_front(IdxExpr::ObjProperty(try!(e)));
                Ok(next)
            },
            (_: static_property, e: _nexpr(), next: _idxs()) => {
                let mut next = try!(next);
                next.push_front(IdxExpr::StaticProperty(try!(e)));
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        // parse a simple expr (lowest priority), this nearly only contains scalar values
        _nexpr(&self) -> Result<Expr<'n>, ParseError> {
            (_: nexpr, _: true_) => Ok(Expr::True),
            (_: nexpr, _: false_) => Ok(Expr::False),
            (_: nexpr, _: null) => Ok(Expr::Null),
            (_: nexpr, _: number, &i: int, _: digits) => {
                Ok(Expr::Int(try!(i.parse())))
            },
            (_: nexpr, _: variable, &id: identifier) => {
                Ok(Expr::Variable(id.into()))
            },
            (_: nexpr, &id: identifier) => {
                Ok(Expr::Identifier(id.into()))
            },
            (_: nexpr, _: string, _: charstring, str_: _charstr()) => {
                Ok(Expr::String(try!(str_)))
            },
            (_: nexpr, _: string, _: normalstring, str_: _str()) => {
                Ok(Expr::String(try!(str_)))
            },
            (_: nexpr, _: closure, params: _func_params(), _: func_params_end, body: _stmt_or_body()) => {
                Ok(Expr::Function(FunctionDecl { params: try!(params).into_iter().collect(), body: try!(body) }))
            }
        }

        // extract (meta) function parameters, this will also contain return type hints & more in the future
        _func_params(&self) -> Result<LinkedList<Expr<'n>>, ParseError> {
            (_: func_param, _: variable, &id: identifier, next: _func_params()) => {
                let mut next = try!(next);
                next.push_front(Expr::Identifier(id.into()));
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }

        // normal string, handle the escaping & concatenation of all fragments
        _str(&self) -> Result<String, ParseError> {
            // handle a "normal" character
            (&ch: character, next: _str()) => {
                Ok(ch.to_owned() + &try!(next))
            },
            // handle a hex-escape sequence
            (_: escape_sequence, &current: hex_escape, others: _hex_escapes(), next: _str()) => {
                let mut others = try!(others);
                let curr = u8::from_str_radix(&current[2..], 16).unwrap();
                others.push_front(curr);
                Ok(try!(String::from_utf8(others.into_iter().collect())) + &try!(next))
            },
            // handle a decimal-escape sequence
            (_: escape_sequence, &seq: decimal_escape, next: _str()) => {
                Ok(try!(String::from_utf8(vec![try!(u8::from_str_radix(&seq[1..], 8))])) + &try!(next))
            },
            // handle an escape-sequence
            (&es: escape_sequence, next: _str()) => {
                Ok(match es {
                    "\\n" => "\n",
                    "\\r" => "\r",
                    "\\t" => "\t",
                    "\\\"" => "\"",
                    "\\\'" => "\'",
                    "\\\\" => "\\",
                    _ => unreachable!()
                }.to_owned() + &try!(next))
            },
            () => Ok(String::new())
        }

        // a singled-quoted string, ignoring most of escaping, raw-literal like
        _charstr(&self) -> Result<String, ParseError> {
            (&ch: character, next: _charstr()) => {
                Ok(ch.to_owned() + &try!(next))
            },
            (&es: escape_sequence, next: _charstr()) => {
                Ok(match es {
                    "\\\'" => "'",
                    "\\\\" => "\\",
                    es => es,
                }.to_owned() + &try!(next))
            },
            () => Ok(String::new())
        }

        // handle the collection of bytes to later convert UTF8-bytes from hex-escapes into a string
        _hex_escapes(&self) -> Result<LinkedList<u8>, ParseError> {
            (&current: hex_escape, next: _hex_escapes()) => {
                let mut next = try!(next);
                let curr = try!(u8::from_str_radix(&current[2..], 16));
                next.push_front(curr);
                Ok(next)
            },
            () => Ok(LinkedList::new())
        }
    }
}

// HELPERS; TODO: move to tests files when visibility is added for these macros

pub fn process_stmt(input: &str) -> Expr {
    let mut parser = Rdp::new(StringInput::new(input));
    assert!(parser.stmt());
    println!("{:?}", parser.queue());
    assert!(parser.end());
    parser._single_stmt().unwrap()
}

pub fn process_expr(input: &str) -> Expr {
    let mut parser = Rdp::new(StringInput::new(input));
    assert!(parser.op_expr());
    println!("{:?}", parser.queue());
    assert!(parser.end());
    parser._op_expr().unwrap()
}
