///! recursive descedant parser
///! using pratt-parser techniques described in [1] and [2]
///!
///! grammar based on [3]
///!
///! [1] http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
///! [2] http://effbot.org/zone/simple-top-down-parsing.htm
///! [3] https://github.com/php/php-src/blob/ab304579ff046426f281e9a95abea8d611e38e1c/Zend/zend_language_parser.y

use std::mem;
use std::borrow::Borrow;
use tokenizer::{Tokenizer, Token, TokenSpan};
use interner::Interner;
pub use tokenizer::{Span, SyntaxError};
pub use ast::{Expr, Expr_, UnaryOp, Op, Path};

pub struct Parser {
    interner: Interner,
    tokens: Vec<TokenSpan>,
}

enum Associativity {
    Left,
    Right
}

#[derive(Copy, Clone)]
enum Precedence {
    None = 0,
    LogicalIncOr1 = 1,
    LogicalAnd = 2,
    BitwiseIncOr = 3,
    BitwiseExcOr = 4,
    BitwiseAnd = 5,
    Equality = 6,
    Relational = 7,
    Shift = 8,
    Add = 9,
    Mul = 10,
    Pow = 11,
    Unary = 12,
}

macro_rules! from_usize {
    ($($arg:ident),*) => {
        impl Precedence {
            fn from_usize(p: usize) -> Precedence {
                match p {
                    $(
                    x if x == (Precedence::$arg as usize) => Precedence::$arg,
                    )*
                    _ => unreachable!()
                }
            }
        }
    };
}
from_usize!(None, LogicalIncOr1, LogicalAnd, BitwiseIncOr, Shift, Add, Mul, Pow, Unary);

impl Token {
    fn precedence(&self) -> Precedence {
        match *self {
            Token::BoolOr => Precedence::LogicalIncOr1,
            Token::BoolAnd => Precedence::LogicalAnd,
            Token::BwOr => Precedence::BitwiseIncOr,
            Token::BwXor => Precedence::BitwiseExcOr,
            Token::Ampersand => Precedence::BitwiseAnd,
            Token::IsIdentical | Token::IsNotIdentical | Token::IsEqual | Token::IsNotEqual => Precedence::Equality,
            Token::SpaceShip | Token::Lt | Token::Gt | Token::IsSmallerOrEqual | Token::IsGreaterOrEqual => Precedence::Relational,
            Token::Sl | Token::Sr => Precedence::Shift,
            Token::Plus | Token::Minus | Token::Dot => Precedence::Add,
            Token::Mul | Token::Div | Token::Mod => Precedence::Mul,
            Token::Pow => Precedence::Pow,
            _ => unimplemented!(),
        }
    }

    fn associativity(&self) -> Associativity {
        match *self {
            Token::BoolOr | Token::BoolAnd => Associativity::Left,
            Token::BwOr | Token::BwXor => Associativity::Left,
            Token::Ampersand =>  Associativity::Left,
            Token::IsIdentical | Token::IsNotIdentical | Token::IsEqual | Token::IsNotEqual => Associativity::Left,
            Token::SpaceShip | Token::Lt | Token::Gt | Token::IsSmallerOrEqual | Token::IsGreaterOrEqual =>  Associativity::Left,
            Token::Sl | Token::Sr => Associativity::Left,
            Token::Plus | Token::Minus | Token::Dot => Associativity::Left,
            Token::Mul | Token::Div | Token::Mod => Associativity::Left,
            Token::Pow =>  Associativity::Right,
            _ => unimplemented!(),
        }
    }
}

// return if Ok() else continue in code flow (to e.g. try the next parser in the "chain")
macro_rules! alt {
    ($e:expr) => (match $e {
        Ok(x) => return Ok(x),
        _ => (),
    })
}

// check if the next token is X, if return and execute block
macro_rules! if_lookahead {
    ($self_:expr, $a:pat, $v:ident, $block:expr, $else_block:expr) => {
        if let Some(&TokenSpan($a, _)) = $self_.tokens.last() {
            let $v = $self_.tokens.pop().unwrap();
            $block
        } else {
            $else_block
        }
    };
    ($self_:expr, $a:pat, $v:ident, $block:expr) => {if_lookahead!($self_, $a, $v, $block, {})};
}

impl Parser {
    fn parse_unary_expression(&mut self, precedence: Precedence) -> Result<Expr, ()> {
        let left = match self.tokens.pop() {
            Some(x) => x,
            None => return Err(()),
        };
        let mut left = match left.0 {
            t@Token::Plus | t@Token::Minus | t@Token::BwNot | t@Token::BoolNot => {
                let op = match t {
                    Token::Plus => UnaryOp::Positive,
                    Token::Minus => UnaryOp::Negative,
                    Token::BwNot => UnaryOp::BitwiseNot,
                    Token::BoolNot => UnaryOp::Not,
                    _ => unreachable!(),
                };
                Expr(Expr_::UnaryOp(op, Box::new(try!(self.parse_expression(Precedence::Unary)))), left.1)
            },
            _ => {
                self.tokens.push(left);
                try!(self.parse_other_expression())
            }
        };
        // handle other binary expressions
        loop {
            match self.parse_binary_expression(&mut left, precedence.clone()) {
                Ok(true) => (),
                Ok(false) | Err(_) => break,
            };
        }
        Ok(left)
    }

    fn parse_binary_expression(&mut self, left: &mut Expr, precedence: Precedence) -> Result<bool, ()> {
        // lookahead to check for binary expression
        let binary_op = {
            match self.tokens.last() {
                Some(x) => match x.0 {
                    Token::BoolOr => Some(Op::Or),
                    Token::BoolAnd => Some(Op::And),
                    Token::BwOr => Some(Op::BitwiseInclOr),
                    Token::BwXor => Some(Op::BitwiseExclOr),
                    Token::Ampersand => Some(Op::BitwiseAnd),
                    Token::IsIdentical => Some(Op::Identical),
                    Token::IsNotIdentical => Some(Op::NotIdentical),
                    Token::IsEqual => Some(Op::Neq),
                    Token::IsNotEqual => Some(Op::Eq),
                    Token::SpaceShip => Some(Op::Spaceship),
                    Token::Lt => Some(Op::Lt),
                    Token::Gt => Some(Op::Gt),
                    Token::IsSmallerOrEqual => Some(Op::Le),
                    Token::IsGreaterOrEqual => Some(Op::Ge),
                    Token::Sl => Some(Op::Sl),
                    Token::Sr => Some(Op::Sr),
                    Token::Plus => Some(Op::Add),
                    Token::Minus => Some(Op::Sub),
                    Token::Dot => Some(Op::Concat),
                    Token::Mul => Some(Op::Mul),
                    Token::Div => Some(Op::Div),
                    Token::Mod => Some(Op::Mod),
                    Token::Pow => Some(Op::Pow),
                    _ => None,
                }.map(|y| (x.0.precedence(), y)),
                None => None,
            }
        };
        // no binary expression found, done
        let (precedence, binary_op) = match binary_op {
            Some(x) => if (precedence as usize) < (x.0 as usize) { x } else {
                return Ok(false);
            },
            None => return Ok(false),
        };
        // consume te binary-operator token
        let binary_token = self.tokens.pop().unwrap();
        let precedence = match binary_token.0.associativity() {
            Associativity::Right => Precedence::from_usize((precedence as usize) - 1),
            Associativity::Left => precedence,
        };
        let right = try!(self.parse_expression(precedence));
        let tmp = Box::new(mem::replace(left, Expr(Expr_::None, Span::new())));
        *left = Expr(Expr_::BinaryOp(binary_op, tmp, Box::new(right)), binary_token.1);
        Ok(true)
    }

    fn parse_simple_variable(&mut self) -> Result<Expr, ()> {
        // TODO '$' '{' expr '}'
        // TODO '$' simple_variable
        if_lookahead!(self, Token::Dollar, _token, unimplemented!());
        // T_VARIABLE
        if_lookahead!(self, Token::Variable(_), _token, Ok(match _token {
            TokenSpan(Token::Variable(varname), span) => Expr(Expr_::Variable(varname.into()), span),
            _ => unreachable!(),
        }), {
            println!("err simple_var");
            Err(())
        })
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ()> {
        if_lookahead!(self, Token::ParenthesesOpen, _token, {
            if_lookahead!(self, Token::ParenthesesClose, _token, {
                return Ok(vec![]);
            });
            // parse arguments (non_empty_argument_list)
            let mut args = vec![];
            loop {
                args.push(try!(self.parse_expression(Precedence::None)));
                if_lookahead!(self, Token::Comma, _tok, {}, break);
            }

            if_lookahead!(self, Token::ParenthesesClose, _token, {
                return Ok(args);
            }, {
                return Err(());
            });
        });
        Err(())
    }

    fn parse_function_call(&mut self) -> Result<Expr, ()> {
        let name_expr = try!(self.parse_name());
        let args = try!(self.parse_argument_list());
        Ok(Expr(Expr_::Call(Box::new(name_expr), args), Span::new()))
    }

    fn parse_callable_variable(&mut self) -> Result<Expr, ()> {
        let var_expr = match self.parse_simple_variable() {
            Err(x) => try!(self.parse_function_call()),
            Ok(x) => x,
        };
        // ('[' optional_expr ']')+
        let mut arr_members = vec![];
        loop {
            if_lookahead!(self, Token::SquareBracketOpen, _tok, match self.parse_opt_expression(Precedence::None) {
                Err(x) => return Err(x),
                Ok(expr) => if_lookahead!(self, Token::SquareBracketClose, _tok, arr_members.push(expr), return Err(())),
            }, break);
        }
        if !arr_members.is_empty() {
            return Ok(Expr(Expr_::ArrayIdx(Box::new(var_expr), arr_members), Span::new()));
        }
        Ok(var_expr)
    }

    fn parse_property_name(&mut self) -> Result<Expr, ()> {
        alt!(self.parse_simple_variable());
        if_lookahead!(self, Token::String(_), token, {
            return Ok(match token.0 {
                Token::String(str_) => Expr(Expr_::Path(Path::Identifier(str_.into())), token.1),
                _ => unreachable!(),
            })
        });
        if_lookahead!(self, Token::CurlyBracesOpen, _tok, {
            match self.parse_expression(Precedence::None) {
                Err(x) => return Err(x),
                Ok(expr) => if_lookahead!(self, Token::CurlyBracesClose, _tok, return Ok(expr), return Err(())),
            }
        });
        Err(())
    }

    fn parse_dereferencable(&mut self, parsed_variable: Expr) -> Result<Expr, ()> {
        let var = match parsed_variable.0 {
            Expr_::None => try!(self.parse_variable()),
            _ => parsed_variable,
        };
        // dereferencable T_OBJECT_OPERATOR property_name = (T_OBJECT_OPERATOR)+
        let mut obj_members = vec![];
        loop {
            if_lookahead!(self, Token::ObjectOp, token, match self.parse_property_name() {
                Err(x) => return Err(x),
                Ok(p) => obj_members.push(p),
            }, break);
        }
        if !obj_members.is_empty() {
            return Ok(Expr(Expr_::ObjMember(Box::new(var), obj_members), Span::new()));
        }
        Ok(var)
    }

    fn parse_variable(&mut self) -> Result<Expr, ()> {
        // To eliminate the left recursion we need to "inline" the last 2 rules of dereferencable
        // This works by passing the parsed callable variable into parse_dereferencable
        // so that it "knows" to stop the left-recursion there
        match self.parse_callable_variable() {
            Err(x) => return Err(x),
            Ok(x) => return self.parse_dereferencable(x),
        }
    }

    fn parse_expression(&mut self, prec: Precedence) -> Result<Expr, ()> {
        // expr_without_variable TODO
        alt!(self.parse_unary_expression(prec));
        println!("err parse_expr");
        Err(())
    }

    fn parse_opt_expression(&mut self, prec: Precedence) -> Result<Expr, ()> {
        match self.parse_expression(prec) {
            Err(()) => return Ok(Expr(Expr_::None, Span::new())),
            x => x,
        }
    }

    /// parsing all expressions after the precedence applying ("callback")
    fn parse_other_expression(&mut self) -> Result<Expr, ()> {
        // '(' expr ')'
        if_lookahead!(self, Token::ParenthesesOpen, _token, {
            let expr_ret =  self.parse_expression(Precedence::None);
            if let Err(x) = expr_ret {
                return Err(x)
            }
            if_lookahead!(self, Token::ParenthesesClose, _token2, {
                return expr_ret;
            });
            return Err(())
        });
        alt!(self.parse_scalar());
        match self.parse_variable() {
            Ok(var) => {
                // variable '=' expr
                // variable '=' '&' variable
                // and all variable T_<OP>_ASSIGNs
                let assign_type = match self.tokens.last() {
                    Some(&TokenSpan(ref x, _)) => match *x {
                        Token::Equal => Some(Op::Eq),
                        Token::PlusEqual => Some(Op::Add),
                        Token::MinusEqual => Some(Op::Sub),
                        Token::MulEqual => Some(Op::Mul),
                        Token::PowEqual => Some(Op::Pow),
                        Token::DivEqual => Some(Op::Div),
                        Token::ConcatEqual => Some(Op::Concat),
                        Token::ModEqual => Some(Op::Mod),
                        Token::AndEqual => Some(Op::And),
                        Token::XorEqual => Some(Op::BitwiseExclOr),
                        Token::SlEqual => Some(Op::Sl),
                        Token::SrEqual => Some(Op::Sr),
                        _ => None,
                    },
                    None => None,
                };
                if let Some(assign_type) = assign_type {
                    let span = match self.tokens.pop() {
                        Some(TokenSpan(_, span)) => span,
                        _ => unreachable!()
                    };
                    let by_ref = match (&assign_type, self.tokens.last()) {
                        (&Op::Eq, Some(&TokenSpan(Token::Ampersand, _))) => {
                            self.tokens.pop();
                            true
                        },
                        _ => false,
                    };

                    return match self.parse_expression(Precedence::None) {
                        Ok(expr) => {
                            let span = Span { start: span.start, end: expr.1.end, ..Span::new() };
                            let expr = match (assign_type, by_ref) {
                                (Op::Eq, false) => Expr_::Assign(Box::new(var), Box::new(expr)),
                                (Op::Eq, true) => Expr_::AssignRef(Box::new(var), Box::new(expr)),
                                (op, _) => Expr_::CompoundAssign(Box::new(var), op, Box::new(expr)),
                            };
                            Ok(Expr(expr, span))
                        },
                        x => x,
                    }
                }
                return Ok(var);
            },
            _ => (),
        };
        // TODO: move me here alt!(self.parse_scalar());
        println!("err other_expr");
        Err(())
    }

    fn parse_namespace_name(&mut self) -> Result<Expr, ()> {
        // T_STRING ~ (NS_SEPARATOR ~ T_STRING)+
        let mut fragments = vec![];
        while !self.tokens.is_empty() {
            if_lookahead!(self, Token::String(_), token, {
                match token {
                    TokenSpan(Token::String(str_), span) => fragments.push((str_, span)),
                    _ => unreachable!(),
                }
                if let Some(&TokenSpan(Token::NsSeparator, _)) = self.tokens.last() {
                    // lookahead to ensure it's followed by string
                    if let Some(&TokenSpan(Token::String(_), _)) = self.tokens.iter().nth(self.tokens.len()-2) {
                        self.tokens.pop(); // pop NS_SEPARATOR
                        continue;
                    }
                }
                let span = Span { start: fragments.first().map(|x| x.1.start).unwrap(), end: fragments.last().map(|x| x.1.end).unwrap(), ..Span::new() };
                return Ok(Expr(Expr_::Path(match fragments.len() {
                    0 => unreachable!(),
                    1 => Path::Identifier(fragments.pop().map(|x| x.0.into()).unwrap()),
                    _ => {
                        let identifier = fragments.pop().unwrap();
                        Path::NsIdentifier(self.interner.intern(&fragments.into_iter().enumerate().fold(String::new(), |acc, (i, el)| {
                            acc + if i > 0 { "\\" } else { "" } + el.0.borrow()
                        })), identifier.0.into())
                    }
                }), span));
            });
            break;
        }
        Err(())
    }

    fn parse_name(&mut self) -> Result<Expr, ()> {
        //TODO: |   T_NAMESPACE T_NS_SEPARATOR namespace_name   { $$ = $3; $$->attr = ZEND_NAME_RELATIVE; }
        // try to consume the \\ if one exists so that a namespace_name will be matched
        // then the path will be a fully quallified (FQ)
        let fq = if_lookahead!(self, Token::NsSeparator, _token, { self.tokens.pop(); true }, false);;
        match self.parse_namespace_name() {
            // TODO: inject FQDN as flag or something?
            Ok(x) => Ok(x),
            x => x,
        }
    }

    fn parse_constant(&mut self) -> Result<Expr, ()> {
        //TODO
        self.parse_name()
    }

    fn parse_encaps_list(&mut self) -> Result<Expr, ()> {
        let mut str_ = String::new();
        let mut start_pos = None;
        let mut end_pos = 0;
        // find string literals
        loop {
            if_lookahead!(self, Token::ConstantEncapsedString(_), token, {
                match token.0 {
                    Token::ConstantEncapsedString(str_part) => {
                        if start_pos.is_none() {
                            start_pos = Some(token.1.start);
                        }
                        if end_pos < token.1.end {
                            end_pos = token.1.end;
                        }
                        str_.push_str(str_part.borrow());
                    },
                    _ => unreachable!(),
                }
            }, { break });
        }
        if str_.len() > 0 {
            return Ok(Expr(Expr_::String(self.interner.intern(&str_)), Span { start: start_pos.unwrap(), end: end_pos, ..Span::new() }));
        }
        Err(())
    }

    fn parse_dereferencable_scalar(&mut self) -> Result<Expr, ()> {
        if_lookahead!(self, Token::ConstantEncapsedString(_), token, {
            match token.0 {
                Token::ConstantEncapsedString(str_) => return Ok(Expr(Expr_::String(str_), token.1)),
                _ => unreachable!(),
            }
        });
        Err(())
    }

    fn parse_scalar(&mut self) -> Result<Expr, ()> {
        match self.tokens.pop() {
            Some(x) => Ok(Expr(match x.0 {
                // LNUMBER
                Token::Int(x) => Expr_::Int(x),
                // DNUMBER
                Token::Double(x) => Expr_::Double(x),
                // several magic constants
                Token::MagicLine => unimplemented!(),
                Token::MagicFile => unimplemented!(),
                Token::MagicDir => unimplemented!(),
                Token::MagicTrait => unimplemented!(),
                Token::MagicMethod => unimplemented!(),
                Token::MagicFunction => unimplemented!(),
                Token::MagicClass => unimplemented!(),
                // '"' encaps_list '"'     { $$ = $2; }
                Token::DoubleQuote => {
                    let ret = try!(self.parse_encaps_list());
                    if_lookahead!(self, Token::DoubleQuote, _token, {}, return Err(()));
                    return Ok(ret);
                },
                // TODO  |   T_START_HEREDOC encaps_list T_END_HEREDOC { $$ = $2; }
                _ => {
                    self.tokens.push(x);
                    alt!(self.parse_dereferencable_scalar());
                    return self.parse_constant();
                }
            }, x.1)),
            None => Err(())
        }
    }

    fn parse_statement(&mut self) -> Result<Expr, ()> {
        // expr ';'
        let expr = try!(self.parse_unary_expression(Precedence::None));
        if_lookahead!(self, Token::SemiColon, _token, { return Ok(expr) });
        Err(())
    }

    fn parse_top_statement(&mut self) -> Result<Expr, ()> {
        // TODO: incomplete
        return self.parse_statement();
    }

    fn parse_tokens(interner: Interner, toks: Vec<TokenSpan>) -> Vec<Expr> {
        // strip whitespace and unnecessary tokens
        let mut tokens: Vec<TokenSpan> = vec![];
        for tok in toks.into_iter().rev() {
            match tok.0 {
                // TODO: pass doc comment in the span on (don't ignore them)
                Token::Comment(_) | Token::InlineHtml(_) | Token::OpenTag => (),
                Token::OpenTagWithEcho => tokens.push(TokenSpan(Token::Echo, tok.1)),
                _ => tokens.push(tok),
            }
        }
        println!("{:?}", tokens);
        let mut p = Parser {
            tokens: tokens,
            interner: interner,
        };
        // error handling..
        let mut exprs = vec![];
        match p.parse_top_statement() {
            Ok(x) => exprs.push(x),
            Err(x) => println!("parse_tokens: err: {:?}", x),
        }
        exprs
    }

    pub fn parse_str(s: &str) -> Vec<Expr> {
        let (interner, tokens) = {
            let mut tokenizer = Tokenizer::new(s);
            let mut tokens = vec![];
            // TODO: error & state handling
            while let Ok(tok) = tokenizer.next_token() {
                tokens.push(tok);
            }
            (tokenizer.into_interner(), tokens)
        };
        Parser::parse_tokens(interner, tokens)
    }
}
