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
pub use tokenizer::{Span, SyntaxError, mk_span};
pub use ast::{Expr, Expr_, IncludeTy, UnaryOp, Op, Path, Ty};

pub struct Parser {
    interner: Interner,
    tokens: Vec<TokenSpan>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<TokenSpan>, interner: Interner) -> Parser {
        Parser {
            tokens: tokens,
            interner: interner,
            pos: 0,
        }
    }

    #[inline]
    fn advance(&mut self, n: isize) {
        self.pos = (self.pos as isize + n as isize) as usize;
    }

    #[inline]
    fn next_token(&mut self) -> Option<&TokenSpan> {
        self.tokens.get(self.pos)
    }
}

enum Associativity {
    Left,
    Right
}

#[derive(Copy, Clone)]
enum Precedence {
    None,
    /// e.g. ternary
    Conditional,
    LogicalIncOr1,
    LogicalAnd,
    BitwiseIncOr,
    BitwiseExcOr,
    BitwiseAnd,
    Equality,
    Relational,
    Shift,
    Add,
    Mul,
    Pow,
    InstanceOf,
    Unary,
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
from_usize!(None, Conditional, LogicalIncOr1, LogicalAnd, BitwiseIncOr, BitwiseExcOr, BitwiseAnd, Equality, Relational, Shift, Add, Mul, Pow, Unary);

impl Token {
    fn precedence(&self) -> Option<Precedence> {
        Some(match *self {
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
            Token::QuestionMark => Precedence::Conditional,
            Token::InstanceOf => Precedence::InstanceOf,
            _ => return None,
        })
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

macro_rules! alt_not {
    ($e:expr, $next:expr) => (match $e {
        None => $next,
        Some(x) => Some(x),
    })
}

// check if the next token is X, if return and execute block
macro_rules! if_lookahead {
    ($self_:expr, $a:pat, $v:ident, $block:expr, $else_block:expr) => {
        if let Some(&TokenSpan($a, _)) = $self_.next_token() {
            let $v = $self_.next_token().unwrap().clone();
            $self_.advance(1);
            $block
        } else {
            $else_block
        }
    };
    ($self_:expr, $a:pat, $v:ident, $block:expr) => {if_lookahead!($self_, $a, $v, $block, {})};
}

impl Parser {
    fn parse_unary_expression(&mut self, precedence: Precedence) -> Result<Expr, ()> {
        let left = match self.next_token() {
            Some(x) => x.clone(),
            None => return Err(()),
        };
        self.advance(1);
        let mut left = match left.0 {
            Token::Plus | Token::Minus | Token::BwNot | Token::BoolNot | Token::Silence | Token::Increment | Token::Decrement => {
                let op = match left.0 {
                    Token::Plus => UnaryOp::Positive,
                    Token::Minus => UnaryOp::Negative,
                    Token::BwNot => UnaryOp::BitwiseNot,
                    Token::BoolNot => UnaryOp::Not,
                    Token::Silence => UnaryOp::SilenceErrors,
                    Token::Increment => UnaryOp::PreInc,
                    Token::Decrement => UnaryOp::PreDec,
                    _ => unreachable!(),
                };
                Expr(Expr_::UnaryOp(op, Box::new(try!(self.parse_expression(Precedence::Unary)))), left.1)
            },
            _ => {
                self.advance(-1);
                try!(self.parse_postfix_expression())
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
        let (new_precedence, binary_op) = {
            match self.next_token() {
                Some(x) => (x.0.precedence(), match x.0 {
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
                }),
                None => (None, None),
            }
        };
        // no expression found, done
        let new_precedence = match new_precedence {
            None => return Ok(false),
            Some(x) => x,
        };
        if (precedence as usize) >= (new_precedence as usize) {
            // nothing of the required precedence we can handle
            return Ok(false);
        }

        // consume the operator token
        let op_token = self.next_token().unwrap().clone();
        self.advance(1);

        // also try to match the ternary here.. since it's PHP and it's left associative therefor
        if let Token::QuestionMark = op_token.0 {
            let expr_ternary_if = try!(self.parse_opt_expression(new_precedence));
            if_lookahead!(self, Token::Colon, _token, {}, { return Err(()); });
            let expr_ternary_else = try!(self.parse_expression(new_precedence));
            let tmp = Box::new(mem::replace(left, Expr(Expr_::Break(None), Span::new())));
            *left = Expr(Expr_::TernaryIf(tmp, expr_ternary_if.map(|x| Box::new(x)), Box::new(expr_ternary_else)), Span::new());
            return Ok(true);
        }

        // also try to match instanceof (non associative!)
        if let Token::InstanceOf = op_token.0 {
            if let Expr(Expr_::InstanceOf(_, _), _) = *left {
                // TODO: throw an error due to the non-associative nature
                unreachable!();
            }
            let right = try!(self.parse_class_name_reference());
            let tmp = Box::new(mem::replace(left, Expr(Expr_::Break(None), Span::new())));
            *left = Expr(Expr_::InstanceOf(tmp, Box::new(right)), op_token.1);
            return Ok(true);
        }

        // handle regular binary-op expression
        let binary_op = binary_op.unwrap();

        let new_precedence = match op_token.0.associativity() {
            Associativity::Right => Precedence::from_usize((new_precedence as usize) - 1),
            Associativity::Left => new_precedence,
        };
        let right = try!(self.parse_expression(new_precedence));
        // TODO: using Break(0) is a hack here (and above), get rid of the mem::replace somehow
        let tmp = Box::new(mem::replace(left, Expr(Expr_::Break(None), Span::new())));
        *left = Expr(Expr_::BinaryOp(binary_op, tmp, Box::new(right)), op_token.1);
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

    #[inline]
    fn parse_expression_list(&mut self) -> Result<Vec<Expr>, ()> {
        let mut args = vec![];
        loop {
            args.push(try!(self.parse_expression(Precedence::None)));
            if_lookahead!(self, Token::Comma, _tok, {}, break);
        }
        Ok(args)
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ()> {
        if_lookahead!(self, Token::ParenthesesOpen, _token, {
            if_lookahead!(self, Token::ParenthesesClose, _token, {
                return Ok(vec![]);
            });
            // parse arguments (non_empty_argument_list)
            let args = try!(self.parse_expression_list());

            if_lookahead!(self, Token::ParenthesesClose, _token, {
                return Ok(args);
            }, {
                return Err(());
            });
        });
        Err(())
    }

    fn parse_function_call(&mut self, name_arg: Option<Expr>) -> Result<Expr, ((), Option<Expr>)> {
        let old_pos = self.pos;
        let name_expr = match name_arg {
            None => try!(self.parse_name().map_err(|x| (x, None))),
            Some(name_expr) => name_expr,
        };
        let args = match self.parse_argument_list() {
            Err(x) => {
                self.pos = old_pos;
                return Err((x, Some(name_expr)));
            },
            Ok(x) => x,
        };
        Ok(Expr(Expr_::Call(Box::new(name_expr), args), Span::new()))
    }

    fn parse_callable_variable(&mut self) -> Result<Expr, ()> {
        let mut var_expr = match self.parse_simple_variable() {
            Err(x) => try!(self.parse_function_call(None).map_err(|x| x.0)),
            Ok(x) => x,
        };
        // ('[' optional_expr ']')+
        // TODO: fix the spans
        loop {
            if_lookahead!(self, Token::SquareBracketOpen, _tok, match self.parse_opt_expression(Precedence::None) {
                Err(x) => return Err(x),
                Ok(expr) => if_lookahead!(self, Token::SquareBracketClose, _tok, match expr {
                    Some(expr) => {
                        if let Expr(Expr_::ArrayIdx(_, ref mut idxs), _) = var_expr {
                            idxs.push(expr);
                        } else {
                            var_expr = Expr(Expr_::ArrayIdx(Box::new(var_expr), vec![expr]), Span::new());
                        }
                        continue
                    },
                    None => {
                        var_expr = Expr(Expr_::ArrayIdx(Box::new(var_expr), vec![]), Span::new());
                        continue;
                    }
                }),
            });
            // attempt to inline parse_function_call, for "callable_expr argument_list"
            var_expr = match self.parse_function_call(Some(var_expr)) {
                Err((err, var_expr)) => return Ok(var_expr.unwrap()),
                Ok(x) => x,
            };
        }
        Err(())
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

    fn parse_dereferencable(&mut self, parsed_variable: Option<Expr>, var_fallthrough: bool) -> Result<Expr, ()> {
        #[inline]
        fn parse_deref_operand(p: &mut Parser, var_fallthrough: bool) -> Result<(bool, Expr), ()> {
            if var_fallthrough {
                alt!(p.parse_variable().map(|x| (false, x)));
            }
            if let Ok(x) =  p.parse_class_name() {
                return Ok((true, x))
            }
            Err(())
        }

        let old_pos = self.pos;
        let (is_class_name, mut var_expr) = match parsed_variable {
            None => try!(parse_deref_operand(self, var_fallthrough)),
            Some(e) => (false, e),
        };
        println!("deref: {:?}", var_expr);
        // dereferencable T_OBJECT_OPERATOR property_name = (T_OBJECT_OPERATOR property_name)+
        // static_member indirection and whatever = (T_PAAMAYIM_NEKUDOTAYIM simple_variable)+
        let mut i = 0;
        loop {
            i += 1;
            //Expr(Expr_::ObjMember(Box::new(var), obj_members), Span::new())
            if !is_class_name || i > 1 {
                if_lookahead!(self, Token::ObjectOp, token, match (self.parse_property_name(), var_expr) {
                    (Err(x), var_expr_new) => var_expr = var_expr_new,
                    (Ok(p), Expr(Expr_::ObjMember(var, mut idxs), span)) => {
                        idxs.push(p);
                        var_expr = Expr(Expr_::ObjMember(var, idxs), span);
                        continue;
                    },
                    (Ok(p), expr) => {
                        var_expr = Expr(Expr_::ObjMember(Box::new(expr), vec![p]), token.1);
                        continue;
                    }
                });
            }
            // this case additionally to "dereferencable" (which are some deeply nested indirect rules)
            // accepts a "class_name" as prefix
            if_lookahead!(self, Token::ScopeOp, token, match (self.parse_simple_variable(), var_expr) {
                (Err(x), var_expr_new) => var_expr = var_expr_new,
                (Ok(p), Expr(Expr_::StaticMember(var, mut idxs), span)) => {
                    idxs.push(p);
                    var_expr = Expr(Expr_::StaticMember(var, idxs), span);
                    continue;
                },
                (Ok(p), expr) => {
                    var_expr = Expr(Expr_::StaticMember(Box::new(expr), vec![p]), token.1);
                    continue;
                }
            });
            break;
        }
        // don't allow a fallthrough in every case
        if i == 1 && (!var_fallthrough || is_class_name) {
            self.pos = old_pos;
            return Err(())
        }
        Ok(var_expr)
    }

    fn parse_variable(&mut self) -> Result<Expr, ()> {
        // To eliminate the left recursion we need to "inline" the last 2 rules of dereferencable
        // This works by passing the parsed callable variable into parse_dereferencable
        // so that it "knows" to stop the left-recursion there
        match self.parse_callable_variable() {
            Err(x) => self.parse_dereferencable(None, false),
            Ok(x) => self.parse_dereferencable(Some(x), true),
        }
    }

    fn parse_expression(&mut self, prec: Precedence) -> Result<Expr, ()> {
        let expr = try!(self.parse_unary_expression(prec));
        Ok(expr)
    }

    fn parse_opt_expression(&mut self, prec: Precedence) -> Result<Option<Expr>, ()> {
        match self.parse_expression(prec) {
            Err(()) => return Ok(None),
            x => x.map(|x| Some(x)),
        }
    }

    /// parsing all expressions after the precedence applying (stage 1 "callback")
    fn parse_postfix_expression(&mut self) -> Result<Expr, ()> {
        let expr = try!(self.parse_other_expression());
        if_lookahead!(self, Token::Increment, token, {
            return Ok(Expr(Expr_::UnaryOp(UnaryOp::PostInc, Box::new(expr)), token.1));
        });
        if_lookahead!(self, Token::Decrement, token, {
            return Ok(Expr(Expr_::UnaryOp(UnaryOp::PostDec, Box::new(expr)), token.1));
        });
        Ok(expr)
    }

    /// parsing all expressions after the precedence applying (stage 2 "callback")
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
        // new
        if_lookahead!(self, Token::New, token, {
            match self.parse_class_name_reference() {
                Err(x) => (),
                Ok(x) => {
                    let args = try!(self.parse_argument_list());
                    return Ok(Expr(Expr_::New(Box::new(x), args), token.1));
                }
            }
            // TODO: anonymous class
        });
        if_lookahead!(self, Token::Clone, token, {
            return Ok(Expr(Expr_::Clone(Box::new(try!(self.parse_expression(Precedence::None)))), token.1));
        });
        if_lookahead!(self, Token::Exit, token, {
            let expr = if_lookahead!(self, Token::ParenthesesOpen, _tok, {
                let ret = Some(try!(self.parse_expression(Precedence::None)));
                if_lookahead!(self, Token::ParenthesesClose, _tok, {}, { return Err(()); });
                ret
            }, None);
            return Ok(Expr(Expr_::Exit(expr.map(|x| Box::new(x))), token.1));
        });
        // internal_functions_in_yacc / casts
        let ret = match self.next_token() {
            Some(&TokenSpan(ref x, ref span)) => match *x {
                    Token::Include| Token::IncludeOnce | Token::Require | Token::RequireOnce
                    | Token::Isset | Token::Empty | Token::CastInt | Token::CastDouble | Token::CastString
                    | Token::CastArray | Token::CastObject | Token::CastBool | Token::CastUnset => Some((x.clone(), span.clone())),
                    _ => None,
            },
            None => None,
        };
        if let Some((token, span)) = ret {
            self.advance(1);
            // several cast operators
            let cast_ty = match token {
                Token::CastInt => Some(Ty::Int),
                Token::CastDouble => Some(Ty::Double),
                Token::CastString => Some(Ty::String),
                Token::CastArray => Some(Ty::Array),
                Token::CastObject => Some(Ty::Object),
                Token::CastBool => Some(Ty::Bool),
                Token::CastUnset => unimplemented!(),
                _ => None,
            };
            if let Some(cast_ty) = cast_ty {
                return Ok(Expr(Expr_::Cast(cast_ty, Box::new(try!(self.parse_expression(Precedence::None)))), span));
            }
            // isset/empty
            match token {
                Token::Isset | Token::Empty => {
                    if_lookahead!(self, Token::ParenthesesOpen, _token, {
                        let mut args = vec![];
                        while {
                            args.push(try!(self.parse_expression(Precedence::None)));
                            if let Token::Isset = token {
                                if_lookahead!(self, Token::Comma, _token, true, false)
                            } else {
                                false
                            }
                        } {}
                        if_lookahead!(self, Token::ParenthesesClose, _token, {}, { return Err(()) });
                        let expr = match token {
                            Token::Isset => Expr_::Isset(args),
                            Token::Empty => {
                                assert_eq!(args.len(), 1);
                                Expr_::Empty(Box::new(args.pop().unwrap()))
                            },
                            _ => unreachable!(),
                        };
                        return Ok(Expr(expr, span))
                    }, return Err(()));
                },
                _ => (),
            }
            // include/require
            let ity = match token {
                Token::Include => IncludeTy::Include,
                Token::IncludeOnce => IncludeTy::IncludeOnce,
                Token::Require => IncludeTy::Require,
                Token::RequireOnce => IncludeTy::RequireOnce,
                _ => unreachable!(),
            };
            return Ok(Expr(Expr_::Include(ity, Box::new(try!(self.parse_expression(Precedence::None)))), span))
        }
        // variable handling
        match self.parse_variable() {
            Ok(var) => {
                // variable '=' expr
                // variable '=' '&' variable
                // and all variable T_<OP>_ASSIGNs
                let assign_type = match self.next_token() {
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
                    let span = match self.next_token() {
                        Some(&TokenSpan(_, ref span)) => span.clone(),
                        _ => unreachable!()
                    };
                    self.advance(1);
                    let by_ref = match (&assign_type, self.next_token()) {
                        (&Op::Eq, Some(&TokenSpan(Token::Ampersand, _))) => {
                            self.advance(1);
                            true
                        },
                        _ => false,
                    };

                    return match self.parse_expression(Precedence::None) {
                        Ok(expr) => {
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
        alt!(self.parse_scalar());
        println!("err other_expr");
        Err(())
    }

    fn parse_namespace_name(&mut self) -> Result<Expr, ()> {
        // T_STRING ~ (NS_SEPARATOR ~ T_STRING)+
        let mut fragments = vec![];
        while let Some(_) = self.next_token() {
            if_lookahead!(self, Token::String(_), token, {
                match token {
                    TokenSpan(Token::String(str_), span) => fragments.push((str_, span)),
                    _ => unreachable!(),
                }
                let old_pos = self.pos;
                if_lookahead!(self, Token::NsSeparator, _tok, {
                    // lookahead to ensure it's followed by string
                    if_lookahead!(self, Token::String(_), _tok, {
                        self.advance(-1); // we want to use the string in the next iteration
                        continue;
                    }, self.pos = old_pos);
                });
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
        let fq = if_lookahead!(self, Token::NsSeparator, _token, true, false);
        match self.parse_namespace_name() {
            // TODO: inject FQDN as flag or something?
            Ok(x) => Ok(x),
            x => x,
        }
    }

    fn parse_class_name(&mut self) -> Result<Expr, ()> {
        if_lookahead!(self, Token::Static, token, {
            return Ok(Expr(Expr_::Path(Path::Identifier(self.interner.intern("static"))), token.1))
        });
        self.parse_name()
    }

    fn parse_class_name_reference(&mut self) -> Result<Expr, ()> {
        self.parse_class_name()
    }

    fn parse_identifier(&mut self) -> Result<Expr, ()> {
        if_lookahead!(self, Token::String(_), token, {
            if let Token::String(str_) = token.0 {
                return Ok(Expr(Expr_::Path(Path::Identifier(str_)), token.1))
            }
        });
        Err(())
    }

    fn parse_constant(&mut self) -> Result<Expr, ()> {
        // class_name T_PAAMAYIM_NEKUDOTAYIM identifier
        // parse a class_name if we don't find T_PAAMAYIM_NEKUDOTAYIM we just return the class_name
        // (which is luckily handled identically as a name)
        let name = try!(self.parse_class_name());
        if_lookahead!(self, Token::ScopeOp, token, {
            match self.parse_identifier() {
                Err(x) => return Err(x),
                Ok(ident) => return Ok(Expr(Expr_::StaticMember(Box::new(name), vec![ident]), token.1)),
            }
        });
        Ok(name)
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
        if_lookahead!(self, Token::Array, token, {
            if_lookahead!(self, Token::ParenthesesOpen, _token, {
                let pairs = try!(self.parse_array_pair_list());
                let end_pos = if_lookahead!(self, Token::ParenthesesClose, token, token.1.end, { return Err(()); });
                return Ok(Expr(Expr_::Array(pairs), Span { start: token.1.start, end: end_pos, ..Span::new() }));
            });
        });
        if_lookahead!(self, Token::SquareBracketOpen, token, {
            let pairs = try!(self.parse_array_pair_list());
            let end_pos = if_lookahead!(self, Token::SquareBracketClose, token, token.1.end, { return Err(()); });
            return Ok(Expr(Expr_::Array(pairs), Span { start: token.1.start, end: end_pos, ..Span::new() }));
        });
        if_lookahead!(self, Token::ConstantEncapsedString(_), token, {
            match token.0 {
                Token::ConstantEncapsedString(str_) => return Ok(Expr(Expr_::String(str_), token.1)),
                _ => unreachable!(),
            }
        });
        Err(())
    }

    fn parse_scalar(&mut self) -> Result<Expr, ()> {
        let next_token = self.next_token().map(|x| x.clone());
        self.advance(1);
        match next_token {
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
                    self.advance(-1);
                    alt!(self.parse_dereferencable_scalar());
                    return self.parse_constant();
                }
            }, x.1.clone())),
            None => Err(())
        }
    }

    fn parse_array_pair_list(&mut self) -> Result<Vec<(Option<Expr>, Expr)>, ()> {
        // parse array pairs as long as possible
        let mut pairs = vec![];
        loop {
            match self.parse_expression(Precedence::None) {
                Err(x) => break,
                Ok(ex) => pairs.push((None, ex)),
            }
            if_lookahead!(self, Token::Comma, _token, {}, break);
        }
        Ok(pairs)
    }

    fn parse_foreach_variable(&mut self) -> Result<Expr, ()> {
        self.parse_variable()
    }

    fn parse_statement(&mut self) -> Result<Expr, ()> {
        // parse a block: { statements }
        if_lookahead!(self, Token::CurlyBracesOpen, token, {
            let mut block = vec![];
            loop {
                // TODO: better error handling
                match self.parse_inner_statement() {
                    Err(()) => break,
                    Ok(x) => block.push(x),
                }
            }
            let end_pos = if_lookahead!(self, Token::CurlyBracesClose, token, token.1.end, { return Err(()); });
            return Ok(Expr(Expr_::Block(block), mk_span(token.1.start as usize, end_pos as usize)));
        });
        // parse a foreach statement
        if_lookahead!(self, Token::Foreach, token, {
            if_lookahead!(self, Token::ParenthesesOpen, _tok, {}, { return Err(()); });
            let expr = try!(self.parse_expression(Precedence::None));
            if_lookahead!(self, Token::As, _tok, {}, { return Err(()); });
            let key_or_v = Box::new(try!(self.parse_foreach_variable()));
            let (key, value) = if_lookahead!(self, Token::DoubleArrow, _tok,
                { (Some(key_or_v), Box::new(try!(self.parse_foreach_variable()))) },
                { (None, key_or_v) }
            );
            if_lookahead!(self, Token::ParenthesesClose, _tok, {}, { return Err(()); });
            let body = Box::new(try!(self.parse_statement()));
            let span = mk_span(token.1.start as usize, body.1.end as usize);
            return Ok(Expr(Expr_::ForEach(Box::new(expr), key, value, body), span));
        });
        // parse a for statement
        if_lookahead!(self, Token::For, token, {
            if_lookahead!(self, Token::ParenthesesOpen, _tok, {}, { return Err(()); });
            let mut stmts = [None, None, None];
            let mut i = 0;
            while {
                stmts[i] = try!(self.parse_opt_expression(Precedence::None)).map(|x| Box::new(x));
                i += 1;
                i < stmts.len()
            } { if_lookahead!(self, Token::SemiColon, _tok, {}, { return Err(()); }) }
            if_lookahead!(self, Token::ParenthesesClose, _tok, {}, { return Err(()); });
            let block = try!(self.parse_statement());
            let span = mk_span(token.1.start as usize, block.1.end as usize);
            let (initial, cond, looper) = (mem::replace(&mut stmts[0], None) , mem::replace(&mut stmts[1], None), mem::replace(&mut stmts[2], None));
            return Ok(Expr(Expr_::For(initial, cond, looper, Box::new(block)), span));
        });
        // parse an if/while-statement/do-while
        match self.next_token().map(|x| x.0.clone()) {
            Some(Token::If) | Some(Token::While) | Some(Token::Do) => {
                let token = self.next_token().unwrap().clone();
                self.advance(1);
                let mut stmts = vec![];
                let do_while_body = match token.0 {
                    Token::Do => {
                        let ret = try!(self.parse_statement());
                        if_lookahead!(self, Token::While, _tok, Some(ret), return Err(()))
                    },
                    _ => None,
                };

                loop {
                    // in the initial run we always require parentheses, also for elseif tokens
                    // for an else token we don't and if we find nothing we break
                    let (requires_parents, start_pos) = if stmts.len() == 0 {
                        (true, token.1.start)
                    } else {
                        if_lookahead!(self, Token::ElseIf, else_token, {(true, else_token.1.start)},
                            if_lookahead!(self, Token::Else, else_token, {(false, else_token.1.start)}, break)
                        )
                    };
                    let cond_expr = match requires_parents {
                        true => {
                            if_lookahead!(self, Token::ParenthesesOpen, _tok, {}, { return Err(()) });
                            let if_expr = try!(self.parse_expression(Precedence::None));
                            if_lookahead!(self, Token::ParenthesesClose, _tok, {}, return Err(()) );
                            Some(if_expr)
                        },
                        false => None,
                    };

                    if let Token::Do = token.0 {
                        let end_pos = if_lookahead!(self, Token::SemiColon, token, token.1.end, return Err(()));
                        let span = mk_span(start_pos as usize, end_pos as usize);
                        return Ok(Expr(Expr_::DoWhile(Box::new(do_while_body.unwrap()), Box::new(cond_expr.unwrap())), span));
                    }

                    let if_body = try!(self.parse_statement());
                    let span = mk_span(start_pos as usize, if_body.1.end as usize);

                    if let Token::While = token.0 {
                        return Ok(Expr(Expr_::While(Box::new(cond_expr.unwrap()), Box::new(if_body)), span))
                    }

                    if let Some(cond_expr) = cond_expr {
                        stmts.push(Expr(Expr_::If(Box::new(cond_expr), Box::new(if_body), None), span));
                    } else {
                        stmts.push(if_body);
                    }
                }
                let initial_stmt = stmts.pop().unwrap();
                return Ok(stmts.into_iter().rev().fold(initial_stmt, |acc, el| match (acc, el) {
                    (e2, Expr(Expr_::If(cond, bl, None), span)) => Expr(Expr_::If(cond, bl, Some(Box::new(e2))), span),
                    _ => unreachable!(),
                }));
            },
            _ => ()
        }
        // parse other statements
        let expr = match self.next_token().map(|x| x.clone()) {
            Some(TokenSpan(token, span)) => {
                self.advance(1);
                let ret = match token {
                    Token::Echo => Some(Expr(Expr_::Echo(try!(self.parse_expression_list())), span)),
                    Token::Return => Some(Expr(Expr_::Return(try!(self.parse_opt_expression(Precedence::None)).map(|x| Box::new(x))), span)),
                    Token::Continue => Some(Expr(Expr_::Continue(try!(self.parse_opt_expression(Precedence::None)).map(|x| Box::new(x))), span)),
                    Token::Break => Some(Expr(Expr_::Break(try!(self.parse_opt_expression(Precedence::None)).map(|x| Box::new(x))), span)),
                    Token::Throw => Some(Expr(Expr_::Throw(Box::new(try!(self.parse_expression(Precedence::None)))), span)),
                    Token::InlineHtml(str_) => Some(Expr(Expr_::Echo(vec![Expr(Expr_::String(str_), span)]), Span::new())),
                    _ => None,
                };
                if let None = ret {
                    self.advance(-1);
                }
                ret
            },
            _ => return Err(()),
        };
        // expr ';'
        let expr = try!(alt_not!(expr, Some(try!(self.parse_expression(Precedence::None)))).ok_or(()));
        // check if the statement is properly terminated
        if_lookahead!(self, Token::SemiColon, _token, { return Ok(expr) });
        Ok(expr)
    }

    /// this subform is just used to disallow certain constructs in inner scopes
    /// (e.g. not allowing namespace stuff, throwing error for __HALTCOMPILER, etc.)
    fn parse_inner_statement(&mut self) -> Result<Expr, ()> {
        // TODO: incomplete
        self.parse_statement()
    }

    fn parse_top_statement(&mut self) -> Result<Expr, ()> {
        // TODO: incomplete
        self.parse_statement()
    }

    fn parse_tokens(interner: Interner, toks: Vec<TokenSpan>) -> Vec<Expr> {
        // strip whitespace and unnecessary tokens
        let mut tokens: Vec<TokenSpan> = vec![];
        for tok in toks.into_iter() {
            match tok.0 {
                // TODO: pass doc comment in the span on (don't ignore them)
                Token::Comment(_) | Token::OpenTag | Token::CloseTag => (),
                Token::InlineHtml(str_) => tokens.extend(vec![
                    TokenSpan(Token::Echo, Span::new()),
                    TokenSpan(Token::ConstantEncapsedString(str_), tok.1),
                    TokenSpan(Token::SemiColon, Span::new()),
                ]),
                Token::OpenTagWithEcho => tokens.push(TokenSpan(Token::Echo, tok.1)),
                _ => tokens.push(tok),
            }
        }
        println!("{:?}", tokens);
        let mut p = Parser::new(tokens, interner);
        // error handling..
        let mut exprs = vec![];
        loop {
            match p.parse_top_statement() {
                Ok(x) => exprs.push(x),
                Err(_) => break,
            }
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
