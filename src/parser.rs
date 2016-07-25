///! recursive descedant parser
///! using pratt-parser techniques described in [1] and [2]
///!
///! grammar based on [3]
///!
///! [1] http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
///! [2] http://effbot.org/zone/simple-top-down-parsing.htm
///! [3] https://github.com/php/php-src/blob/ab304579ff046426f281e9a95abea8d611e38e1c/Zend/zend_language_parser.y

use std::mem;
use std::borrow::{Borrow, Cow};
use std::iter;
use tokenizer::{Tokenizer, Token, TokenSpan};
use interner::{Interner, RcStr};
pub use tokenizer::{Span, SyntaxError, TokenizerExternalState, mk_span};
pub use ast::{Block, CatchClause, Expr, Expr_, IncludeTy, UnaryOp, Op, Path, SwitchCase, Stmt, Stmt_, NullableTy, Ty, TraitUse, UseClause};
pub use ast::{Const, Decl, FunctionDecl, ClassDecl, ParamDefinition, Member, MemberModifier, MemberModifiers, ClassModifier, ClassModifiers};

#[derive(Debug)]
pub struct ParserError {
    /// A given set of tokens was expected
    tokens: Vec<Token>,
    /// the (byte-)position the tokens were expected at
    pos: usize,
    /// an optional message to replace a generic error message with
    message: Option<&'static str>,
    syntax: Option<SyntaxError>,
}

impl ParserError {
    fn new(tokens: Vec<Token>, position: usize) -> ParserError {
        ParserError {
            tokens: tokens,
            pos: position,
            message: None,
            syntax: None,
        }
    }

    fn syntax(e: SyntaxError, position: usize) -> ParserError {
        ParserError {
            tokens: vec![],
            pos: position,
            message: None,
            syntax: Some(e),
        }
    }
}

#[derive(Debug)]
pub struct SpannedParserError {
    start: u32,
    end: u32,
    line_start: u32,
    line_end: u32,
    line: usize,
    error: ParserError,
}

impl SpannedParserError {
    pub fn error_message(&self, code: Option<&str>) -> Cow<'static, str> {
        if let Some(message) = self.error.message {
            return message.into();
        }

        let mut str_ = format!("expected one of {:?} at line {:?}\n", self.error.tokens,
            self.line,
        );
        if let Some(code) = code {
            str_.push_str(&code[self.line_start as usize..self.line_end as usize]);
            str_.push_str("\n");
            str_.push_str(&iter::repeat(" ").take((self.start - self.line_start) as usize).collect::<String>());
            str_.push_str("^");
            str_.push_str(&iter::repeat("~").take((self.end-self.start-1) as usize).collect::<String>());
        }
        str_.into()
    }
}

pub struct Parser {
    interner: Interner,
    external: TokenizerExternalState,
    tokens: Vec<TokenSpan>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<TokenSpan>, ext: TokenizerExternalState, interner: Interner) -> Parser {
        Parser {
            tokens: tokens,
            interner: interner,
            external: ext,
            pos: 0,
        }
    }

    #[inline]
    fn advance(&mut self, n: isize) {
        self.pos = (self.pos as isize + n as isize) as usize;
    }

    #[inline]
    fn next_token(&self) -> Option<&TokenSpan> {
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
        Err(x) => x,
    })
}

macro_rules! deepest {
    ($store:expr, $expr:expr) => {
        match $expr {
            Ok(e) => return Ok(e),
            Err(x) => {
                match $store {
                    Some((spos, _)) => if x.pos > spos {
                        $store = Some((x.pos, x));
                    },
                    None => $store = Some((x.pos, x)),
                }
            }
        }
    };
}

macro_rules! deepest_unpack {
    ($self_:expr, $err:expr) => {match $err {
        Some((_, err)) => Err(err),
        None => Err(ParserError::new(vec![], $self_.pos)),
    }};
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

// reset the position if no return happened
macro_rules! if_lookahead_restore {
    ($self_:expr, $a:pat, $v:ident, $block:expr) => {if_lookahead!($self_, $a, $v, { let bak_pos = $self_.pos - 1; $block; $self_.pos = bak_pos; })};
}

// check if the next token is X, if not return a ParserError expecting it
macro_rules! if_lookahead_expect {
    ($self_:expr, $a:pat, $b:expr, $v:ident, $block:expr, $else_block:expr) => {
        if_lookahead!($self_, $a, $v, $block, {
            $else_block;
            return Err(ParserError::new(vec![$b], $self_.pos))
        })
    };
    ($self_:expr, $a:pat, $b:expr, $v:ident, $block:expr) => {if_lookahead_expect!($self_, $a, $b, $v, $block, {})};
    ($self_:expr, $a:pat, $b:expr) => {if_lookahead_expect!($self_, $a, $b, _tok, {})};
}

impl Parser {
    fn parse_unary_expression(&mut self, precedence: Precedence) -> Result<Expr, ParserError> {
        let left = match self.next_token() {
            Some(x) => x.clone(),
            None => return Err(ParserError::new(vec![], self.pos)),
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
                let expr = try!(self.parse_expression(Precedence::Unary));
                let span = mk_span(left.1.start as usize, expr.1.end as usize);
                Expr(Expr_::UnaryOp(op, Box::new(expr)), span)
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

    fn parse_binary_expression(&mut self, left: &mut Expr, precedence: Precedence) -> Result<bool, ParserError> {
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
            if_lookahead_expect!(self, Token::Colon, Token::Colon);
            let expr_ternary_else = try!(self.parse_expression(new_precedence));
            let tmp = Box::new(mem::replace(left, Expr(Expr_::Int(0), Span::new())));
            let span = mk_span(tmp.1.start as usize, expr_ternary_else.1.end as usize);
            *left = Expr(Expr_::TernaryIf(tmp, expr_ternary_if.map(|x| Box::new(x)), Box::new(expr_ternary_else)), span);
            return Ok(true);
        }

        // also try to match instanceof (non associative!)
        if let Token::InstanceOf = op_token.0 {
            if let Expr(Expr_::InstanceOf(_, _), _) = *left {
                // TODO: throw an error due to the non-associative nature
                unreachable!();
            }
            let right = try!(self.parse_class_name_reference());
            let tmp = Box::new(mem::replace(left, Expr(Expr_::Int(0), Span::new())));
            let span = mk_span(tmp.1.start as usize, right.1.end as usize);
            *left = Expr(Expr_::InstanceOf(tmp, Box::new(right)), span);
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
        let tmp = Box::new(mem::replace(left, Expr(Expr_::Int(0), Span::new())));
        let span = mk_span(tmp.1.start as usize, right.1.end as usize);
        *left = Expr(Expr_::BinaryOp(binary_op, tmp, Box::new(right)), span);
        Ok(true)
    }

    fn parse_simple_variable(&mut self) -> Result<Expr, ParserError> {
        // TODO '$' '{' expr '}'
        // TODO '$' simple_variable
        if_lookahead!(self, Token::Dollar, _token, unimplemented!());
        // T_VARIABLE
        if_lookahead!(self, Token::Variable(_), _token, Ok(match _token {
            TokenSpan(Token::Variable(varname), span) => Expr(Expr_::Variable(varname.into()), span),
            _ => unreachable!(),
        }), {

            return Err(ParserError::new(vec![Token::Dollar, Token::Variable(self.interner.intern(""))], self.pos));
        })
    }

    #[inline]
    fn parse_expression_list(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut args = vec![];
        loop {
            args.push(try!(self.parse_expression(Precedence::None)));
            if_lookahead!(self, Token::Comma, _tok, {}, break);
        }
        Ok(args)
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ParserError> {
        if_lookahead_expect!(self, Token::ParenthesesOpen, Token::ParenthesesOpen, _token, {
            if_lookahead!(self, Token::ParenthesesClose, _token, {
                return Ok(vec![]);
            });
            // parse arguments (non_empty_argument_list)
            let args = try!(self.parse_expression_list());

            if_lookahead_expect!(self, Token::ParenthesesClose, Token::ParenthesesClose, _token, return Ok(args));
        });
    }

    fn parse_function_call(&mut self, name_arg: Option<Expr>) -> Result<Expr, (ParserError, Option<Expr>)> {
        let old_pos = self.pos;
        let name_expr = match name_arg {
            None => try!(self.parse_name_as_expr().map_err(|x| (x, None))),
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

    fn parse_property_name(&mut self) -> Result<Expr, ParserError> {
        let err1 = alt!(self.parse_simple_variable());
        if_lookahead!(self, Token::String(_), token, {
            return Ok(match token.0 {
                Token::String(str_) => Expr(Expr_::Path(Path::Identifier(str_.into())), token.1),
                _ => unreachable!(),
            })
        });
        if_lookahead!(self, Token::CurlyBracesOpen, _tok, {
            match self.parse_expression(Precedence::None) {
                Err(x) => return Err(x),
                Ok(expr) => if_lookahead_expect!(self, Token::CurlyBracesClose, Token::CurlyBracesClose, _tok, return Ok(expr)),
            }
        });
        Err(ParserError::new(vec![], self.pos))
    }

    fn parse_variable(&mut self) -> Result<Expr, ParserError> {
        #[inline]
        fn parse_base_variable(p: &mut Parser) -> Result<(Expr, bool), ParserError> {
            let mut deepest_err: Option<(usize, ParserError)> = None;
            if_lookahead!(p, Token::ParenthesesOpen, token, {
                let expr = try!(p.parse_expression(Precedence::None)).0;
                let end_pos = if_lookahead_expect!(p, Token::ParenthesesClose, Token::ParenthesesClose, token, token.1.end);
                let span = mk_span(token.1.start as usize, end_pos as usize);
                return Ok((Expr(expr, span), false));
            });
            deepest!(deepest_err, p.parse_simple_variable().map(|x| (x, false)));
            deepest!(deepest_err, p.parse_dereferencable_scalar().map(|x| (x, false)));
            deepest_unpack!(p, deepest_err)
        }

        //  parse the "base" item, after which some "appendixes" for array indexing, function calling or members can be
        //    class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable
        //    class_name T_PAAMAYIM_NEKUDOTAYIM identifier '('   //inlined the "constant" grammar rule
        #[inline]
        fn parse_const_scoped(p: &mut Parser) -> Result<(Expr, Expr, Span), ParserError> {
            let old_pos = p.pos;
            if let Ok(cls_name) = p.parse_class_name() {
                if_lookahead!(p, Token::ScopeOp, tok, {
                    let start_pos = { let Expr(_, ref span) = cls_name; span.start };
                    if let Ok(var_name) = p.parse_simple_variable() {
                        let end_pos = { let Expr(_, ref span) = var_name; span.end };
                        return Ok((cls_name, var_name, mk_span(start_pos as usize, end_pos as usize)));
                    }
                    let identifier = try!(p.parse_identifier_as_expr());
                    if let Some(&TokenSpan(Token::ParenthesesOpen, _)) = p.next_token() {
                        let end_pos = { let Expr(_, ref span) = identifier; span.end };
                        return Ok((cls_name, identifier, mk_span(start_pos as usize, end_pos as usize)));
                    }
                });
            }
            p.pos = old_pos;
            Err(ParserError::new(vec![], p.pos))
        }

        #[inline]
        fn parse_fn_call_base_item(p: &mut Parser) -> Result<Expr, ParserError> {
            let old_pos = p.pos;
            // parse name followed by call syntax
            if let Ok(name) = p.parse_name_as_expr() {
                if let Some(&TokenSpan(Token::ParenthesesOpen, _)) = p.next_token() {
                    return Ok(name)
                }
            }
            p.pos = old_pos;
            Err(ParserError::new(vec![], p.pos))
        }

        let old_pos = self.pos;
        let (mut var_expr, requires_appendix) = match parse_const_scoped(self) {
            Ok((cls, prop, span)) => (Expr(Expr_::StaticMember(Box::new(cls), vec![prop]), span), false),
            Err(_) => match parse_fn_call_base_item(self) {
                Ok(e) => (e, false),
                Err(_) => match parse_base_variable(self) {
                    Ok(e) => e,
                    Err(e) => {
                        self.pos = old_pos;
                        return Err(e);
                    }
                }
            }
        };

        let mut i = 0;
        // handle appendixes
        loop {
            i += 1;
            // array indexing
            if_lookahead!(self, Token::SquareBracketOpen, _tok, match self.parse_opt_expression(Precedence::None) {
                Err(x) => return Err(x),
                Ok(expr) => if_lookahead!(self, Token::SquareBracketClose, _tok, match expr {
                    Some(expr) => {
                        if let Expr(Expr_::ArrayIdx(_, ref mut idxs), ref mut span) = var_expr {
                            span.end = self.tokens[self.pos-1].1.end;
                            idxs.push(expr);
                        } else {
                            let span = mk_span(var_expr.1.start as usize, self.tokens[self.pos-1].1.end as usize);
                            var_expr = Expr(Expr_::ArrayIdx(Box::new(var_expr), vec![expr]), span);
                        }
                        continue
                    },
                    None => {
                        let span = mk_span(var_expr.1.start as usize, self.tokens[self.pos-1].1.end as usize);
                        var_expr = Expr(Expr_::ArrayIdx(Box::new(var_expr), vec![]), span);
                        continue;
                    }
                }),
            });
            // object property indexing
            if_lookahead!(self, Token::ObjectOp, token, match (self.parse_property_name(), var_expr) {
                (Err(x), var_expr_new) => var_expr = var_expr_new,
                (Ok(p), Expr(Expr_::ObjMember(var, mut idxs), mut span)) => {
                    idxs.push(p);
                    span.end = self.tokens[self.pos-1].1.end;
                    var_expr = Expr(Expr_::ObjMember(var, idxs), span);
                    continue;
                },
                (Ok(p), Expr(expr, old_span)) => {
                    let span = mk_span(old_span.start as usize, self.tokens[self.pos-1].1.end as usize);
                    var_expr = Expr(Expr_::ObjMember(Box::new(Expr(expr, old_span)), vec![p]), span);
                    continue;
                }
            });
            // static member indexing
            if_lookahead!(self, Token::ScopeOp, token, match (self.parse_simple_variable(), var_expr) {
                (Err(x), var_expr_new) => var_expr = var_expr_new,
                (Ok(p), Expr(Expr_::StaticMember(var, mut idxs), mut span)) => {
                    idxs.push(p);
                    span.end = self.tokens[self.pos-1].1.end;
                    var_expr = Expr(Expr_::StaticMember(var, idxs), span);
                    continue;
                },
                (Ok(p), Expr(expr, old_span)) => {
                    let span = mk_span(old_span.start as usize, self.tokens[self.pos-1].1.end as usize);
                    var_expr = Expr(Expr_::StaticMember(Box::new(Expr(expr, old_span)), vec![p]), span);
                    continue;
                }
            });
            // call syntax
            if let Some(&TokenSpan(Token::ParenthesesOpen, _)) = self.next_token() {
                let args = try!(self.parse_argument_list());
                let span = mk_span(var_expr.1.start as usize, self.tokens[self.pos-1].1.end as usize);
                var_expr = Expr(Expr_::Call(Box::new(var_expr), args), span);
                continue;
            }
            break;
        }

        // filter out the expression types that can't be alone
        if requires_appendix && i < 2 {
            self.pos = old_pos;
            return Err(ParserError::new(vec![], self.pos));
        }

        Ok(var_expr)
    }

    fn parse_expression(&mut self, prec: Precedence) -> Result<Expr, ParserError> {
        let expr = try!(self.parse_unary_expression(prec));
        Ok(expr)
    }

    fn parse_opt_expression(&mut self, prec: Precedence) -> Result<Option<Expr>, ParserError> {
        match self.parse_expression(prec) {
            //TODO: maybe check for ParseError(vec![]) ?
            Err(_) => return Ok(None),
            x => x.map(|x| Some(x)),
        }
    }

    /// parsing all expressions after the precedence applying (stage 1 "callback")
    fn parse_postfix_expression(&mut self) -> Result<Expr, ParserError> {
        let expr = try!(self.parse_other_expression());
        let start_pos = expr.1.start;
        if_lookahead!(self, Token::Increment, token, {
            return Ok(Expr(Expr_::UnaryOp(UnaryOp::PostInc, Box::new(expr)), mk_span(start_pos as usize, token.1.end as usize)));
        });
        if_lookahead!(self, Token::Decrement, token, {
            return Ok(Expr(Expr_::UnaryOp(UnaryOp::PostDec, Box::new(expr)),  mk_span(start_pos as usize, token.1.end as usize)));
        });
        Ok(expr)
    }

    #[inline]
    fn parse_is_ref(&mut self) -> bool {
        if_lookahead!(self, Token::Ampersand, _tok, true, false)
    }

    fn parse_parameter_list(&mut self) -> (Vec<ParamDefinition>, Option<ParserError>) {
        let mut params = vec![];
        loop {
            // type hint:
            let nullable = if_lookahead!(self, Token::QuestionMark, _tok, true, false);
            let mut ty = None;
            if_lookahead!(self, Token::Array, _tok, ty=Some(Ty::Array), if_lookahead!(self, Token::Callable, _tok, ty=Some(Ty::Callable), {
                match self.parse_name() {
                    Ok((path, _)) => ty=Some(Ty::Object(Some(path))),
                    Err(e) => if nullable {
                        return (params, Some(e))
                    },
                }
            }));
            let is_ref = self.parse_is_ref();
            //todo: variadic
            // parameter name
            let param_name = if_lookahead!(self, Token::Variable(_), token, {
                match token.0 {
                    Token::Variable(name) => name,
                    _ => unreachable!(),
                }
            }, {
                return (params, Some(ParserError::new(vec![Token::Variable(self.interner.intern(""))], self.pos)))
            });
            // optional default value
            let default = if_lookahead!(self, Token::Equal, _tok, Some(match self.parse_expression(Precedence::None) {
                Ok(x) => x,
                Err(err) => return (params, Some(err)),
            }), None);
            params.push(ParamDefinition {
                name: param_name,
                as_ref: is_ref,
                ty: ty,
                default: default,
            });
            if_lookahead!(self, Token::Comma, _tok, {}, break);
        }
        (params, None)
    }

    fn parse_function_declaration(&mut self, span: Span, parse_closure: bool, allow_abstract: bool) -> Result<Stmt, ParserError> {
        //TODO: doc_comment
        let returns_ref = self.parse_is_ref();
        let name = match parse_closure {
            true => None,
            false => Some(if_lookahead_expect!(self, Token::String(_), Token::String(self.interner.intern("")), token, {
                match token.0 {
                    Token::String(str_) => str_,
                    _ => unreachable!(),
                }
            }))
        };
        if_lookahead_expect!(self, Token::ParenthesesOpen, Token::ParenthesesOpen);
        let (params, params_err) = self.parse_parameter_list();
        if_lookahead!(self, Token::ParenthesesClose, _tok, {}, return Err(params_err.unwrap()));
        // lexical_vars (use clause)
        let mut use_variables = vec![];
        if parse_closure {
            if_lookahead!(self, Token::Use, _tok, {
                if_lookahead_expect!(self, Token::ParenthesesOpen, Token::ParenthesesOpen);
                loop {
                    let is_ref = self.parse_is_ref();
                    let var = if_lookahead_expect!(self, Token::Variable(_), Token::Variable(self.interner.intern("")), token, match token.0 {
                        Token::Variable(var) => var,
                        _ => unreachable!()
                    });
                    use_variables.push((is_ref, var));
                    if_lookahead!(self, Token::Comma, _tok, continue, break);
                }
                if_lookahead_expect!(self, Token::ParenthesesClose, Token::ParenthesesClose);
            });
        }
        // TODO: return_type
        let no_body = match allow_abstract {
            true => if_lookahead!(self, Token::SemiColon, token, true, false),
            false => false
        };
        let (body, stmts_err) = if !no_body {
            if_lookahead_expect!(self, Token::CurlyBracesOpen, Token::CurlyBracesOpen);
            let (body, stmts_err) = self.parse_inner_statement_list();
            if_lookahead_expect!(self, Token::CurlyBracesClose, Token::CurlyBracesClose, tok, tok.1.end, {
                if let Some(err) = stmts_err {
                    return Err(err)
                }
            });
            (body, stmts_err)
        } else {
            (vec![], None, )
        };
        let decl = FunctionDecl {
            params: params,
            body: Block(body),
            usev: use_variables,
            ret_ref: returns_ref,
        };
        let span = mk_span(span.start as usize, self.tokens[self.pos-1].1.end as usize);
        return Ok(Stmt(match name {
            None => Stmt_::Expr(Expr(Expr_::Function(decl), span.clone())),
            Some(name) => Stmt_::Decl(Decl::GlobalFunction(name, decl)),
        }, span))
    }

    /// parses a class or trait declaration
    fn parse_oo_declaration(&mut self) -> Result<Stmt, ParserError> {
        let old_pos = self.pos;
        enum OoType {
            Class,
            Trait,
            Interface,
        }
        let oo_type = if_lookahead!(self, Token::Trait, _tok, OoType::Trait, if_lookahead!(self, Token::Interface, _tok, OoType::Interface, OoType::Class));

        let mut class_modifiers = vec![];
        // only a class has modifiers (and a class token ofcourse)
        if let OoType::Class = oo_type {
            loop {
                if_lookahead!(self, Token::Abstract, _tok, { class_modifiers.push(ClassModifier::Abstract); continue; });
                if_lookahead!(self, Token::Final, _tok, { class_modifiers.push(ClassModifier::Final); continue; });
                break;
            }
            if_lookahead_expect!(self, Token::Class, Token::Class);
        }
        let start_pos = self.tokens[self.pos - 1 - class_modifiers.len()].1.start;
        let name = if_lookahead_expect!(self, Token::String(_), Token::String(self.interner.intern("")), token, match token.0 {
            Token::String(str_) => str_,
            _ => unreachable!(),
        });
        // extends are only valid for interfaces and classes
        let extends = match oo_type {
            OoType::Class => if_lookahead!(self, Token::Extends, _tok, Some(try!(self.parse_name()).0), None),
            _ => None,
        };
        // implements = extended interfaces (equals to implements clause for classes and extends for interfaces)
        let implements_token = match oo_type {
            OoType::Class => Some(Token::Implements),
            OoType::Interface => Some(Token::Extends),
            _ => None,
        };
        let implements = match implements_token {
            Some(itoken) => if let Some(r_token) = self.next_token().map(|x| x.0.clone()) {
                if r_token == itoken {
                    self.advance(1);
                    try!(self.parse_name_list()).into_iter().map(|x| x.0).collect()
                } else { vec![] }
            } else { vec![] },
            _ => vec![],
        };
        if_lookahead_expect!(self, Token::CurlyBracesOpen, Token::CurlyBracesOpen);
        let (members, err) = self.parse_class_statement_list();
        let end_pos = if_lookahead_expect!(self, Token::CurlyBracesClose, Token::CurlyBracesClose, token, token.1.end, if let Some(err) = err {
            return Err(err);
        });
        let span = mk_span(start_pos as usize, end_pos as usize);
        let ret_expr = match oo_type {
            OoType::Class => Stmt_::Decl(Decl::Class(ClassDecl {
                cmod: ClassModifiers::new(&class_modifiers),
                name: name,
                base_class: extends,
                implements: implements,
                members: members,
            })),
            OoType::Interface => Stmt_::Decl(Decl::Interface(name, implements, members)),
            OoType::Trait => Stmt_::Decl(Decl::Trait(name, members)),
        };
        return Ok(Stmt(ret_expr, span));
    }

    /// parsing all expressions after the precedence applying (stage 2 "callback")
    fn parse_other_expression(&mut self) -> Result<Expr, ParserError> {
        let mut deepest_err: Option<(usize, ParserError)> = None;

        // new
        if_lookahead!(self, Token::New, token, {
            match self.parse_class_name_reference() {
                Err(x) => (),
                Ok(x) => {
                    let has_parents = if let Some(&TokenSpan(Token::ParenthesesOpen, _)) = self.next_token() { true } else {
                        false
                    };
                    let args = match has_parents {
                        true => try!(self.parse_argument_list()),
                        false => vec![],
                    };
                    let span = Span { end: self.tokens[self.pos-1].1.end, ..token.1 };
                    return Ok(Expr(Expr_::New(Box::new(x), args), span));
                }
            }
            // TODO: anonymous class
        });
        if_lookahead!(self, Token::Clone, token, {
            return Ok(Expr(Expr_::Clone(Box::new(try!(self.parse_expression(Precedence::None)))), token.1));
        });
        if_lookahead!(self, Token::Exit, token, {
            let mut span = token.1;
            let expr = if_lookahead!(self, Token::ParenthesesOpen, token, {
                let ret = Some(try!(self.parse_expression(Precedence::None)));
                if_lookahead_expect!(self, Token::ParenthesesClose, Token::ParenthesesClose, token, { span.end = token.1.end; ret })
            }, None);
            return Ok(Expr(Expr_::Exit(expr.map(|x| Box::new(x))), span));
        });
        // function declaration (anonymous function)
        if_lookahead!(self, Token::Function, token, if let Stmt_::Expr(e) = try!(self.parse_function_declaration(token.1, true, false)).0 {
            return Ok(e)
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
        if let Some((token, mut span)) = ret {
            self.advance(1);
            // several cast operators
            let cast_ty = match token {
                Token::CastInt => Some(Ty::Int),
                Token::CastDouble => Some(Ty::Double),
                Token::CastString => Some(Ty::String),
                Token::CastArray => Some(Ty::Array),
                Token::CastObject => Some(Ty::Object(None)),
                Token::CastBool => Some(Ty::Bool),
                Token::CastUnset => unimplemented!(),
                _ => None,
            };
            if let Some(cast_ty) = cast_ty {
                let expr = try!(self.parse_expression(Precedence::None));
                span.end = expr.1.end;
                return Ok(Expr(Expr_::Cast(cast_ty, Box::new(expr)), span));
            }
            // isset/empty
            match token {
                Token::Isset | Token::Empty => {
                    if_lookahead_expect!(self, Token::ParenthesesOpen, Token::ParenthesesOpen, _tok, {
                        let mut args = vec![];
                        while {
                            args.push(try!(self.parse_expression(Precedence::None)));
                            if let Token::Isset = token {
                                if_lookahead!(self, Token::Comma, _token, true, false)
                            } else {
                                false
                            }
                        } {}
                        span.end = if_lookahead_expect!(self, Token::ParenthesesClose, Token::ParenthesesClose, token, token.1.end);
                        let expr = match token {
                            Token::Isset => Expr_::Isset(args),
                            Token::Empty => {
                                assert_eq!(args.len(), 1);
                                Expr_::Empty(Box::new(args.pop().unwrap()))
                            },
                            _ => unreachable!(),
                        };
                        return Ok(Expr(expr, span))
                    });
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
            let expr = try!(self.parse_expression(Precedence::None));
            return Ok(Expr(Expr_::Include(ity, Box::new(expr)), mk_span(span.start as usize, self.tokens[self.pos-1].1.end as usize)))
        }
        // variable handling
        let assign_target = match self.parse_variable() {
            Ok(x) => Some(x),
            Err(x) => { deepest!(deepest_err, Err(x)); None },
        };

        // parse a list_statement (which is only valid as assign_target)
        let assign_target = match assign_target {
            Some(x) => Some(x),
            None => if_lookahead!(self, Token::List, token, {
                if_lookahead_expect!(self, Token::ParenthesesOpen, Token::ParenthesesOpen);
                let pairs = try!(self.parse_array_pair_list());
                let end_pos = if_lookahead_expect!(self, Token::ParenthesesClose, Token::ParenthesesClose, token, token.1.end);
                // only valid as assign target
                match self.next_token() {
                    Some(&TokenSpan(Token::Equal, _)) => (),
                    _ => return Err(ParserError::new(vec![Token::Equal], self.pos)),
                }
                let span = mk_span(token.1.start as usize, end_pos as usize);
                Some(Expr(Expr_::List(pairs), span))
            }, None),
        };

        if let Some(var) = assign_target {
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
                        let span = mk_span(var.1.start as usize, self.tokens[self.pos-1].1.end as usize);
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
        };

        // '(' expr ')'
        if_lookahead!(self, Token::ParenthesesOpen, token, {
            let expr_ret =  try!(self.parse_expression(Precedence::None));
            if_lookahead_expect!(self, Token::ParenthesesClose, Token::ParenthesesClose, token2, {
                return Ok(Expr(expr_ret.0, mk_span(token.1.start as usize, token2.1.end as usize)));
            });
        });

        deepest!(deepest_err, self.parse_scalar());
        deepest_unpack!(self, deepest_err)
    }

    fn parse_namespace_name(&mut self) -> Result<(Path, Span), ParserError> {
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
                return Ok((match fragments.len() {
                    0 => unreachable!(),
                    1 => Path::Identifier(fragments.pop().map(|x| x.0.into()).unwrap()),
                    _ => {
                        let identifier = fragments.pop().unwrap();
                        Path::NsIdentifier(self.interner.intern(&fragments.into_iter().enumerate().fold(String::new(), |acc, (i, el)| {
                            acc + if i > 0 { "\\" } else { "" } + el.0.borrow()
                        })), identifier.0.into())
                    }
                }, span));
            });
            break;
        }
        // just let this generate our error for us (does not really do any grammar related lookahead, since it failed above, this wouldn't be reached else)
        if_lookahead_expect!(self, Token::String(_), Token::String(self.interner.intern("")));
        unreachable!();
    }

    fn parse_name(&mut self) -> Result<(Path, Span), ParserError> {
        //TODO: |   T_NAMESPACE T_NS_SEPARATOR namespace_name   { $$ = $3; $$->attr = ZEND_NAME_RELATIVE; }
        // try to consume the \\ if one exists so that a namespace_name will be matched
        // then the path will be a fully quallified (FQ)
        let fq = if_lookahead!(self, Token::NsSeparator, _token, true, false);
        match self.parse_namespace_name() {
            // TODO: inject FQDN as flag or something?
            Ok(x) => Ok(x),
            Err(x) => Err(x),
        }
    }

    #[inline]
    fn parse_name_as_expr(&mut self) -> Result<Expr, ParserError> {
        let (path, span) = try!(self.parse_name());
        return Ok(Expr(Expr_::Path(path), span))
    }

    fn parse_name_list(&mut self) -> Result<Vec<(Path, Span)>, ParserError> {
        let mut names = vec![];
        loop {
            names.push(try!(self.parse_name()));
            if_lookahead!(self, Token::Comma, _tok, continue, break);
        }
        Ok(names)
    }

    #[inline]
    fn parse_name_list_as_expr(&mut self) -> Result<Vec<Expr>, ParserError> {
        Ok(try!(self.parse_name_list()).into_iter().map(|(expr, span)| Expr(Expr_::Path(expr), span)).collect())
    }

    fn parse_class_name(&mut self) -> Result<Expr, ParserError> {
        if_lookahead!(self, Token::Static, token, {
            return Ok(Expr(Expr_::Path(Path::Identifier(self.interner.intern("static"))), token.1))
        });
        self.parse_name_as_expr()
    }

    fn parse_class_name_reference(&mut self) -> Result<Expr, ParserError> {
        self.parse_class_name()
    }

    fn parse_identifier(&mut self) -> Result<(RcStr, Span), ParserError> {
        if let Some(TokenSpan(token, span)) = self.next_token().map(|x| x.clone()) {
            if let Token::String(str_) = token {
                self.advance(1);
                return Ok((str_, span));
            } else if token.is_reserved_non_modifier() {
                self.advance(1);
                return Ok((self.interner.intern(token.repr()), span));
            }
        }
        return Err(ParserError::new(vec![Token::String(self.interner.intern(""))], self.pos));
    }

    #[inline]
    fn parse_identifier_as_expr(&mut self) -> Result<Expr, ParserError> {
        let (path, span) = try!(self.parse_identifier());
        Ok(Expr(Expr_::Path(Path::Identifier(path)), span))
    }

    fn parse_constant(&mut self) -> Result<Expr, ParserError> {
        // class_name T_PAAMAYIM_NEKUDOTAYIM identifier
        // parse a class_name if we don't find T_PAAMAYIM_NEKUDOTAYIM we just return the class_name
        // (which is luckily handled identically as a name)
        let name = try!(self.parse_class_name());
        if_lookahead!(self, Token::ScopeOp, token, {
            match self.parse_identifier_as_expr() {
                Err(x) => return Err(x),
                Ok(ident) => {
                    let span = mk_span(name.1.start as usize, ident.1.end as usize);
                    return Ok(Expr(Expr_::StaticMember(Box::new(name), vec![ident]), span))
                }
            }
        });
        Ok(name)
    }

    fn parse_encaps_list(&mut self) -> Result<Expr, ParserError> {
        let mut str_ = String::new();
        let mut parts = vec![];
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
                        continue;
                    },
                    _ => unreachable!(),
                }
            });
            if_lookahead!(self, Token::CurlyBracesOpen, _tok, {
                if !str_.is_empty() {
                    parts.push(Expr(Expr_::String(self.interner.intern(&str_)), mk_span(start_pos.unwrap() as usize, end_pos as usize)));
                }
                str_.clear();
                let expr = try!(self.parse_expression(Precedence::None));
                end_pos = if_lookahead_expect!(self, Token::CurlyBracesClose, Token::CurlyBracesClose, token, token.1.end);
                start_pos = Some(end_pos);
                parts.push(expr);
                continue;
            });
            match self.parse_variable() {
                Ok(expr) => {
                    if !str_.is_empty() {
                        parts.push(Expr(Expr_::String(self.interner.intern(&str_)), mk_span(start_pos.unwrap() as usize, end_pos as usize)));
                    }
                    str_.clear();
                    start_pos = Some(expr.1.end);
                    end_pos = expr.1.end;
                    parts.push(expr);
                    continue;
                },
                Err(_) => (),
            }
            break;
        }
        let str_expr = if !str_.is_empty() {
            Some(Expr(Expr_::String(self.interner.intern(&str_)), Span { start: start_pos.unwrap(), end: end_pos, ..Span::new() }))
        } else {
            None
        };
        if !parts.is_empty() {
            let initial_expr = match str_expr {
                Some(x) => x,
                None => parts.pop().unwrap(),
            };
            // concat all parts
            return Ok(parts.into_iter().rev().fold(initial_expr, |acc, part| {
                let span = mk_span(part.1.start as usize, acc.1.end as usize);
                Expr(Expr_::BinaryOp(Op::Concat, Box::new(part), Box::new(acc)), span)
            }));
        }
        if let Some(str_expr) = str_expr {
            return Ok(str_expr);
        }
        // use this to generate our error, does not anything related to the grammar
        if_lookahead_expect!(self, Token::ConstantEncapsedString(_), Token::ConstantEncapsedString(self.interner.intern("")));
        unreachable!();
    }

    fn parse_dereferencable_scalar(&mut self) -> Result<Expr, ParserError> {
        if_lookahead!(self, Token::Array, token, {
            if_lookahead_expect!(self, Token::ParenthesesOpen, Token::ParenthesesOpen, _tok, {
                let pairs = try!(self.parse_array_pair_list());
                let end_pos = if_lookahead_expect!(self, Token::ParenthesesClose, Token::ParenthesesClose, token, token.1.end);
                return Ok(Expr(Expr_::Array(pairs), Span { start: token.1.start, end: end_pos, ..Span::new() }));
            });
        });
        if_lookahead!(self, Token::SquareBracketOpen, token, {
            let pairs = try!(self.parse_array_pair_list());
            let end_pos = if_lookahead_expect!(self, Token::SquareBracketClose, Token::SquareBracketClose, token, token.1.end);
            return Ok(Expr(Expr_::Array(pairs), Span { start: token.1.start, end: end_pos, ..Span::new() }));
        });
        if_lookahead!(self, Token::ConstantEncapsedString(_), token, {
            match token.0 {
                Token::ConstantEncapsedString(str_) => return Ok(Expr(Expr_::String(str_), token.1)),
                _ => unreachable!(),
            }
        });
        let expected = vec![Token::Array, Token::SquareBracketOpen, Token::ConstantEncapsedString(self.interner.intern(""))];
        return Err(ParserError::new(expected, self.pos))
    }

    fn parse_scalar(&mut self) -> Result<Expr, ParserError> {
        let next_token = self.next_token().map(|x| x.clone());
        self.advance(1);
        match next_token {
            Some(x) => Ok(Expr(match x.0 {
                // LNUMBER
                Token::Int(x) => Expr_::Int(x),
                // DNUMBER
                Token::Double(x) => Expr_::Double(x),
                // several magic constants
                Token::MagicLine => Expr_::Constant(Const::MagicLine),
                Token::MagicFile => Expr_::Constant(Const::MagicFile),
                Token::MagicDir => Expr_::Constant(Const::MagicDir),
                Token::MagicTrait => Expr_::Constant(Const::MagicTrait),
                Token::MagicMethod => Expr_::Constant(Const::MagicMethod),
                Token::MagicFunction => Expr_::Constant(Const::MagicFunction),
                Token::MagicClass => Expr_::Constant(Const::MagicClass),
                // '"' encaps_list '"'     { $$ = $2; }
                Token::DoubleQuote => {
                    let mut ret = try!(self.parse_encaps_list());
                    ret.1.start = x.1.start;
                    ret.1.end = if_lookahead_expect!(self, Token::DoubleQuote, Token::DoubleQuote, token, token.1.end);
                    return Ok(ret);
                },
                Token::HereDocStart => {
                    let mut ret = try!(self.parse_encaps_list());
                    ret.1.start = x.1.start;
                    ret.1.end = if_lookahead_expect!(self, Token::HereDocEnd, Token::HereDocEnd, token, token.1.end);
                    return Ok(ret);
                },
                // TODO  |   T_START_HEREDOC encaps_list T_END_HEREDOC { $$ = $2; }
                _ => {
                    self.advance(-1);
                    // TODO: check which error of dereferencable_scalar, parse_constant goes deeper
                    alt!(self.parse_dereferencable_scalar());
                    return self.parse_constant();
                }
            }, x.1.clone())),
            None => Err(ParserError::new(vec![], self.pos)),
        }
    }

    fn parse_array_pair_list(&mut self) -> Result<Vec<(Option<Expr>, Expr)>, ParserError> {
        // parse array pairs as long as possible
        let mut pairs = vec![];
        loop {
            let expr = match self.parse_expression(Precedence::None) {
                Ok(x) => x,
                Err(_) => break,
            };
            let kv_pair = if_lookahead!(self, Token::DoubleArrow, token, {
                (Some(expr), try!(self.parse_expression(Precedence::None)))
            }, {(None, expr)});
            pairs.push(kv_pair);
            if_lookahead!(self, Token::Comma, _token, {}, break);
        }
        Ok(pairs)
    }

    fn parse_foreach_variable(&mut self) -> Result<Expr, ParserError> {
        let is_var = self.parse_is_ref();
        let expr = try!(self.parse_variable());
        if is_var {
            let span = mk_span(expr.1.start as usize - 1, expr.1.start as usize);
            return Ok(Expr(Expr_::Reference(Box::new(expr)), span));
        }
        Ok(expr)
    }

    #[inline]
    fn parse_statement_extract_block(&mut self) -> Result<(Block, Span), ParserError> {
        Ok(match try!(self.parse_statement()) {
            Stmt(Stmt_::Block(bl), span) => (bl, span),
            Stmt(stmt, span) => (Block(vec![Stmt(stmt, span.clone())]), span),
        })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        let mut deepest_err: Option<(usize, ParserError)> = None;

        // parse a block: { statements }
        if_lookahead!(self, Token::CurlyBracesOpen, token, {
            let (block, stmts_err) = self.parse_inner_statement_list();
            let end_pos = if_lookahead_expect!(self, Token::CurlyBracesClose, Token::CurlyBracesClose, tok, tok.1.end, if let Some(err) = stmts_err {
                return Err(err)
            });
            let span = mk_span(token.1.start as usize, end_pos as usize);
            return Ok(Stmt(Stmt_::Block(Block(block)), span));
        });
        // parse a try statement
        if_lookahead!(self, Token::Try, token, {
            if_lookahead_expect!(self, Token::CurlyBracesOpen, Token::CurlyBracesOpen);
            let (body, stmts_err) = self.parse_inner_statement_list();
            let end_pos = if_lookahead_expect!(self, Token::CurlyBracesClose, Token::CurlyBracesClose, tok, tok.1.end, if let Some(err) = stmts_err {
                return Err(err)
            });
            // parse catch-clauses
            let mut catch_clauses = vec![];
            loop {
                if_lookahead!(self, Token::Catch, _tok, {
                    if_lookahead_expect!(self, Token::ParenthesesOpen, Token::ParenthesesOpen);
                    //TODO: support | syntax
                    let ty = try!(self.parse_name()).0;
                    let var_binding = if_lookahead_expect!(self, Token::Variable(_), Token::Variable(self.interner.intern("")), tok, match tok.0 {
                        Token::Variable(varname) => varname,
                        _ => unreachable!(),
                    });
                    if_lookahead_expect!(self, Token::ParenthesesClose, Token::ParenthesesClose);
                    if_lookahead_expect!(self, Token::CurlyBracesOpen, Token::CurlyBracesOpen);
                    let (block, stmts_err) = self.parse_inner_statement_list();
                    if_lookahead_expect!(self, Token::CurlyBracesClose, Token::CurlyBracesClose, tok, {}, if let Some(err) = stmts_err {
                        return Err(err)
                    });
                    catch_clauses.push(CatchClause { ty: ty, var: var_binding, block: Block(block) });
                }, break);
            }
            // parse finally clause (optional)
            let finally_clause = if_lookahead!(self, Token::Finally, _tok, {
                if_lookahead_expect!(self, Token::CurlyBracesOpen, Token::CurlyBracesOpen);
                let (fbody, stmts_err) = self.parse_inner_statement_list();
                let end_pos = if_lookahead_expect!(self, Token::CurlyBracesClose, Token::CurlyBracesClose, tok, tok.1.end, if let Some(err) = stmts_err {
                    return Err(err)
                });
                Some(Block(fbody))
            }, None);
            let span = mk_span(token.1.start as usize, self.tokens[self.pos-1].1.end as usize);
            return Ok(Stmt(Stmt_::Try(Block(body), catch_clauses, finally_clause), span));
        });
        // parse a unset statement
        if_lookahead!(self, Token::Unset, token, {
            if_lookahead_expect!(self, Token::ParenthesesOpen, Token::ParenthesesOpen);
            let mut vars = vec![];
            loop {
                vars.push(try!(self.parse_variable()));
                if_lookahead!(self, Token::Comma, _tok, continue, break);
            }
            if_lookahead_expect!(self, Token::ParenthesesClose, Token::ParenthesesClose);
            let end_pos = if_lookahead_expect!(self, Token::SemiColon, Token::SemiColon, token, token.1.end);
            return Ok(Stmt(Stmt_::Unset(vars), mk_span(token.1.start as usize, end_pos as usize)));
        });
        // parse a foreach statement
        if_lookahead!(self, Token::Foreach, token, {
            if_lookahead_expect!(self, Token::ParenthesesOpen, Token::ParenthesesOpen);
            let expr = try!(self.parse_expression(Precedence::None));
            if_lookahead_expect!(self, Token::As, Token::As);
            let key_or_v = Box::new(try!(self.parse_foreach_variable()));
            let (key, value) = if_lookahead!(self, Token::DoubleArrow, _tok,
                { (Some(key_or_v), Box::new(try!(self.parse_foreach_variable()))) },
                { (None, key_or_v) }
            );
            if_lookahead_expect!(self, Token::ParenthesesClose, Token::ParenthesesClose);
            let (body, bl_span) = try!(self.parse_statement_extract_block());
            let span = mk_span(token.1.start as usize, bl_span.end as usize);
            return Ok(Stmt(Stmt_::ForEach(Box::new(expr), key, value, body), span));
        });
        // parse a for statement
        if_lookahead!(self, Token::For, token, {
            if_lookahead_expect!(self, Token::ParenthesesOpen, Token::ParenthesesOpen);
            let mut stmts = [None, None, None];
            let mut i = 0;
            while {
                stmts[i] = try!(self.parse_opt_expression(Precedence::None)).map(|x| Box::new(x));
                i += 1;
                i < stmts.len()
            } { if_lookahead_expect!(self, Token::SemiColon, Token::SemiColon) }
            if_lookahead_expect!(self, Token::ParenthesesClose, Token::ParenthesesClose);
            let (block, bl_span) = try!(self.parse_statement_extract_block());
            let span = mk_span(token.1.start as usize, bl_span.end as usize);
            let (initial, cond, looper) = (mem::replace(&mut stmts[0], None) , mem::replace(&mut stmts[1], None), mem::replace(&mut stmts[2], None));
            return Ok(Stmt(Stmt_::For(initial, cond, looper, block), span));
        });
        // parse a switch statement
        if_lookahead!(self, Token::Switch, token, {
            if_lookahead_expect!(self, Token::ParenthesesOpen, Token::ParenthesesOpen);
            let expr = try!(self.parse_expression(Precedence::None));
            if_lookahead_expect!(self, Token::ParenthesesClose, Token::ParenthesesClose);
            if_lookahead_expect!(self, Token::CurlyBracesOpen, Token::CurlyBracesOpen);
            let (mut cases, mut conds) = (vec![], vec![]);
            loop {
                let case_expr = if_lookahead!(self, Token::Case, _tok, {
                    Some(try!(self.parse_expression(Precedence::None)))
                }, if_lookahead!(self, Token::Default, _tok, {
                    None
                }, break));

                if_lookahead!(self, Token::Colon, _tok, {}, if_lookahead!(self, Token::SemiColon, _tok, {}, {
                    return Err(ParserError::new(vec![Token::Colon, Token::SemiColon], self.pos));
                }));
                let (body, errs) = self.parse_inner_statement_list();
                let is_default = match case_expr {
                    Some(x) => { conds.push(x); false },
                    _ => true
                };
                if !body.is_empty() {
                    cases.push(SwitchCase { default: is_default, conds: conds.split_off(0), block: Block(body) });
                }
            }
            // add conds with empty bodys too
            for cond in conds.into_iter() {
                cases.push(SwitchCase { default: false, conds: vec![cond], block: Block(vec![]) })
            }
            let end_pos = if_lookahead_expect!(self, Token::CurlyBracesClose, Token::CurlyBracesClose, token, token.1.end);
            let span = mk_span(token.1.start as usize, end_pos as usize);
            return Ok(Stmt(Stmt_::Switch(Box::new(expr), cases), span));
        });
        // parse an if/while-statement/do-while
        match self.next_token().map(|x| x.0.clone()) {
            Some(Token::If) | Some(Token::While) | Some(Token::Do) => {
                let token = self.next_token().unwrap().clone();
                self.advance(1);
                let mut stmts = vec![];
                let do_while_body = match token.0 {
                    Token::Do => {
                        let (ret, _) = try!(self.parse_statement_extract_block());
                        if_lookahead_expect!(self, Token::While, Token::While, _tok, Some(ret))
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
                            if_lookahead_expect!(self, Token::ParenthesesOpen, Token::ParenthesesOpen);
                            let if_expr = try!(self.parse_expression(Precedence::None));
                            if_lookahead_expect!(self, Token::ParenthesesClose, Token::ParenthesesClose);
                            Some(if_expr)
                        },
                        false => None,
                    };

                    if let Token::Do = token.0 {
                        let end_pos = if_lookahead_expect!(self, Token::SemiColon, Token::SemiColon, token, token.1.end);
                        let span = mk_span(start_pos as usize, end_pos as usize);
                        return Ok(Stmt(Stmt_::DoWhile(do_while_body.unwrap(), Box::new(cond_expr.unwrap())), span));
                    }

                    let (if_body, if_span) = try!(self.parse_statement_extract_block());
                    let span = mk_span(start_pos as usize, if_span.end as usize);

                    if let Token::While = token.0 {
                        return Ok(Stmt(Stmt_::While(Box::new(cond_expr.unwrap()), if_body), span))
                    }

                    if let Some(cond_expr) = cond_expr {
                        stmts.push(Stmt(Stmt_::If(Box::new(cond_expr), if_body, Block::empty()), span));
                    } else {
                        stmts.push(Stmt(Stmt_::Block(if_body), span));
                    }
                }
                let initial_stmt = stmts.pop().unwrap();
                return Ok(stmts.into_iter().rev().fold(initial_stmt, |acc, el| match (acc, el) {
                    (Stmt(Stmt_::Block(e2), bl_span), Stmt(Stmt_::If(cond, bl, else_bl), mut span)) => {
                        assert_eq!(else_bl.0.len(), 0);
                        span.end = bl_span.end;
                        Stmt(Stmt_::If(cond, bl, e2), span)
                    },
                    (Stmt(Stmt_::If(cond, bl, else_bl), span), Stmt(Stmt_::If(cond2, bl2, mut else_bl2), mut span2)) => {
                        assert_eq!(else_bl2.0.len(), 0);
                        span2.end = span.end;
                        else_bl2.0.push(Stmt(Stmt_::If(cond, bl, else_bl), span));
                        Stmt(Stmt_::If(cond2, bl2, else_bl2), span2)
                    },
                    _ => unreachable!(),
                }));
            },
            _ => ()
        }
        // parse static variable declaration
        fn parse_static_var_decl(p: &mut Parser, span: &Span) -> Result<Stmt, ParserError> {
            let mut vars = vec![];
            loop {
                let var_name = if_lookahead_expect!(p, Token::Variable(_), Token::Variable(p.interner.intern("")), token, match token.0 {
                    Token::Variable(var) => var,
                    _ => unreachable!(),
                });
                let value = if_lookahead!(p, Token::Equal, _tok, Some(try!(p.parse_expression(Precedence::None))), None);
                vars.push((var_name, value));
                if_lookahead!(p, Token::Comma, _tok, continue, break);
            }
            if_lookahead_expect!(p, Token::SemiColon, Token::SemiColon);
            let span = mk_span(span.start as usize, p.tokens[p.pos-2].1.end as usize);
            return Ok(Stmt(Stmt_::Decl(Decl::StaticVars(vars)), span))
        }
        if_lookahead_restore!(self, Token::Static, token, {
            deepest!(deepest_err, parse_static_var_decl(self, &token.1));
        });
        // function declaration statement
        if_lookahead_restore!(self, Token::Function, token, {
            deepest!(deepest_err, self.parse_function_declaration(token.1, false, false));
        });
        deepest!(deepest_err, self.parse_oo_declaration());

        // parse other statements
        deepest!(deepest_err, match self.next_token().map(|x| x.clone()) {
            Some(TokenSpan(token, span)) => {
                self.advance(1);
                let ret = match token {
                    Token::Echo => Some(Stmt_::Echo(try!(self.parse_expression_list()))),
                    Token::Return => Some(Stmt_::Return(try!(self.parse_opt_expression(Precedence::None)).map(|x| Box::new(x)))),
                    Token::Continue => Some(Stmt_::Continue(try!(self.parse_opt_expression(Precedence::None)).map(|x| Box::new(x)))),
                    Token::Break => Some(Stmt_::Break(try!(self.parse_opt_expression(Precedence::None)).map(|x| Box::new(x)))),
                    Token::Throw => Some(Stmt_::Throw(Box::new(try!(self.parse_expression(Precedence::None))))),
                    Token::InlineHtml(str_) => Some(Stmt_::Echo(vec![Expr(Expr_::String(str_), span.clone())])),
                    _ => None,
                };
                if let None = ret {
                    self.advance(-1);
                }
                if let Some(ret) = ret {
                    // check if the statement is properly terminated
                    if_lookahead_expect!(self, Token::SemiColon, Token::SemiColon);
                    let span = mk_span(span.start as usize, self.tokens[self.pos-1].1.end as usize);
                    return Ok(Stmt(ret, span));
                }
                Err(ParserError::new(vec![], self.pos))
            },
            _ => return Err(ParserError::new(vec![], self.pos)),
        });

        // expr ';'
        deepest!(deepest_err, match self.parse_expression(Precedence::None) {
            Err(x) => Err(x),
            Ok(Expr(expr, span)) => {
                let end_pos = if_lookahead_expect!(self, Token::SemiColon, Token::SemiColon, token, token.1.end);
                let stmt_span = mk_span(span.start as usize, end_pos as usize);
                return Ok(Stmt(Stmt_::Expr(Expr(expr, span)), stmt_span));
            }
        });

        // TODO: error reporting
        deepest_unpack!(self, deepest_err)
    }

    /// this subform is just used to disallow certain constructs in inner scopes
    /// (e.g. not allowing namespace stuff, throwing error for __HALTCOMPILER, etc.)
    fn parse_inner_statement(&mut self) -> Result<Stmt, ParserError> {
        // TODO: incomplete
        self.parse_statement()
    }

    /// this will fail in all usages, and that's ok
    /// to determine whether the error is really an error just lookahead for your token
    /// mostly probably a } or )
    fn parse_inner_statement_list(&mut self) -> (Vec<Stmt>, Option<ParserError>) {
        let mut stmts = vec![];
        let tokc = self.tokens.len() - 1;
        while self.pos <= tokc {
            let stmt = match self.parse_inner_statement() {
                Err(e) => return (stmts, Some(e)),
                Ok(stmt) => stmt,
            };
            stmts.push(stmt);
        }
        (stmts, None)
    }

    fn parse_member_modifiers(&mut self) -> Vec<MemberModifier> {
        let mut modifiers = vec![];
        loop {
            if_lookahead!(self, Token::Public, _tok, { modifiers.push(MemberModifier::Public); continue });
            if_lookahead!(self, Token::Protected, _tok, { modifiers.push(MemberModifier::Protected); continue; });
            if_lookahead!(self, Token::Private, _tok, { modifiers.push(MemberModifier::Private); continue; });
            if_lookahead!(self, Token::Static, _tok, { modifiers.push(MemberModifier::Static); continue; });
            if_lookahead!(self, Token::Abstract, _tok, { modifiers.push(MemberModifier::Abstract); continue; });
            if_lookahead!(self, Token::Final, _tok, { modifiers.push(MemberModifier::Final); continue; });
            break;
        }
        modifiers
    }

    fn parse_absolute_trait_method_reference(&mut self) -> Result<(Path, RcStr), ParserError> {
        let path_to_trait = try!(self.parse_name()).0;
        if_lookahead_expect!(self, Token::ScopeOp, Token::ScopeOp);
        let identifier_of_member = try!(self.parse_identifier()).0;
        Ok((path_to_trait, identifier_of_member))
    }

    fn parse_class_statement(&mut self) -> Result<Vec<Member>, ParserError> {
        let mut members = vec![];
        let (modifiers, is_var) = if_lookahead!(self, Token::Var, _tok, (MemberModifiers::new(&[MemberModifier::Public]), true),
            (MemberModifiers::new(&self.parse_member_modifiers()), false)
        );

        if !is_var {
            if_lookahead!(self, Token::Use, token, {
                let names = try!(self.parse_name_list()).into_iter().map(|x| x.0).collect();
                // trait_adaptions
                if_lookahead!(self, Token::SemiColon, _tok, {
                    members.push(Member::TraitUse(names, vec![]));
                    return Ok(members);
                });
                if_lookahead_expect!(self, Token::CurlyBracesOpen, Token::CurlyBracesOpen);
                if_lookahead!(self, Token::CurlyBracesClose, _tok, return Ok(members));
                let mut uses = vec![];
                loop {
                    match self.parse_absolute_trait_method_reference() {
                        Ok((path_to_trait, trait_method_name)) => {
                            if_lookahead!(self, Token::Insteadof, _tok, {
                                let names = try!(self.parse_name_list()).into_iter().map(|x| x.0).collect();
                                uses.push(TraitUse::InsteadOf(path_to_trait, trait_method_name, names));
                            }, if_lookahead!(self, Token::As, _tok, {
                                if_lookahead!(self, Token::String(_), token, match token.0 {
                                    Token::String(str_) => uses.push(TraitUse::As(path_to_trait, trait_method_name, MemberModifiers::none(), Some(str_))),
                                    _ => unreachable!()
                                }, unimplemented!());
                            }));
                        },
                        _ => break,
                    }
                    if_lookahead_expect!(self, Token::SemiColon, Token::SemiColon);
                }
                if_lookahead_expect!(self, Token::CurlyBracesClose, Token::CurlyBracesClose);
                return Ok(vec![Member::TraitUse(names, uses)]);
            });
            if_lookahead!(self, Token::Function, token, {
                let (name, decl) = match try!(self.parse_function_declaration(token.1, false, true)).0 {
                    Stmt_::Decl(Decl::GlobalFunction(name, decl)) => (name, decl),
                    _ => unreachable!(),
                };
                members.push(Member::Method(modifiers, name, decl));
                // function declaration does not require semicolon as constants below, so return early
                return Ok(members);
            });
            // constants
            if members.is_empty() {
                if_lookahead!(self, Token::Const, _tok, {
                    loop {
                        let id = try!(self.parse_identifier()).0;
                        if_lookahead_expect!(self, Token::Equal, Token::Equal);
                        let val = try!(self.parse_expression(Precedence::None));
                        members.push(Member::Constant(modifiers, id, val));
                        if_lookahead!(self, Token::Comma, _tok, continue, break);
                    }
                });
            }
        }

        // properties
        if members.is_empty() {
            loop {
                let varname = if_lookahead_expect!(self, Token::Variable(_), Token::Variable(self.interner.intern("")), token, match token.0 {
                    Token::Variable(var) => var,
                    _ => unreachable!(),
                });
                let default_val = if_lookahead!(self, Token::Equal, _tok, Some(try!(self.parse_expression(Precedence::None))), None);
                members.push(Member::Property(modifiers, varname, default_val));
                if_lookahead!(self, Token::Comma, _tok, continue, break);
            }
        }
        if_lookahead_expect!(self, Token::SemiColon, Token::SemiColon);
        Ok(members)
    }

    /// refer to `parse_inner_statement_list` for usage
    fn parse_class_statement_list(&mut self) -> (Vec<Member>, Option<ParserError>) {
        let mut exprs = vec![];
        let tokc = self.tokens.len() - 1;
        while self.pos <= tokc {
            let expr = match self.parse_class_statement() {
                Err(e) => return (exprs, Some(e)),
                Ok(expr) => expr,
            };
            exprs.extend(expr);
        }
        (exprs, None)
    }

    fn parse_top_statement(&mut self) -> Result<Stmt, ParserError> {
        if_lookahead!(self, Token::Namespace, token, {
            let name = try!(self.parse_namespace_name());
            let end_pos = if_lookahead_expect!(self, Token::SemiColon, Token::SemiColon, token, token.1.start);
            let span = mk_span(token.1.start as usize, end_pos as usize);
            return Ok(Stmt(Stmt_::Decl(Decl::Namespace(name.0)), span));
        });
        if_lookahead!(self, Token::Use, token, {
            let mut clauses = vec![];
            loop {
                let ns_name = try!(self.parse_namespace_name()).0;
                let alias = if_lookahead!(self, Token::As, _tok, {
                    if_lookahead_expect!(self, Token::String(_), Token::String(self.interner.intern("")), token, {
                        match token.0 {
                            Token::String(str_) => Some(str_),
                            _ => unreachable!(),
                        }
                    })
                }, None);
                clauses.push(UseClause::QualifiedName(ns_name, alias));
                if_lookahead!(self, Token::Comma, _tok, continue, break);
            }
            let end_pos = if_lookahead_expect!(self, Token::SemiColon, Token::SemiColon, token, token.1.end);
            let span = mk_span(token.1.start as usize, end_pos as usize);
            return Ok(Stmt(Stmt_::Use(clauses), span));
        });
        // TODO: incomplete
        self.parse_statement()
    }

    fn parse_top_statement_list(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts = vec![];
        let tokc = self.tokens.len() - 1;
        while self.pos <= tokc {
            stmts.push(try!(self.parse_top_statement()));
        }
        Ok(stmts)
    }

    fn parse_tokens(interner: Interner, ext: TokenizerExternalState, toks: Vec<TokenSpan>) -> Result<Vec<Stmt>, SpannedParserError> {
        // strip whitespace and unnecessary tokens
        let mut tokens: Vec<TokenSpan> = vec![];
        for tok in toks.into_iter() {
            match tok.0 {
                // TODO: pass doc comment in the span on (don't ignore them)
                Token::Comment(_) | Token::OpenTag | Token::CloseTag => (),
                Token::InlineHtml(str_) => tokens.extend(vec![
                    TokenSpan(Token::Echo, tok.1.clone()),
                    TokenSpan(Token::ConstantEncapsedString(str_), tok.1.clone()),
                    TokenSpan(Token::SemiColon, tok.1),
                ]),
                Token::OpenTagWithEcho => tokens.push(TokenSpan(Token::Echo, tok.1)),
                _ => tokens.push(tok),
            }
        }
        //println!("{:?}", tokens);
        let mut p = Parser::new(tokens, ext, interner);
        // error handling..
        Ok(match p.parse_top_statement_list() {
            Err(e) => {
                let (pos, after) = if e.pos < p.tokens.len() { (e.pos, false) } else {
                    (p.tokens.len() - 1, true)
                };
                let span = p.tokens[pos].1.clone();
                let (start, end) = match after {
                    true => (span.end, span.end + 1),
                    false => (span.start, span.end)
                };
                let line = p.external.line_map.line_from_position(end as usize);
                let (line_start, line_end) = p.external.line_map.line(line);
                return Err(SpannedParserError {
                    start: start,
                    end: end,
                    line: line,
                    line_start: line_start,
                    line_end: line_end,
                    error: e,
                })
            },
            Ok(x) => x,
        })
    }

    pub fn parse_str(s: &str) -> Result<Vec<Stmt>, SpannedParserError> {
        let ((interner, ext_state), tokens) = {
            let mut tokenizer = Tokenizer::new(s);
            let mut tokens = vec![];
            // TODO: error & state handling
            loop {
                match tokenizer.next_token() {
                    Ok(TokenSpan(Token::End, _)) => break,
                    Ok(tok) => tokens.push(tok),
                    Err(e) => {
                        let span = e.span();
                        let line = tokenizer.state.external.line_map.line_from_position(span.end as usize);
                        let (line_start, line_end) = tokenizer.state.external.line_map.line(line);
                        return Err(SpannedParserError {
                            start: span.start,
                            end: span.end,
                            line_start: line_start,
                            line_end: line_end,
                            line: line,
                            error: ParserError::syntax(e, 0),
                        })
                    },
                }
            }
            (tokenizer.into_external_state(), tokens)
        };
        Parser::parse_tokens(interner, ext_state, tokens)
    }
}
