use std::rc::Rc;
use interner::RcStr;

#[derive(Clone, Debug)]
pub struct TokenSpan(pub Token, pub Span);

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    /// the lower byte position (inclusive)
    pub start: u32,
    /// the upper byte position (exclusive)
    pub end: u32,
    /// This allows tokens to set or unset the current doc_comment for an declaration
    /// which the parser this way can easily track
    pub doc_comment: Option<String>,
}

impl Span {
    #[inline]
    pub fn new() -> Span {
        Span {
            start: 0,
            end: 0,
            doc_comment: None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum SyntaxError {
    None,
    Unterminated(&'static str, Span),
    UnknownCharacter(Span),
}

impl SyntaxError {
    pub fn span(&self) -> Span {
        match *self {
            SyntaxError::None => unimplemented!(),
            SyntaxError::Unterminated(_, ref span) |
            SyntaxError::UnknownCharacter(ref span) => span.clone(),
        }
    }
}

#[allow(dead_code)]
// TODO: remove some day
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    End,
    // very simple tokens
    SemiColon,
    Colon,
    Comma,
    Dot,
    SquareBracketOpen,
    SquareBracketClose,
    ParenthesesOpen,
    ParenthesesClose,
    BwOr,
    BwXor,
    Ampersand,
    Plus,
    Minus,
    Div,
    Mul,
    Equal,
    Mod,
    BoolNot,
    BwNot,
    Dollar,
    Lt,
    Gt,
    QuestionMark,
    Silence,
    DollarCurlyBracesOpen,
    CurlyBracesOpen,
    CurlyBracesClose,
    /// `
    Backquote,
    DoubleQuote,
    HereDocStart,
    HereDocEnd,
    // php tokens
    OpenTagWithEcho,
    OpenTag,
    /// this counts as implicit ';'
    CloseTag,
    Exit,
    Function,
    Const,
    Return,
    Yield,
    YieldFrom,
    Try,
    Catch,
    Finally,
    Throw,
    If,
    ElseIf,
    EndIf,
    Else,
    While,
    EndWhile,
    Do,
    For,
    Endfor,
    Foreach,
    EndForeach,
    Declare,
    EndDeclare,
    InstanceOf,
    As,
    Switch,
    EndSwitch,
    Case,
    Default,
    Break,
    Continue,
    Goto,
    Echo,
    Print,
    Class,
    Interface,
    Trait,
    Extends,
    Implements,
    /// T_OBJECT_OPERATOR
    ObjectOp,
    /// T_PAAMAYIM_NEKUDOTAYIM
    ScopeOp,
    NsSeparator,
    Ellipsis,
    Coalesce,
    New,
    Clone,
    Var,
    CastInt,
    CastDouble,
    CastString,
    CastArray,
    CastObject,
    CastBool,
    CastUnset,
    Eval,
    Include,
    IncludeOnce,
    Require,
    RequireOnce,
    Namespace,
    Use,
    Insteadof,
    Global,
    Isset,
    Empty,
    HaltCompiler,
    Static,
    Abstract,
    Final,
    Private,
    Protected,
    Public,
    Unset,
    DoubleArrow,
    List,
    Array,
    Callable,
    Increment,
    Decrement,
    IsIdentical,
    IsNotIdentical,
    IsEqual,
    IsNotEqual,
    SpaceShip,
    IsSmallerOrEqual,
    IsGreaterOrEqual,
    PlusEqual,
    MinusEqual,
    MulEqual,
    Pow,
    PowEqual,
    DivEqual,
    ConcatEqual,
    ModEqual,
    SlEqual,
    SrEqual,
    AndEqual,
    OrEqual,
    XorEqual,
    BoolOr,
    BoolAnd,
    LogicalOr,
    LogicalAnd,
    LogicalXor,
    Sl,
    Sr,
    Variable(RcStr),
    Int(i64),
    Double(f64),
    Comment(RcStr),
    /// likely an arbitrary identifier
    String(RcStr),
    /// like 'test', constant encapsed string
    ConstantEncapsedString(RcStr),
    BinaryCharSequence(Rc<Vec<u8>>),
    InlineHtml(RcStr),
    // magic-tokens
    MagicClass,
    MagicTrait,
    MagicFunction,
    MagicMethod,
    MagicLine,
    MagicFile,
    MagicDir,
    MagicNamespace,
}

impl Token {
    #[inline]
    pub fn is_reserved_non_modifier(&self) -> bool {
        match *self {
            Token::Include | Token::IncludeOnce | Token::Eval | Token::Require | Token::RequireOnce | Token::LogicalOr | Token::LogicalXor | Token::LogicalAnd
            | Token::InstanceOf | Token::New | Token::Clone | Token::Exit | Token::If | Token::ElseIf | Token::Else | Token::EndIf | Token::Echo
            | Token::Do | Token::While | Token::EndWhile
            | Token::For | /*Token::EndFor |*/ Token::Foreach | Token::EndForeach | Token::Declare | Token::EndDeclare | Token::As | Token::Try | Token::Catch | Token::Finally
            | Token::Throw | Token::Use | Token::Insteadof | Token::Global | Token::Var | Token::Unset | Token::Isset | Token::Empty | Token::Continue | Token::Goto
            | Token::Function | Token::Const | Token::Return | Token::Print | Token::Yield | Token::List | Token::Switch | Token::EndSwitch | Token::Case | Token::Default
            | Token::Break | Token::Array | Token::Callable | Token::Extends | Token::Implements | Token::Namespace | Token::Trait | Token::Interface | Token::Class
            | Token::MagicClass | Token::MagicTrait | Token::MagicFunction | Token::MagicMethod | Token::MagicLine | Token::MagicFile | Token::MagicDir
            | Token::MagicNamespace => true,
            _ => false,
        }
    }

    /// Get the string representation of a token
    #[inline]
    pub fn repr(&self) -> &'static str {
        match *self {
            Token::OpenTagWithEcho => "<?=",
            Token::OpenTag => "<?php",
            Token::CloseTag => "?>",
            Token::Exit => "exit",
            Token::Function => "function",
            Token::Const => "const",
            Token::Return => "return",
            Token::Try => "try",
            Token::Catch => "catch",
            Token::Finally => "finally",
            Token::Throw => "throw",
            Token::If => "if",
            Token::ElseIf => "elseif",
            Token::EndIf => "endif",
            Token::Else => "else",
            Token::While => "while",
            Token::EndWhile => "endwhile",
            Token::Do => "do",
            Token::Foreach => "foreach",
            Token::EndForeach => "endforeach",
            Token::For => "for",
            Token::Endfor => "endfor",
            Token::Declare => "declare",
            Token::EndDeclare => "enddeclare",
            Token::InstanceOf => "instanceof",
            Token::As => "as",
            Token::Switch => "switch",
            Token::EndSwitch => "endswitch",
            Token::Case => "case",
            Token::Default => "default",
            Token::Break => "break",
            Token::Continue => "continue",
            Token::Goto => "goto",
            Token::Echo => "echo",
            Token::Print => "print",
            Token::Class => "class",
            Token::Interface => "interface",
            Token::Trait => "trait",
            Token::Extends => "extends",
            Token::Implements => "implements",
            Token::ObjectOp => "->",
            Token::ScopeOp => "::",
            Token::NsSeparator => "\\",
            Token::Ellipsis => "...",
            Token::Coalesce => "??",
            Token::New => "new",
            Token::Clone => "clone",
            Token::Var => "var",
            Token::CastInt => "int",
            Token::CastDouble => "double",
            Token::CastString => "string",
            Token::CastArray => "array",
            Token::CastObject => "object",
            Token::CastBool => "bool",
            Token::CastUnset => "unset",
            Token::Eval => "eval",
            Token::IncludeOnce => "include_once",
            Token::Include => "include",
            Token::RequireOnce => "require_once",
            Token::Require => "require",
            Token::Namespace => "namespace",
            Token::Use => "use",
            Token::Insteadof => "insteadof",
            Token::Global => "global",
            Token::Isset => "isset",
            Token::Empty => "empty",
            Token::HaltCompiler => "__halt_compiler",
            Token::Static => "static",
            Token::Abstract => "abstract",
            Token::Final => "final",
            Token::Private => "private",
            Token::Protected => "protected",
            Token::Public => "public",
            Token::Unset => "unset",
            Token::DoubleArrow => "=>",
            Token::List => "list",
            Token::Array => "array",
            Token::Callable => "callable",
            Token::Increment => "++",
            Token::Decrement => "--",
            Token::IsIdentical => "===",
            Token::IsNotIdentical => "!==",
            Token::IsEqual => "==",
            Token::IsNotEqual => "!=",
            Token::SpaceShip => "<=>",
            Token::IsSmallerOrEqual => "<=",
            Token::IsGreaterOrEqual => ">=",
            Token::PlusEqual => "+=",
            Token::MinusEqual => "-=",
            Token::MulEqual => "*=",
            Token::Pow => "**",
            Token::PowEqual => "**=",
            Token::DivEqual => "/=",
            Token::ConcatEqual => ".=",
            Token::ModEqual => "%=",
            Token::SlEqual => "<<=",
            Token::SrEqual => ">>=",
            Token::AndEqual => "&=",
            Token::OrEqual => "|=",
            Token::XorEqual => "^=",
            Token::BoolOr => "||",
            Token::BoolAnd => "&&",
            Token::LogicalOr => "OR",
            Token::LogicalAnd => "AND",
            Token::LogicalXor => "XOR",
            Token::Sl => "<<",
            Token::Sr => ">>",
            Token::CurlyBracesOpen => "{",
            Token::CurlyBracesClose => "}",
            Token::MagicClass => "__CLASS__",
            Token::MagicTrait => "__TRAIT__",
            Token::MagicFunction => "__FUNCTION__",
            Token::MagicMethod => "__METHOD__",
            Token::MagicLine => "__LINE__",
            Token::MagicFile => "__FILE__",
            Token::MagicDir => "__DIR__",
            Token::MagicNamespace => "__NAMESPACE__",
            _ => unimplemented!(),
        }
    }
}
