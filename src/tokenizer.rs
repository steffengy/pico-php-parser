//! tokenizer based on [Zend LS](https://github.com/php/php-src/blob/ebb99a1a3a2ec9216d95c63b267ae0f66074f4de/Zend/zend_language_scanner.l)
//! since the reference doesn't seem very correct in some cases
use std::str::{self, FromStr};
use std::rc::Rc;
use std::mem;

use interner::Interner;
pub use tokens::{Span, Token, TokenSpan, SyntaxError};

pub trait AsSpanPos {
    fn as_span_pos(&self) -> u32;
}

impl AsSpanPos for u32 {
    fn as_span_pos(&self) -> u32 {
        *self
    }
}

impl AsSpanPos for usize {
    fn as_span_pos(&self) -> u32 {
        *self as u32
    }
}

#[inline]
pub fn mk_span<A: AsSpanPos, B: AsSpanPos>(start: A, end: B) -> Span {
    Span {
        start: start.as_span_pos(),
        end: end.as_span_pos(),
        ..Span::new()
    }
}

#[derive(Clone, Debug, PartialEq)]
enum State {
    /// dummy state to force an ending after a `}` in InScripting
    DoNothing,
    Initial,
    InScripting,
    LookingForProperty,
    EmitQueue,
    Done,
}

pub struct Tokenizer<'a> {
    code: &'a str,
    /// whether to support short tags, equal to CG(short_tags)
    short_tags: bool,
    pub state: TokenizerState,
    queue: Vec<TokenSpan>,
    interner: Interner,
}

#[derive(Clone, Debug)]
pub struct LineMap {
    data: Vec<u32>,
    end_pos: usize,
}

impl LineMap {
    fn new() -> LineMap {
        LineMap {
            data: vec![0],
            end_pos: 0,
        }
    }

    pub fn line_from_position(&self, pos: usize) -> usize {
        let mut b = self.data.len();
        let mut a = 0;
        while b - a > 1 {
            let mid = (a + b) / 2;
            if self.data[mid] as usize > pos {
                b = mid;
            } else {
                a = mid;
            }
        }
        a
    }

    pub fn line(&self, line: usize) -> (u32, u32) {
        let end = if line + 1 < self.data.len() {
            self.data[line + 1]
        } else {
            self.end_pos as u32
        };
        (self.data[line], end)
    }

    #[inline]
    pub fn push(&mut self, pos: u32) {
        self.data.push(pos);
    }
}

/// stuff that's also important for the parser
#[derive(Debug, Clone)]
pub struct TokenizerExternalState {
    /// contains the first byte-position of every line
    pub line_map: LineMap,
}

impl TokenizerExternalState {
    fn new() -> TokenizerExternalState {
        TokenizerExternalState { line_map: LineMap::new() }
    }
}

#[derive(Debug, Clone)]
pub struct TokenizerState {
    src_pos: usize,
    state: State,
    state_stack: Vec<State>,
    line_num: usize,
    pub external: TokenizerExternalState,
    restart: bool,
}

impl TokenizerState {
    #[inline]
    fn next_line(&mut self) {
        self.external.line_map.push(self.src_pos as u32);
        self.line_num += 1;
    }
}

/// Check if a string staarts with a token (case-insensitive)
trait StrStartsWithCI {
    fn starts_with_ci(&self, s: &str) -> bool;
}

impl<'a> StrStartsWithCI for &'a str {
    #[inline]
    fn starts_with_ci(&self, s: &str) -> bool {
        self.chars().take(s.len()).collect::<String>() == s.to_lowercase()
    }
}

macro_rules! state_helper {
    (push, $self_:expr, $new_state:ident) => ({
        let old_state = mem::replace(&mut $self_.state.state, State::$new_state);
        $self_.state.state_stack.push(old_state);
    });
    (pop, $self_:expr) => ($self_.pop_state());
}

macro_rules! match_token_alias {
    // set state syntax
    ($self_:expr, $alias:expr, $token:ident, state=$new_state:ident) => {{
        let str_repr = $alias;
        if $self_.input().starts_with_ci(str_repr) {
            let old_pos = $self_.state.src_pos;
            $self_.advance(str_repr.len());
            let span = mk_span(old_pos, $self_.state.src_pos);
            $self_.state.state = State::$new_state;
            Ok(TokenSpan(Token::$token, span))
        } else { Err(SyntaxError::None) }
    }};
    // state unchanged
    ($self_:expr, $alias:expr, $token:ident) => {{
        let str_repr = $alias;
        if $self_.input().starts_with_ci(str_repr) {
            let old_pos = $self_.state.src_pos;
            $self_.advance(str_repr.len());
            let span = mk_span(old_pos, $self_.state.src_pos);
            Ok(TokenSpan(Token::$token, span))
        } else { Err(SyntaxError::None) }
    }};
}

macro_rules! match_token {
    // set state syntax
    ($self_:expr, $token:ident, state=$new_state:ident) => {match_token_alias!($self_, Token::$token.repr(), $token, state=$new_state)};
    // state unchanged
    ($self_:expr, $token:ident) => {match_token_alias!($self_, Token::$token.repr(), $token)};
    // state push
    ($self_:expr, $token:ident, state<-$new_state:ident) => {{
        let str_repr = Token::$token.repr();
        if $self_.input().starts_with_ci(str_repr) {
            let old_pos = $self_.state.src_pos;
            $self_.advance(str_repr.len());
            let span = mk_span(old_pos, $self_.state.src_pos);
            state_helper!(push, $self_, $new_state);
            Ok(TokenSpan(Token::$token, span))
        } else { Err(SyntaxError::None) }
    }};
    // state pop
    ($self_:expr, $token:ident, state->) => {{
        let str_repr = Token::$token.repr();
        if $self_.input().starts_with_ci(str_repr) {
            let old_pos = $self_.state.src_pos;
            $self_.advance(str_repr.len());
            let span = mk_span(old_pos, $self_.state.src_pos);
            state_helper!(pop, $self_);
            Ok(TokenSpan(Token::$token, span))
        } else { Err(SyntaxError::None) }
    }};
}

/// return if a token was found, else continue in code flow
macro_rules! ret_token {
    ($e:expr) => {match $e {
            Ok(x) => return Ok(x),
            Err(SyntaxError::None) => (),
            x => return x,
        }
    };
}

/// helper to transform string-members into the appropriate token
impl<'a> Tokenizer<'a> {
    pub fn new(src: &'a str) -> Tokenizer<'a> {
        let mut tokenizer = Tokenizer {
            code: src,
            state: TokenizerState {
                src_pos: 0,
                state: State::Initial,
                state_stack: vec![],
                line_num: 1,
                external: TokenizerExternalState::new(),
                restart: false,
            },
            short_tags: true,
            queue: vec![],
            interner: Interner::new(),
        };
        tokenizer.state.external.line_map.end_pos = src.len();
        tokenizer
    }

    #[inline]
    pub fn into_external_state(self) -> (Interner, TokenizerExternalState) {
        (self.interner, self.state.external)
    }

    /// advances by n-positions where a position is a char-index
    /// this returns a substring of the skipped part (old[..n])
    #[inline]
    fn advance(&mut self, n: usize) -> &'a str {
        let end_byte_pos = match self.input().char_indices().nth(n) {
            Some((byte_pos, _)) => byte_pos,
            None => self.input().len(),
        };
        let ret = &self.input()[..end_byte_pos];
        self.state.src_pos += end_byte_pos;
        ret
    }

    #[inline]
    fn input(&self) -> &'a str {
        &self.code[self.state.src_pos..]
    }

    #[inline]
    fn input_pos(&self) -> usize {
        self.state.src_pos
    }

    #[inline]
    fn pop_state(&mut self) {
        let new_state = match self.state.state_stack.pop() {
            Some(x) => x,
            None => unreachable!(),
        };
        self.state.state = new_state;
    }

    /// handle whitespace
    fn whitespace(&mut self) {
        while !self.input().is_empty() {
            match self.input().chars().nth(0).unwrap() {
                ' ' | '\t' | '\r' => {
                    self.advance(1);
                }
                '\n' => {
                    self.advance(1);
                    self.state.next_line();
                }
                _ => break,
            }
        }
    }

    /// handle tabs and spaces
    fn whitespace_only(&mut self) {
        while !self.input().is_empty() {
            match self.input().chars().nth(0).unwrap() {
                ' ' | '\t' => {
                    self.advance(1);
                }
                _ => break,
            }
        }
    }

    /// match exactly one newline
    fn newline(&mut self) -> bool {
        let amount = if self.input().starts_with("\r\n") {
            2
        } else if self.input().starts_with('\n') || self.input().starts_with('\r') {
            1
        } else {
            return false;
        };
        self.advance(amount);
        self.state.next_line();
        true
    }

    // re2c stuff (mostly prefixed with _)
    fn _label(&mut self) -> Option<(&'a str, Span)> {
        if self.input().is_empty() {
            return None;
        }
        if let Some('0'...'9') = self.input().chars().nth(0) {
            return None;
        }

        // \u{0080} U+0080
        // \u{00BF} U+00FF
        let end_pos = self.input().chars().position(|x| match x {
            'a'...'z' |
            'A'...'Z' |
            '\u{80}'...'\u{FF}' |
            '_' |
            '0'...'9' => false,
            _ => true,
        });
        let end_pos = match end_pos {
            Some(0) => return None,
            Some(x) => x,
            None => self.input().len(),
        };
        let old_pos = self.input_pos();
        let ret = self.advance(end_pos);
        Some((ret, mk_span(old_pos, old_pos + end_pos)))
    }

    /// matches a token which consists of one character (simple)
    fn _token(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().is_empty() {
            return Ok(TokenSpan(Token::End, mk_span(self.code.len(), self.code.len())));
        }
        let tok = match self.input().chars().nth(0).unwrap() {
            ';' => Token::SemiColon,
            ':' => Token::Colon,
            ',' => Token::Comma,
            '.' => Token::Dot,
            '[' => Token::SquareBracketOpen,
            ']' => Token::SquareBracketClose,
            '(' => Token::ParenthesesOpen,
            ')' => Token::ParenthesesClose,
            '|' => Token::BwOr,
            '^' => Token::BwXor,
            '&' => Token::Ampersand,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '/' => Token::Div,
            '*' => Token::Mul,
            '=' => Token::Equal,
            '%' => Token::Mod,
            '!' => Token::BoolNot,
            '~' => Token::BwNot,
            '$' => Token::Dollar,
            '<' => Token::Lt,
            '>' => Token::Gt,
            '?' => Token::QuestionMark,
            '@' => Token::Silence,
            _ => return Err(SyntaxError::None),
        };
        self.advance(1);
        Ok(TokenSpan(tok, mk_span(self.input_pos() - 1, self.input_pos())))
    }

    /// match a double number (or a long number/octal number)
    fn _onum_dnum_lnum(&mut self) -> Result<TokenSpan, SyntaxError> {
        // valid inputs for double: "long.", ".long", "long.long"
        if self.input().is_empty() {
            return Ok(TokenSpan(Token::End, mk_span(self.code.len(), self.code.len())));
        }
        let end_pos = match self.input().chars().position(|x| x < '0' || x > '9') {
            None => self.input().len(),
            Some(end_pos) => end_pos,
        };
        let old_pos = self.input_pos();
        let str_ = self.advance(end_pos).to_owned();
        if !self.input().starts_with('.') {
            {
                let span = mk_span(old_pos, self.input_pos());
                // long sub-match
                if end_pos != 0 {
                    if str_.starts_with('0') && str_.len() > 1 {
                        return Ok(TokenSpan(Token::Int(i64::from_str_radix(&str_[1..], 8).unwrap()), span));
                    } else {
                        return Ok(TokenSpan(Token::Int(i64::from_str_radix(&str_, 10).unwrap()), span));
                    }
                }
            }
            self.state.src_pos = old_pos;
            return Err(SyntaxError::None);
        }
        let mut str_ = str_ + self.advance(1);
        // at this point we either matched "long." or just "."
        let end_pos2 = match self.input().chars().position(|x| x < '0' || x > '9') {
            None => self.input().len(),
            Some(0) if str_.len() == 1 => {
                self.state.src_pos = old_pos;
                return Err(SyntaxError::None);
            }
            Some(end_pos) => end_pos,
        };
        let span = mk_span(self.input_pos(), end_pos);
        str_.push_str(self.advance(end_pos2));
        Ok(TokenSpan(Token::Double(f64::from_str(&str_).unwrap()), span))
    }

    /// match a hex number
    fn _hnum(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().len() < 3 || !self.input().starts_with("0x") {
            return Err(SyntaxError::None);
        }
        self.advance(2);
        let end_pos = match self.input().chars().position(|x| match x {
            'a'...'f' | 'A'...'F' | '0'...'9' => false,
            _ => true,
        }) {
            None => self.input().len(),
            Some(0) => return Err(SyntaxError::None),
            Some(end_pos) => end_pos,
        };
        let span = mk_span(self.input_pos() - 2, end_pos);
        let str_ = self.advance(end_pos);
        Ok(TokenSpan(Token::Int(i64::from_str_radix(str_, 16).unwrap()), span))
    }

    /// match a binary number
    fn _bnum(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().len() < 3 || !self.input().starts_with("0b") {
            return Err(SyntaxError::None);
        }
        self.advance(2);
        let end_pos = match self.input().chars().position(|x| x != '0' && x != '1') {
            None => self.input().len(),
            Some(0) => return Err(SyntaxError::None),
            Some(end_pos) => end_pos,
        };
        let span = mk_span(self.input_pos() - 2, end_pos);
        let str_ = self.advance(end_pos);
        Ok(TokenSpan(Token::Int(i64::from_str_radix(str_, 2).unwrap()), span))
    }

    /// matches ${label} so any valid variable_name
    fn match_variable(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().len() < 2 || !self.input().starts_with('$') {
            return Err(SyntaxError::None);
        }
        let bak_pos = self.input_pos();
        self.advance(1);
        match self._label().map(|(x, span)| (self.interner.intern(x), span)) {
            Some((name, mut span)) => {
                span.start = bak_pos as u32;
                Ok(TokenSpan(Token::Variable(name), span))
            }
            None => {
                self.state.src_pos = bak_pos;
                Err(SyntaxError::None)
            }
        }
    }

    fn str_escape(&mut self, bytes: &mut Vec<u8>, sq: bool) -> Result<(), SyntaxError> {
        let chr = match (self.input().chars().nth(1), sq) {
            (Some('n'), false) => Some(b'\n'),
            (Some('r'), false) => Some(b'\r'),
            (Some('t'), false) => Some(b'\t'),
            (Some('f'), false) => Some(b'\x0C'),
            (Some('v'), false) => Some(b'\x0B'),
            (Some('e'), false) => unimplemented!(),
            (Some('"'), false) => Some(b'"'),
            (Some('\''), true) => Some(b'\''),
            (Some('\\'), _) => Some(b'\\'),
            (Some('$'), false) => Some(b'$'),
            (Some(x @ 'x'), false) |
            (Some(x @ 'X'), false) => {
                // read up to 2 hex characters, on 0 add \x to bytes
                let mut end_idx = 0;
                for i in 2..4 {
                    match self.input().chars().nth(i) {
                        Some('a'...'z') | Some('A'...'Z') | Some('0'...'9') => end_idx = i,
                        _ => break,
                    }
                }
                if end_idx == 0 {
                    bytes.push(b'\\');
                    Some(x as u8)
                } else {
                    let start_pos = self.input().char_indices().nth(2).unwrap().0;
                    let end_pos = self.input().char_indices().nth(end_idx + 1).unwrap().0;
                    let byte = u8::from_str_radix(&self.input()[start_pos..end_pos], 16).unwrap();
                    bytes.push(byte);
                    self.advance(1 + end_idx);
                    return Ok(());
                }
            }
            (Some('u'), false) => unimplemented!(),
            (Some(x), _) => {
                bytes.push(b'\\');
                let mut tmp_str = String::new();
                tmp_str.push(x);
                bytes.extend(tmp_str.as_bytes());
                None
            }
            _ => return Err(SyntaxError::Unterminated("string escape sequence", mk_span(self.input_pos(), self.input_pos() + 1))),
        };
        self.advance(2);
        if let Some(chr) = chr {
            bytes.push(chr);
        }
        Ok(())
    }

    #[inline]
    fn return_tokens_from_parts(&mut self,
                                start_tok: TokenSpan,
                                end_tok: TokenSpan,
                                bytes: Vec<u8>,
                                parts: Vec<TokenSpan>)
                                -> TokenSpan {
        let (start_pos, end_pos) = (start_tok.1.start, end_tok.1.end);
        self.queue.push(end_tok);
        if !bytes.is_empty() {
            let ret_token = match String::from_utf8(bytes) {
                Ok(str_) => Token::ConstantEncapsedString(self.interner.intern(&str_)),
                Err(err) => Token::BinaryCharSequence(Rc::new(err.into_bytes())),
            };
            self.queue.push(TokenSpan(ret_token, mk_span(start_pos as usize, end_pos as usize)));
        }
        if !parts.is_empty() {
            self.queue.extend(parts.into_iter().rev());
        }
        state_helper!(push, self, EmitQueue);
        start_tok
    }

    /// matches a single-quoted string literal
    fn match_sq_string(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().len() < 2 {
            return Err(SyntaxError::None);
        }
        // backup the whole state, since line counting needs resetting too
        let bak_state = self.state.clone();
        if self.input().starts_with("b'") {
            self.advance(2);
        } else if self.input().starts_with('\'') {
            self.advance(1);
        } else {
            return Err(SyntaxError::None);
        }

        // valid escapes: \ \\
        // repeatedly progress until we encounter an escape sequence (or end)
        let mut bytes: Vec<u8> = vec![];
        loop {
            let end_pos =
                match self.input().chars().position(|x| x == '\\' || x == '\'' || x == '\n') {
                    Some(end_pos) => end_pos,
                    None => self.input().len(),
                };
            bytes.extend(self.advance(end_pos).as_bytes());
            match self.input().chars().nth(0) {
                Some('\n') => {
                    self.advance(1);
                    self.state.next_line();
                    bytes.push(b'\n');
                }
                Some('\\') => try!(self.str_escape(&mut bytes, true)),
                Some('\'') => {
                    self.advance(1);
                    break;
                }
                _ => {
                    let old_pos = self.state.src_pos;
                    self.state = bak_state;
                    return Err(SyntaxError::Unterminated("single-quoted string literal", mk_span(self.input_pos(), old_pos + 1)));
                }
            }
        }
        let span = mk_span(bak_state.src_pos, self.input_pos());
        let ret_token = match String::from_utf8(bytes) {
            Ok(str_) => Token::ConstantEncapsedString(self.interner.intern(&str_)),
            Err(err) => Token::BinaryCharSequence(Rc::new(err.into_bytes())),
        };
        Ok(TokenSpan(ret_token, span))
    }

    fn str_variable(&mut self, bytes: &mut Vec<u8>, parts: &mut Vec<TokenSpan>) {
        self.advance(1);
        // T_DOLLAR_OPEN_CURLY_BRACES ${ ... } syntax (simple = DollarCurlyBraces, complex = str_block)
        if self.input().starts_with('{') {
            let pos = self.input_pos() - 1;
            if !bytes.is_empty() {
                let len = bytes.len();
                let old_fragment = match String::from_utf8(mem::replace(bytes, vec![])) {
                    Ok(str_) => Token::ConstantEncapsedString(self.interner.intern(&str_)),
                    Err(err) => Token::BinaryCharSequence(Rc::new(err.into_bytes())),
                };
                parts.push(TokenSpan(old_fragment, mk_span(pos - len, pos)));
            }
            let next_part = parts.len();
            self.str_block(bytes, parts, false);
            // patch the CurlyBracesOpen token
            assert_eq!(parts[next_part].0, Token::CurlyBracesOpen);
            parts[next_part].1.start -= 1;
            parts[next_part].0 = Token::DollarCurlyBracesOpen;
            return;
        }
        // match variable
        if let Some((label, span)) = self._label().map(|(x, span)| (self.interner.intern(x), span)) {
            let mut tmp_parts = vec![];
            // match var_offset
            if self.input().starts_with('[') {
                unimplemented!();
            }
            // match object access (only $var->label supported in PHP)
            else if self.input().starts_with("->") {
                let bak_pos = self.input_pos();
                self.advance(2);
                if let Some((property, span)) = self._label().map(|(x, span)| (self.interner.intern(x), span)) {
                    tmp_parts.push(TokenSpan(Token::ObjectOp, mk_span(bak_pos, bak_pos+1)));
                    tmp_parts.push(TokenSpan(Token::String(property), mk_span(span.start, span.end)));
                } else {
                    self.state.src_pos = bak_pos;
                }
            }
            // and match the single variable, prepend it
            if !bytes.is_empty() {
                let len = bytes.len();
                let start = span.start - 1;
                let old_fragment = match String::from_utf8(mem::replace(bytes, vec![])) {
                    Ok(str_) => Token::ConstantEncapsedString(self.interner.intern(&str_)),
                    Err(err) => Token::BinaryCharSequence(Rc::new(err.into_bytes())),
                };
                parts.push(TokenSpan(old_fragment, mk_span(start as usize - len, start)));
            }
            parts.push(TokenSpan(Token::Variable(label), mk_span(span.start - 1, span.end)));
            parts.extend(tmp_parts);
        } else {
            bytes.push(b'$');
        }
    }

    fn str_block(&mut self,
                 bytes: &mut Vec<u8>,
                 parts: &mut Vec<TokenSpan>,
                 require_dollar: bool) {
        self.advance(1);
        if self.input().starts_with('$') || !require_dollar {
            let bak_state = self.state.clone();
            // temporary state transition to use the same instance to match
            self.state.state = State::InScripting;
            self.state.state_stack = vec![State::DoNothing];
            let mut tokens = vec![TokenSpan(Token::CurlyBracesOpen, mk_span(self.input_pos()-1, self.input_pos()))];
            while let Ok(tok) = self.next_token() {
                tokens.push(tok);
            }
            if let Some(&TokenSpan(Token::CurlyBracesClose, _)) = tokens.last() {
                if !bytes.is_empty() {
                    let len = bytes.len();
                    let start = bak_state.src_pos - 1;
                    let old_fragment = match String::from_utf8(mem::replace(bytes, vec![])) {
                        Ok(str_) => Token::ConstantEncapsedString(self.interner.intern(&str_)),
                        Err(err) => Token::BinaryCharSequence(Rc::new(err.into_bytes())),
                    };
                    parts.push(TokenSpan(old_fragment, mk_span(start - len, start)));
                }
                parts.extend(tokens);
                // undo the temporary tokenizer state transition
                self.state.state = bak_state.state;
                self.state.state_stack = bak_state.state_stack;
            } else {
                self.state = bak_state;
            }
        } else {
            bytes.push(b'{');
        }
    }

    /// matches a double-quoted string literal
    fn match_dq_string(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().len() < 2 {
            return Err(SyntaxError::None);
        }
        let bak_state_str = self.state.clone();
        if self.input().starts_with("b\"") {
            self.advance(2);
        } else if self.input().starts_with('"') {
            self.advance(1);
        } else {
            return Err(SyntaxError::None);
        }
        // valid escapes: \n \r \t \f \v \e \" \\ \$ \x \X \u{unicode}

        // repeatedly progress until we encounter an escape sequence (or end)
        let mut parts = vec![];
        let mut bytes: Vec<u8> = vec![];
        loop {
            let end_pos = match self.input()
                .chars()
                .position(|x| x == '\\' || x == '"' || x == '$' || x == '\n' || x == '{') {
                Some(end_pos) => end_pos,
                None => self.input().len() - 1,
            };
            bytes.extend(self.advance(end_pos).as_bytes());

            match self.input().chars().nth(0) {
                Some('\n') => {
                    self.advance(1);
                    self.state.next_line();
                    bytes.push(b'\n');
                }
                Some('\\') => try!(self.str_escape(&mut bytes, false)),
                Some('"') => {
                    self.advance(1);
                    break;
                }
                Some('$') => self.str_variable(&mut bytes, &mut parts),
                // match {$<IN_SCRIPTING>} block
                Some('{') => self.str_block(&mut bytes, &mut parts, true),
                _ => {
                    let err_pos = self.input_pos();
                    self.state = bak_state_str;
                    return Err(SyntaxError::Unterminated("double-quoted string literal", mk_span(self.input_pos(), err_pos)))
                },
            }
        }
        let current_pos = self.input_pos();
        Ok(self.return_tokens_from_parts(
            TokenSpan(Token::DoubleQuote, mk_span(bak_state_str.src_pos, bak_state_str.src_pos + 1)),
            TokenSpan(Token::DoubleQuote, mk_span(current_pos - 1, current_pos)),
            bytes, parts
        ))
    }

    /// backquote handling
    fn match_backquote(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().len() < 2 {
            return Err(SyntaxError::None);
        }
        let bak_state_str = if self.input().starts_with('`') {
            let bak_state = self.state.clone();
            self.advance(1);
            bak_state
        } else {
            return Err(SyntaxError::None);
        };
        let mut parts = vec![];
        let mut bytes: Vec<u8> = vec![];
        loop {
            let end_pos = match self.input()
                .chars()
                .position(|x| x == '\\' || x == '"' || x == '$' || x == '\n' || x == '{') {
                Some(end_pos) => end_pos,
                None => self.input().len() - 1,
            };
            bytes.extend(self.advance(end_pos).as_bytes());

            match self.input().chars().nth(0) {
                Some('\n') => {
                    self.advance(1);
                    self.state.next_line();
                    bytes.push(b'\n');
                }
                Some('`') => {
                    self.advance(1);
                    break;
                }
                Some('$') => self.str_variable(&mut bytes, &mut parts),
                // match {$<IN_SCRIPTING>} block
                Some('{') => self.str_block(&mut bytes, &mut parts, true),
                _ => {
                    let old_pos = self.input_pos();
                    self.state = bak_state_str;
                    return Err(SyntaxError::Unterminated("Backquote", mk_span(self.state.src_pos, old_pos)));
                }
            }
        }
        let current_pos = self.input_pos();
        Ok(self.return_tokens_from_parts(
            TokenSpan(Token::Backquote, mk_span(bak_state_str.src_pos, bak_state_str.src_pos + 1)),
            TokenSpan(Token::Backquote, mk_span(current_pos - 1, current_pos)),
            bytes, parts
        ))
    }

    /// Try to parse the heredoc or nowdoc syntax for a string
    fn match_here_now_doc(&mut self) -> Result<TokenSpan, SyntaxError> {
        enum DocType {
            /// label
            HereDoc,
            /// "label"
            HereDocEncapsed,
            /// 'label'
            NowDoc,
        }
        if self.input().len() < 8 {
            return Err(SyntaxError::None);
        }
        let bak_state_str = self.state.clone();
        if self.input().starts_with("b<<<") {
            self.advance(4);
        } else if self.input().starts_with("<<<") {
            self.advance(3);
        } else {
            return Err(SyntaxError::None);
        }
        self.whitespace_only();
        // determine the label type
        let mut doc_ty = DocType::HereDoc;
        if self.input().starts_with('\'') {
            doc_ty = DocType::NowDoc;
            self.advance(1);
        } else if self.input().starts_with('"') {
            doc_ty = DocType::HereDocEncapsed;
            self.advance(1);
        }
        // match the label
        let label = if let Some(label) = self._label().map(|x| x.0.to_owned()) {
            label
        } else {
            self.state = bak_state_str;
            return Err(SyntaxError::None);
        };
        // match the following label-type (if required)
        let required_chr = match doc_ty {
            DocType::HereDocEncapsed => Some('"'),
            DocType::NowDoc => Some('\''),
            DocType::HereDoc => None,
        };
        if let Some(chr) = required_chr {
            if self.input().starts_with(chr) {
                self.advance(1);
            } else {
                self.state = bak_state_str;
                return Err(SyntaxError::None);
            }
        }
        // match a required newline after the "header" of the doc
        if !self.newline() {
            self.state = bak_state_str;
            return Err(SyntaxError::None);
        }

        // NOWDOC behaves roughly like sq_string and HEREDOC like dq_string
        let mut bytes: Vec<u8> = vec![];
        let mut parts = vec![];

        let is_now_doc = match doc_ty {
            DocType::NowDoc => true,
            _ => false,
        };

        // match characters until we find the required end_tag
        let end_tag = label;
        loop {
            let end_pos = match self.input()
                .chars()
                .position(|x| x == '\\' || x == '$' || x == '\n' || x == '{') {
                Some(end_pos) => end_pos,
                None => self.input().len(),
            };
            bytes.extend(self.advance(end_pos).as_bytes());

            match (self.input().chars().nth(0), is_now_doc) {
                (Some('\n'), _) => {
                    self.advance(1);
                    self.state.next_line();
                    // we are done if we are followed by our end-tag
                    if self.input().starts_with(&end_tag) {
                        self.advance(end_tag.len());
                        if self.input().starts_with(';') {
                            self.advance(1);
                        }
                        if !self.newline() {
                            let old_pos = self.input_pos();
                            self.state = bak_state_str;
                            return Err(SyntaxError::Unterminated("Here/Nowdoc: end-tag requires to be followed by a newline", mk_span(self.state.src_pos, old_pos)));
                        }
                        break
                    } else {
                        bytes.push(b'\n');
                    }
                }
                (Some('\\'), _) => try!(self.str_escape(&mut bytes, is_now_doc)),
                (Some('$'), false) => self.str_variable(&mut bytes, &mut parts),
                (Some('{'), false) => self.str_block(&mut bytes, &mut parts, true),
                _ => {
                    let old_pos = self.input_pos();
                    self.state = bak_state_str;
                    return Err(SyntaxError::Unterminated("Here/Nowdoc", mk_span(self.state.src_pos, old_pos)));
                }
            }
        }
        if is_now_doc {
            assert!(parts.is_empty());
            let ret_token = match String::from_utf8(bytes) {
                Ok(str_) => Token::ConstantEncapsedString(self.interner.intern(&str_)),
                Err(err) => Token::BinaryCharSequence(Rc::new(err.into_bytes())),
            };
            return Ok(TokenSpan(ret_token, mk_span(bak_state_str.src_pos, self.input_pos())));
        }
        let current_pos = self.input_pos();
        Ok(self.return_tokens_from_parts(
            TokenSpan(Token::HereDocStart, mk_span(bak_state_str.src_pos, bak_state_str.src_pos + 1)),
            TokenSpan(Token::HereDocEnd, mk_span(current_pos - 1, current_pos)),
            bytes, parts
        ))
    }

    /// match a comment
    pub fn match_comments(&mut self) -> Result<TokenSpan, SyntaxError> {
        let old_pos = self.input_pos();
        let mut doc_comment = false;
        // single line comment
        let start_tokens_count = if self.input().starts_with('#') {
            1
        } else if self.input().starts_with("//") {
            2
        } else {
            0
        };
        let comment = if start_tokens_count > 0 {
                self.advance(start_tokens_count);
                let end_pos = match self.input().chars().position(|x| x == '\n') {
                    Some(end_pos) => end_pos,
                    None => self.input().len(),
                };
                self.advance(end_pos)
            } else {
                // block comment
                let start_tokens_count = if self.input().starts_with("/*") {
                    2
                } else if self.input().starts_with("/**") {
                    doc_comment = true;
                    3
                } else {
                    0
                };
                if start_tokens_count > 0 {
                    self.advance(start_tokens_count);
                    let end_pos = match self.input().find("*/") {
                        Some(end_pos) => end_pos,
                        None => {
                            let old_pos = self.state.src_pos;
                            self.state.src_pos = old_pos;
                        	return Err(SyntaxError::Unterminated("comment", mk_span(self.input_pos(), old_pos)));
                    	}
                    };
                    let ret = self.advance(end_pos);
                    self.advance(2);
                    ret
            } else {
                return Err(SyntaxError::None);
            }
        }.to_owned();
        for _ in 0..comment.lines().count().checked_sub(1).unwrap_or(0) {
            self.state.next_line()
        }
        let mut span = mk_span(old_pos, self.input_pos());
        if doc_comment {
            // For a doc comment the parser'll use the content of Token::Comment as doc_comment
            // for this specific case `.doc_comment` merely acts as a flag
            span.doc_comment = Some("".to_owned());
        }
        Ok(TokenSpan(Token::Comment(self.interner.intern(&comment)), span))
    }

    /// Try to parse a token depending on the current state
    pub fn next_token(&mut self) -> Result<TokenSpan, SyntaxError> {
        loop {
            let ret = match self.state.state {
                State::Done | State::DoNothing => Err(SyntaxError::None),
                State::Initial => self.initial_token(),
                State::InScripting => self.in_scripting_token(),
                State::LookingForProperty => self.looking_for_property_token(),
                /// this state allows returning multiple tokens (for e.g. string fragments)
                State::EmitQueue => {
                    match self.queue.pop() {
                        Some(x) => Ok(x),
                        None => {
                            state_helper!(pop, self);
                            self.state.restart = true;
                            continue;
                        }
                    }
                }
            };
            match ret {
                Err(SyntaxError::None) if self.state.restart => {
                    self.state.restart = false;
                    continue;
                }
                _ => return ret,
            }
        }
    }

    /// token scanner for initial-state
    fn initial_token(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().is_empty() {
            self.state.state = State::Done;
            return Ok(TokenSpan(Token::End, mk_span(self.code.len(), self.code.len())));
        }
        ret_token!(match_token!(self, OpenTagWithEcho, state = InScripting));
        ret_token!(match_token!(self, OpenTag, state = InScripting));
        if self.short_tags {
            ret_token!(match_token_alias!(self, "<?", OpenTag, state = InScripting));
        }
        // read inline HTML until PHP starttag (keep last)
        let mut end_pos = 0;
        {
            let mut input = self.input();
            loop {
                let (is_php_tag, next_pos) = match input.find("<?") {
                    None => (false, input.len()),
                    Some(x) => (true, x),
                };
                let is_php_tag = if is_php_tag && self.short_tags {
                    true
                } else {
                    input.starts_with("<?=") || input.starts_with_ci("<?php")
                };
                if is_php_tag || next_pos == input.len() {
                    end_pos += next_pos;
                    break;
                }
                // move on (skip) on <? with short-tags disabled and no PHP "open-suffix" (= or php)
                input = &input[next_pos + 2..];
            }
        }
        if end_pos != 0 {
            let span = mk_span(self.input_pos(), self.input_pos() + end_pos);
            let str_ = self.advance(end_pos);
            return Ok(TokenSpan(Token::InlineHtml(self.interner.intern(str_)), span));
        }
        Ok(TokenSpan(Token::End, mk_span(self.code.len(), self.code.len())))
    }

    /// token scanner for script-seciton
    fn in_scripting_token(&mut self) -> Result<TokenSpan, SyntaxError> {
        self.whitespace();
        // check if we are at the end of the input
        if self.input().is_empty() {
            return Ok(TokenSpan(Token::End, mk_span(self.code.len(), self.code.len())));
        }
        let mut ret = vec![];
        let old_pos = self.input_pos();
        ret_token!(self.match_here_now_doc());
        ret_token!(self._bnum());
        ret_token!(self._hnum());
        ret_token!(self._onum_dnum_lnum());
        ret_token!(self.match_dq_string());
        ret_token!(self.match_variable());
        ret_token!(self.match_sq_string());
        ret_token!(self.match_comments());
        ret_token!(self.match_backquote());
        self.state.src_pos = old_pos;

        if let Some((label, span)) = self._label() {
            ret.push(Ok(TokenSpan(Token::String(self.interner.intern(label)), span)));
        }
        self.state.src_pos = old_pos;
        ret.push(self.in_scripting_other_token());

        // we return either the longest success or longest error
        // being the longest means, having the furthest position
        let mut longest = (0, None);
        let mut longest_err = (0, None);
        for r in ret {
            match r {
                Ok(token) => if longest.0 <= token.1.end {
                    longest = (token.1.end, Some(token));
                },
                Err(SyntaxError::None) => (),
                Err(err) => if longest_err.0 <= err.span().end {
                    longest_err = (err.span().end, Some(err));
                }
            }
        }
        if let Some(ret) = longest.1 {
            self.state.src_pos = longest.0 as usize;
            return Ok(ret);
        }
        if let Some(err) = longest_err.1 {
            self.state.src_pos = longest_err.0 as usize;
            return Err(err);
        }
        Err(SyntaxError::UnknownCharacter(mk_span(self.input_pos(), self.input_pos() + 1)))
    }

    fn in_scripting_other_token(&mut self) -> Result<TokenSpan, SyntaxError> {
        ret_token!(match_token!(self, CloseTag, state = Initial));
        ret_token!(match_token!(self, Exit));
        ret_token!(match_token_alias!(self, "die", Exit));
        ret_token!(match_token!(self, Function));
        ret_token!(match_token!(self, Const));
        ret_token!(match_token!(self, Return));
        ret_token!({
            let keyword = "yield";
            if self.input().starts_with_ci(keyword) {
                let old_pos = self.input_pos();
                self.advance(keyword.len());
                // yield_from submatch
                self.whitespace();
                let keyword = "from";
                if self.input().starts_with_ci(keyword) {
                    self.advance(keyword.len());
                    let span = mk_span(old_pos, self.input_pos());
                    Ok(TokenSpan(Token::YieldFrom, span))
                } else {
                    let span = mk_span(old_pos, self.input_pos());
                    Ok(TokenSpan(Token::Yield, span))
                }
            } else {
                Err(SyntaxError::None)
            }
        });
        ret_token!(match_token!(self, Try));
        ret_token!(match_token!(self, Catch));
        ret_token!(match_token!(self, Finally));
        ret_token!(match_token!(self, Throw));
        ret_token!(match_token!(self, If));
        ret_token!(match_token!(self, ElseIf));
        ret_token!(match_token!(self, EndIf));
        ret_token!(match_token!(self, Else));
        ret_token!(match_token!(self, While));
        ret_token!(match_token!(self, EndWhile));
        ret_token!(match_token!(self, Do));
        ret_token!(match_token!(self, Foreach));
        ret_token!(match_token!(self, EndForeach));
        ret_token!(match_token!(self, For));
        ret_token!(match_token!(self, Endfor));
        ret_token!(match_token!(self, Declare));
        ret_token!(match_token!(self, EndDeclare));
        ret_token!(match_token!(self, InstanceOf));
        ret_token!(match_token!(self, As));
        ret_token!(match_token!(self, Switch));
        ret_token!(match_token!(self, EndSwitch));
        ret_token!(match_token!(self, Case));
        ret_token!(match_token!(self, Default));
        ret_token!(match_token!(self, Break));
        ret_token!(match_token!(self, Continue));
        ret_token!(match_token!(self, Goto));
        ret_token!(match_token!(self, Echo));
        ret_token!(match_token!(self, Print));
        ret_token!(match_token!(self, Class));
        ret_token!(match_token!(self, Interface));
        ret_token!(match_token!(self, Trait));
        ret_token!(match_token!(self, Extends));
        ret_token!(match_token!(self, Implements));
        ret_token!(match_token!(self, ObjectOp, state <- LookingForProperty));
        ret_token!(match_token!(self, ScopeOp));
        ret_token!(match_token!(self, NsSeparator));
        ret_token!(match_token!(self, Ellipsis));
        ret_token!(match_token!(self, Coalesce));
        ret_token!(match_token!(self, New));
        ret_token!(match_token!(self, Clone));
        ret_token!(match_token!(self, Var));

        // match cast tokens, all in one-try
        if self.input().starts_with('(') {
            #[inline]
            fn try_determine_cast_type(self_: &mut Tokenizer) -> Result<TokenSpan, SyntaxError> {
                ret_token!(match_token!(self_, CastInt));
                ret_token!(match_token_alias!(self_, "int", CastInt));
                ret_token!(match_token_alias!(self_, "real", CastDouble));
                ret_token!(match_token!(self_, CastDouble));
                ret_token!(match_token_alias!(self_, "float", CastDouble));
                ret_token!(match_token!(self_, CastString));
                ret_token!(match_token_alias!(self_, "binary", CastString));
                ret_token!(match_token!(self_, CastArray));
                ret_token!(match_token!(self_, CastObject));
                ret_token!(match_token_alias!(self_, "boolean", CastBool));
                ret_token!(match_token!(self_, CastBool));
                ret_token!(match_token!(self_, CastUnset));
                Err(SyntaxError::None)
            }
            let old_pos = self.input_pos();
            self.advance(1);
            self.whitespace_only();
            if let Ok(ret) = try_determine_cast_type(self) {
                self.whitespace_only();
                if self.input().starts_with(')') {
                    self.advance(1);
                    return Ok(TokenSpan(ret.0, mk_span(old_pos, self.input_pos())));
                }
            }
            // restore the position if we didn't match a catch
            self.state.src_pos = old_pos;
        }
        ret_token!(match_token!(self, Eval));
        ret_token!(match_token!(self, IncludeOnce));
        ret_token!(match_token!(self, Include));
        ret_token!(match_token!(self, RequireOnce));
        ret_token!(match_token!(self, Require));
        ret_token!(match_token!(self, Namespace));
        ret_token!(match_token!(self, Use));
        ret_token!(match_token!(self, Insteadof));
        ret_token!(match_token!(self, Global));
        ret_token!(match_token!(self, Isset));
        ret_token!(match_token!(self, Empty));
        ret_token!(match_token!(self, HaltCompiler));
        ret_token!(match_token!(self, Static));
        ret_token!(match_token!(self, Abstract));
        ret_token!(match_token!(self, Final));
        ret_token!(match_token!(self, Private));
        ret_token!(match_token!(self, Protected));
        ret_token!(match_token!(self, Public));
        ret_token!(match_token!(self, Unset));
        ret_token!(match_token!(self, DoubleArrow));
        ret_token!(match_token!(self, List));
        ret_token!(match_token!(self, Array));
        ret_token!(match_token!(self, Callable));
        ret_token!(match_token!(self, Increment));
        ret_token!(match_token!(self, Decrement));
        ret_token!(match_token!(self, IsIdentical));
        ret_token!(match_token!(self, IsNotIdentical));
        ret_token!(match_token!(self, IsEqual));
        ret_token!(match_token!(self, IsNotEqual));
        ret_token!(match_token_alias!(self, "<>", IsNotEqual));
        ret_token!(match_token!(self, SpaceShip));
        ret_token!(match_token!(self, IsSmallerOrEqual));
        ret_token!(match_token!(self, IsGreaterOrEqual));
        ret_token!(match_token!(self, PlusEqual));
        ret_token!(match_token!(self, MinusEqual));
        ret_token!(match_token!(self, MulEqual));
        ret_token!(match_token!(self, Pow));
        ret_token!(match_token!(self, PowEqual));
        ret_token!(match_token!(self, DivEqual));
        ret_token!(match_token!(self, ConcatEqual));
        ret_token!(match_token!(self, ModEqual));
        ret_token!(match_token!(self, SlEqual));
        ret_token!(match_token!(self, SrEqual));
        ret_token!(match_token!(self, AndEqual));
        ret_token!(match_token!(self, OrEqual));
        ret_token!(match_token!(self, XorEqual));
        ret_token!(match_token!(self, BoolOr));
        ret_token!(match_token!(self, BoolAnd));
        ret_token!(match_token!(self, LogicalOr));
        ret_token!(match_token!(self, LogicalAnd));
        ret_token!(match_token!(self, LogicalXor));
        ret_token!(match_token!(self, Sl));
        ret_token!(match_token!(self, Sr));
        ret_token!(match_token!(self, CurlyBracesOpen, state <- InScripting));
        ret_token!(match match_token!(self, CurlyBracesClose, state ->) {
            Ok(TokenSpan(token, mut span)) => {
                // equivalent of RESET_DOC_COMMENT()
                span.doc_comment = Some("".to_owned());
                Ok(TokenSpan(token, span))
            }
            x => x,
        }); //TODO: stack is allowed to be empty! (dont fail once error handling is implemented)
        ret_token!(match_token!(self, MagicClass));
        ret_token!(match_token!(self, MagicTrait));
        ret_token!(match_token!(self, MagicFunction));
        ret_token!(match_token!(self, MagicMethod));
        ret_token!(match_token!(self, MagicLine));
        ret_token!(match_token!(self, MagicFile));
        ret_token!(match_token!(self, MagicDir));
        ret_token!(match_token!(self, MagicNamespace));
        ret_token!(self._token()); //{TOKENS}, keep this last
        Err(SyntaxError::UnknownCharacter(mk_span(self.input_pos(), self.input_pos() + 1)))
    }

    /// token-scanner for looking-for-property state
    fn looking_for_property_token(&mut self) -> Result<TokenSpan, SyntaxError> {
        self.whitespace();
        ret_token!(match_token!(self, ObjectOp));
        match self._label().map(|(x, span)| (self.interner.intern(x), span)) {
            None => (),
            Some((x, span)) => {
                state_helper!(pop, self);
                return Ok(TokenSpan(Token::String(x), span));
            }
        }
        // ANY_CHAR: pop_state
        self.pop_state();
        self.state.restart = true;
        Err(SyntaxError::None)
    }
}

#[cfg(test)]
mod tests {
    use super::State;
    use super::*;

    macro_rules! assert_eq_tok {
        ($a:expr, $b:expr) => {assert_eq!($a.map(|x| x.0), $b)};
    }

    #[inline]
    fn get_n_tokens(tokenizer: &mut Tokenizer, n: usize) -> Vec<Result<Token, SyntaxError>> {
        (0..n).map(|_| tokenizer.next_token().map(|x| x.0)).collect()
    }

    #[test]
    fn simple_whitespace_line_num() {
        let mut tokenizer = Tokenizer::new("<?php   \n  \n  \n?>");
        assert_eq_tok!(tokenizer.next_token(), Ok(Token::OpenTag));
        assert_eq_tok!(tokenizer.next_token(), Ok(Token::CloseTag));
        assert_eq!(tokenizer.state.line_num, 4);
    }

    #[test]
    fn simple_object_operator() {
        let mut tokenizer = Tokenizer::new("<?php  ->test ?>");
        assert_eq!(get_n_tokens(&mut tokenizer, 3), vec![Ok(Token::OpenTag), Ok(Token::ObjectOp), Ok(Token::String("test".into()))]);
        let mut tokenizer = Tokenizer::new("<?php  ->test->gest2 ?>");
        assert_eq!(get_n_tokens(&mut tokenizer, 5), vec![Ok(Token::OpenTag), Ok(Token::ObjectOp), Ok(Token::String("test".into())),
            Ok(Token::ObjectOp), Ok(Token::String("gest2".into()))
        ]);
    }

    #[test]
    fn simple_cast() {
        let mut tokenizer = Tokenizer::new("<?php  (string) ?>");
        assert_eq!(get_n_tokens(&mut tokenizer, 3),
                   vec![Ok(Token::OpenTag), Ok(Token::CastString), Ok(Token::CloseTag)]);
    }

    #[test]
    fn simple_scripting_initial_state_test() {
        let mut tokenizer = Tokenizer::new("<?php");
        let result = tokenizer.next_token();
        assert_eq!(tokenizer.state.state, State::InScripting);
        assert_eq_tok!(result, Ok(Token::OpenTag));
        tokenizer = Tokenizer::new("<?php?>");
        assert_eq!(tokenizer.state.state, State::Initial);
        assert_eq_tok!(tokenizer.next_token(), Ok(Token::OpenTag));
        assert_eq!(tokenizer.state.state, State::InScripting);
        assert_eq_tok!(tokenizer.next_token(), Ok(Token::CloseTag));
        assert_eq!(tokenizer.state.state, State::Initial);
    }

    #[test]
    fn scripting_initial_text() {
        let mut tokenizer = Tokenizer::new("a?>b<?php?>");
        assert_eq!(get_n_tokens(&mut tokenizer, 3), vec![Ok(Token::InlineHtml("a?>b".into())), Ok(Token::OpenTag), Ok(Token::CloseTag)]);
    }

    #[test]
    fn simple_lnum() {
        let mut tokenizer = Tokenizer::new("<?php  42");
        assert_eq!(get_n_tokens(&mut tokenizer, 2),
                   vec![Ok(Token::OpenTag), Ok(Token::Int(42))]);
    }

    #[test]
    fn simple_onum() {
        let mut tokenizer = Tokenizer::new("<?php  0144");
        assert_eq!(get_n_tokens(&mut tokenizer, 2),
                   vec![Ok(Token::OpenTag), Ok(Token::Int(100))]);
    }

    #[test]
    fn simple_bnum() {
        let mut tokenizer = Tokenizer::new("<?php  0b101");
        assert_eq!(get_n_tokens(&mut tokenizer, 2),
                   vec![Ok(Token::OpenTag), Ok(Token::Int(5))]);
    }

    #[test]
    fn simple_hnum() {
        let mut tokenizer = Tokenizer::new("<?php  0xff");
        assert_eq!(get_n_tokens(&mut tokenizer, 2),
                   vec![Ok(Token::OpenTag), Ok(Token::Int(255))]);
    }

    #[test]
    fn simple_dnum() {
        let mut tokenizer = Tokenizer::new("<?php  .10");
        assert_eq!(get_n_tokens(&mut tokenizer, 2),
                   vec![Ok(Token::OpenTag), Ok(Token::Double(0.1))]);
        let mut tokenizer = Tokenizer::new("<?php  1.");
        assert_eq!(get_n_tokens(&mut tokenizer, 2),
                   vec![Ok(Token::OpenTag), Ok(Token::Double(1.0))]);
        let mut tokenizer = Tokenizer::new("<?php  1.1");
        assert_eq!(get_n_tokens(&mut tokenizer, 2),
                   vec![Ok(Token::OpenTag), Ok(Token::Double(1.1))]);
    }

    #[test]
    fn dq_string() {
        let mut tokenizer = Tokenizer::new("<?php \"testhallo\\nwelt\"");
        assert_eq!(get_n_tokens(&mut tokenizer, 4),
                   vec![Ok(Token::OpenTag),
                        Ok(Token::DoubleQuote),
                        Ok(Token::ConstantEncapsedString("testhallo\nwelt".into())),
                        Ok(Token::DoubleQuote)]);
        let mut tokenizer = Tokenizer::new(r#"<?php "testhallo\"welt\"""#);
        assert_eq!(get_n_tokens(&mut tokenizer, 3),
                   vec![Ok(Token::OpenTag),
                        Ok(Token::DoubleQuote),
                        Ok(Token::ConstantEncapsedString("testhallo\"welt\"".into()))]);
        // check if we can handle UTF-8 without crashing
        let mut tokenizer = Tokenizer::new("<?php \"转\\t注\\t字\"");
        assert_eq!(get_n_tokens(&mut tokenizer, 3),
                   vec![Ok(Token::OpenTag),
                        Ok(Token::DoubleQuote),
                        Ok(Token::ConstantEncapsedString("转\t注\t字".into()))]);
    }

    #[test]
    fn sq_string() {
        let mut tokenizer = Tokenizer::new("<?php 'testhallo\\nwelt \\'g\\''");
        assert_eq!(get_n_tokens(&mut tokenizer, 2),
                   vec![Ok(Token::OpenTag),
                        Ok(Token::ConstantEncapsedString("testhallo\\nwelt 'g'".into()))]);
    }

    #[test]
    fn dq_string_var() {
        let mut tokenizer = Tokenizer::new("<?php \"ab $world cd\"");
        assert_eq!(get_n_tokens(&mut tokenizer, 6),
                   vec![Ok(Token::OpenTag),
                        Ok(Token::DoubleQuote),
                        Ok(Token::ConstantEncapsedString("ab ".into())),
                        Ok(Token::Variable("world".into())),
                        Ok(Token::ConstantEncapsedString(" cd".into())),
                        Ok(Token::DoubleQuote)]);
        let mut tokenizer = Tokenizer::new("<?php \"$world->ab\"");
        assert_eq!(get_n_tokens(&mut tokenizer, 6), vec![Ok(Token::OpenTag), Ok(Token::DoubleQuote), Ok(Token::Variable("world".into())), Ok(Token::ObjectOp),
            Ok(Token::String("ab".into())), Ok(Token::DoubleQuote),
        ]);
        let mut tokenizer = Tokenizer::new("<?php \"$world->ab->cd\"");
        assert_eq!(get_n_tokens(&mut tokenizer, 7),
                   vec![Ok(Token::OpenTag),
                        Ok(Token::DoubleQuote),
                        Ok(Token::Variable("world".into())),
                        Ok(Token::ObjectOp),
                        Ok(Token::String("ab".into())),
                        Ok(Token::ConstantEncapsedString("->cd".into())),
                        Ok(Token::DoubleQuote)]);
        let mut tokenizer = Tokenizer::new("<?php \"{$world->ab->cd}\"");
        assert_eq!(get_n_tokens(&mut tokenizer, 9),
                   vec![Ok(Token::OpenTag),
                        Ok(Token::DoubleQuote),
                        Ok(Token::CurlyBracesOpen),
                        Ok(Token::Variable("world".into())),
                        Ok(Token::ObjectOp),
                        Ok(Token::String("ab".into())),
                        Ok(Token::ObjectOp),
                        Ok(Token::String("cd".into())),
                        Ok(Token::CurlyBracesClose)]);
    }

    #[test]
    fn backquote() {
        let mut tokenizer = Tokenizer::new("<?php `ab $world cd`");
        assert_eq!(get_n_tokens(&mut tokenizer, 6),
                   vec![Ok(Token::OpenTag),
                        Ok(Token::Backquote),
                        Ok(Token::ConstantEncapsedString("ab ".into())),
                        Ok(Token::Variable("world".into())),
                        Ok(Token::ConstantEncapsedString(" cd".into())),
                        Ok(Token::Backquote)]);
    }

    #[test]
    fn heredoc() {
        let mut tokenizer = Tokenizer::new("<?php <<<EOT\ntest\nEOT;\n");
        assert_eq!(get_n_tokens(&mut tokenizer, 4),
                   vec![Ok(Token::OpenTag),
                        Ok(Token::HereDocStart),
                        Ok(Token::ConstantEncapsedString("test".into())),
                        Ok(Token::HereDocEnd)]);
        let mut tokenizer = Tokenizer::new("<?php <<<\"EOT\"\nte\\tst\nEOT;\n");
        assert_eq!(get_n_tokens(&mut tokenizer, 4),
                   vec![Ok(Token::OpenTag),
                        Ok(Token::HereDocStart),
                        Ok(Token::ConstantEncapsedString("te\tst".into())),
                        Ok(Token::HereDocEnd)]);
    }

    #[test]
    fn nowdoc() {
        let mut tokenizer = Tokenizer::new("<?php <<<'EOT'\nte\\tst\nEOT;\n");
        assert_eq!(get_n_tokens(&mut tokenizer, 2),
                   vec![Ok(Token::OpenTag), Ok(Token::ConstantEncapsedString("te\\tst".into()))]);
    }

    #[test]
    fn single_line_comment() {
        let mut tokenizer = Tokenizer::new("<?php //test");
        assert_eq!(get_n_tokens(&mut tokenizer, 2),
                   vec![Ok(Token::OpenTag), Ok(Token::Comment("test".into()))]);
        let mut tokenizer = Tokenizer::new("<?php //test\n$a");
        assert_eq!(get_n_tokens(&mut tokenizer, 2),
                   vec![Ok(Token::OpenTag), Ok(Token::Comment("test".into()))]);
    }

    #[test]
    fn block_comment() {
        let mut tokenizer = Tokenizer::new("<?php /* test\ntest2 */bb");
        assert_eq!(get_n_tokens(&mut tokenizer, 2),
                   vec![Ok(Token::OpenTag), Ok(Token::Comment(" test\ntest2 ".into()))]);
    }

    #[test]
    fn resolve_line_nums() {
        let mut tokenizer = Tokenizer::new("<?php \n\n\n/* test */");
        assert_eq!(get_n_tokens(&mut tokenizer, 1), vec![Ok(Token::OpenTag)]);
        match tokenizer.next_token() {
            Ok(TokenSpan(Token::Comment(comment), span)) => {
                assert_eq!(tokenizer.state.line_num, 4);
                assert_eq!(comment, " test ".into());
                println!("{:?}", span);
                assert_eq!(tokenizer.state.external.line_map.line_from_position(span.start as usize), 4 - 1); //0-based lines
            },
            _ => {assert!(false);},
        }
    }

    // TODO: use own error type?
    // TODO: error handling tests
}
