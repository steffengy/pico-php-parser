//! tokenizer based on https://github.com/php/php-src/blob/ebb99a1a3a2ec9216d95c63b267ae0f66074f4de/Zend/zend_language_scanner.l
//! since the reference doesn't seem very correct in some cases
use std::str::{self, FromStr};
use std::mem;

pub use tokens::{Span, Token, TokenSpan, SyntaxError};

#[inline]
fn mk_span(start: usize, end: usize) -> Span {
    Span { start: start as u32, end: end as u32, ..Span::new() }
}

#[derive(Clone, Debug, PartialEq)]
enum State {
    /// dummy state to force an ending after a `}` in InScripting
    DoNothing,
    Initial,
    InScripting,
    LookingForProperty,
    EmitQueue,
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    code: &'a str,
    /// whether to support short tags, equal to CG(short_tags)
    short_tags: bool,
    state: TokenizerState,
    queue: Vec<TokenSpan>,
}

#[derive(Debug, Clone)]
struct TokenizerState {
    src_pos: usize,
    state: State,
    state_stack: Vec<State>,
    line_num: usize,
    /// contains the first byte-position of every line
    line_map: Vec<u32>,
    restart: bool,
}

impl TokenizerState {
    #[inline]
    fn next_line(&mut self) {
        self.line_map.push(self.src_pos as u32);
        self.line_num += 1;
    }

    fn line_from_position(&self, pos: usize) -> usize {
        println!("{:?}", self.line_map);
        let mut b = self.line_map.len();
        let mut a = 0;
        while b - a > 1 {
            let mid = (a + b) / 2;
            if self.line_map[mid] as usize > pos { b = mid; } else { a = mid; }
        }
        a
    }
}

macro_rules! state_helper {
    (push, $self_:expr, $new_state:ident) => ({
        let old_state = mem::replace(&mut $self_.state.state, State::$new_state);
        $self_.state.state_stack.push(old_state);
    });
    (pop, $self_:expr) => ($self_.pop_state());
}

macro_rules! match_token {
    // set state syntax
    ($self_:expr, $e:expr, $token:ident, state=$new_state:ident) => {
        if $self_.input().starts_with($e) {
            let old_pos = $self_.state.src_pos;
            $self_.advance($e.len());
            let span = mk_span(old_pos, $self_.state.src_pos);
            $self_.state.state = State::$new_state;
            Ok(TokenSpan(Token::$token, span))
        } else { Err(SyntaxError::None) }
    };
    // state unchanged
    ($self_:expr, $e:expr, $token:ident) => {
        if $self_.input().starts_with($e) {
            let old_pos = $self_.state.src_pos;
            $self_.advance($e.len());
            let span = mk_span(old_pos, $self_.state.src_pos);
            Ok(TokenSpan(Token::$token, span))
        } else { Err(SyntaxError::None) }
    };
    // state push
    ($self_:expr, $e:expr, $token:ident, state<-$new_state:ident) => {
        if $self_.input().starts_with($e) {
            let old_pos = $self_.state.src_pos;
            $self_.advance($e.len());
            let span = mk_span(old_pos, $self_.state.src_pos);
            state_helper!(push, $self_, $new_state);
            Ok(TokenSpan(Token::$token, span))
        } else { Err(SyntaxError::None) }
    };
    // state pop
    ($self_:expr, $e:expr, $token:ident, state->) => {
        if $self_.input().starts_with($e) {
            let old_pos = $self_.state.src_pos;
            $self_.advance($e.len());
            let span = mk_span(old_pos, $self_.state.src_pos);
            state_helper!(pop, $self_);
            Ok(TokenSpan(Token::$token, span))
        } else { Err(SyntaxError::None) }
    }
}

/// return if a token was found, else continue in code flow
macro_rules! ret_token {
    ($e:expr) => (match $e {
        Ok(x) => return Ok(x),
        Err(SyntaxError::None) => (),
        x => return x,
    });
}

/// helper to transform string-members into the appropriate token
impl<'a> Tokenizer<'a> {
    pub fn new(src: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            code: src,
            state: TokenizerState {
                src_pos: 0,
                state: State::Initial,
                state_stack: vec![],
                line_num: 1,
                line_map: vec![0],
                restart: false,
            },
            short_tags: true,
            queue: vec![],
        }
    }

    /// advances by n-positions where a position is a char-index
    /// this returns a substring of the skipped part (old[..n])
    #[inline]
    fn advance(&mut self, n: usize) -> &str {
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
                ' ' | '\t' | '\r' => {self.advance(1);},
                '\n' => {
                    self.advance(1);
                    self.state.next_line();
                },
                _ => break
            }
        }
    }

    /// handle tabs and spaces
    fn whitespace_only(&mut self) {
        while !self.input().is_empty() {
            match self.input().chars().nth(0).unwrap() {
                ' ' | '\t' => {self.advance(1);},
                _ => break
            }
        }
    }

    /// match exactly one newline
    fn newline(&mut self) -> bool {
        let amount = if self.input().starts_with("\r\n") { 2 }
        else if self.input().starts_with('\n') || self.input().starts_with('\r') { 1 }
        else {
            return false;
        };
        self.advance(amount);
        self.state.next_line();
        true
    }

    // re2c stuff (mostly prefixed with _)
    fn _label(&mut self) -> Option<(&str, Span)> {
        if self.input().is_empty() {
            return None
        }
        if let Some('0'...'9') = self.input().chars().nth(0) {
            return None
        }

        //\u{c280} U+0080
        //\u{c3BF} U+00FF
        let end_pos = self.input().chars().position(|x| match x {
            'a'...'z' | 'A'...'Z' | '\u{c280}'...'\u{c3BF}' | '0'...'9' => false,
            _ => true,
        });
        let end_pos = match end_pos {
            Some(0) => return None,
            Some(x) => x,
            None => self.input().len()
        };
        let old_pos = self.input_pos();
        let ret = self.advance(end_pos);
        Some((ret, mk_span(old_pos, old_pos + end_pos)))
    }

    /// matches a token which consists of one character (simple)
    fn _token(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().is_empty() {
            return Err(SyntaxError::None);
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

    /// match a double number (or a long number)
    fn _dnum_lnum(&mut self) -> Result<TokenSpan, SyntaxError> {
        // valid inputs for double: "long.", ".long", "long.long"
        if self.input().is_empty() {
            return Err(SyntaxError::None);
        }
        let end_pos = match self.input().chars().position(|x| x < '0' || x > '9') {
            None => self.input().len(),
            Some(end_pos) => end_pos,
        };
        let old_pos = self.input_pos();
        let mut str_ = self.advance(end_pos).to_owned();
        if !self.input().starts_with('.') {
            {
                let span = mk_span(old_pos, self.input_pos());
                // long sub-match
                if end_pos != 0 {
                    return Ok(TokenSpan(Token::Int(i64::from_str_radix(&str_, 10).unwrap()), span))
                }
            }
            self.state.src_pos = old_pos;
            return Err(SyntaxError::None);
        }
        let mut str_ = str_ + self.advance(1);
        // at this point we either matched "long." or just "."
        let end_pos2 = match self.input().chars().position(|x| x < '0' || x > '9') {
            None => self.input().len(),
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
            _ => true
        }) {
            None => self.input().len(),
            Some(0) => return Err(SyntaxError::None),
            Some(end_pos) => end_pos
        };
        let span = mk_span(self.input_pos() - 2, end_pos);
        let str_ = self.advance(end_pos);
        Ok(TokenSpan(Token::Int(i64::from_str_radix(str_, 16).unwrap()), span))
    }

    /// match a binary number
    fn _bnum(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().len() < 3 || !self.input().starts_with("0b") {
            return Err(SyntaxError::None)
        }
        self.advance(2);
        let end_pos = match self.input().chars().position(|x| x != '0' && x != '1') {
            None => self.input().len(),
            Some(0) => return Err(SyntaxError::None),
            Some(end_pos) => end_pos
        };
        let span = mk_span(self.input_pos() - 2, end_pos);
        let str_ = self.advance(end_pos);
        Ok(TokenSpan(Token::Int(i64::from_str_radix(str_, 2).unwrap()), span))
    }

    /// matches ${label} so any valid variable_name
    fn match_variable(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().len() < 2 || !self.input().starts_with('$') {
            return Err(SyntaxError::None)
        }
        let bak_pos = self.input_pos();
        self.advance(1);
        match self._label().map(|(x, span)| (x.to_owned(), span)) {
            Some((name, mut span)) => {
                span.start = bak_pos as u32;
                Ok(TokenSpan(Token::Variable(name), span))
            },
            None => {
                self.state.src_pos = bak_pos;
                Err(SyntaxError::None)
            }
        }
    }

    /// matches ${label}[ and starts OFFSET_SCANNING or only ${label}
    /*fn match_variable_offset(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().len() < 2 {
            return Err(SyntaxError::None)
        }
        match self.match_variable() {
            Ok(token_span) => {
                Ok(if self.input().starts_with('[') {
                    state_helper!(push, self, VarOffset);
                    token_span
                } else {
                    token_span
                })
            },
            x => x,
        }
    }*/

    fn str_escape(&mut self, str_: &mut String, sq: bool) -> Result<(), SyntaxError> {
        let chr = match (self.input().chars().nth(1), sq) {
            (Some('n'), false) => '\n',
            (Some('r'), false) => '\r',
            (Some('t'), false) => '\t',
            (Some('f'), false) => '\x0C',
            (Some('v'), false) => '\x0B',
            (Some('e'), false) => unimplemented!(),
            (Some('"'), false) => '"',
            (Some('\''), true) => '\'',
            (Some('\\'), _) => '\\',
            (Some('$'), false) => '$',
            (Some('x'), false) | (Some('X'), false) => unimplemented!(),
            (Some('u'), false) => unimplemented!(),
            (Some(x), _) => {
                str_.push('\\');
                x
            },
            _ => return Err(SyntaxError::Unterminated("string escape sequence", mk_span(self.input_pos(), self.input_pos() + 1))),
        };
        str_.push(chr);
        self.advance(2);
        Ok(())
    }

    #[inline]
    fn return_tokens_from_parts(&mut self, start_tok: TokenSpan, end_tok: TokenSpan, str_: String, parts: Vec<TokenSpan>) -> TokenSpan {
        let (start_pos, end_pos) = (start_tok.1.start, end_tok.1.end);
        self.queue.push(end_tok);
        if !str_.is_empty() {
            self.queue.push(TokenSpan(Token::ConstantEncapsedString(str_), mk_span(start_pos as usize, end_pos as usize)));
        }
        if parts.len() > 0 {
            self.queue.extend(parts.into_iter().rev());
        }
        state_helper!(push, self, EmitQueue);
        start_tok
    }

    /// matches a single-quoted string literal
    fn match_sq_string(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().len() < 2 {
            return Err(SyntaxError::None)
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
        let mut str_ = String::new();
        loop {
            let end_pos = match self.input().chars().position(|x| x == '\\' || x == '\'' || x == '\n') {
                Some(end_pos) => end_pos,
                None => self.input().len(),
            };
            str_.push_str(self.advance(end_pos));
            match self.input().chars().nth(0) {
                Some('\n') => {
                    self.advance(1);
                    self.state.next_line();
                    str_.push('\n');
                },
                Some('\\') => try!(self.str_escape(&mut str_, true)),
                Some('\'') => {
                    self.advance(1);
                    break;
                },
                _ => {
                    let old_pos = self.state.src_pos;
                    self.state = bak_state;
                    return Err(SyntaxError::Unterminated("single-quoted string literal", mk_span(self.input_pos(), old_pos + 1)));
                }
            }
        }
        let span = mk_span(bak_state.src_pos, self.input_pos());
        Ok(TokenSpan(Token::ConstantEncapsedString(str_), span))
    }

    fn str_variable(&mut self, str_: &mut String, parts: &mut Vec<TokenSpan>) {
        self.advance(1);
        if self.input().starts_with("{") {
            panic!("unimplemented: T_DOLLAR_OPEN_CURLY_BRACES: ${ ... } syntax not supported yet");
        }
        // match variable
        if let Some((label, span)) = self._label().map(|(x, span)| (x.to_owned(), span)) {
            let mut tmp_parts = vec![];
            // match var_offset
            if self.input().starts_with('[') {
                let bak_state = self.state.clone();
                unimplemented!();
            }
            // match object access (only $var->label supported in PHP)
            else if self.input().starts_with("->") {
                let bak_pos = self.input_pos();
                self.advance(2);
                if let Some((property, span)) = self._label().map(|(x, span)| (x.to_owned(), span)) {
                    tmp_parts.push(TokenSpan(Token::ObjectOp, mk_span(bak_pos, bak_pos+1)));
                    tmp_parts.push(TokenSpan(Token::String(property), mk_span(span.start as usize, span.end as usize)));
                } else {
                    self.state.src_pos = bak_pos;
                }
            }
            // and match the single variable, prepend it
            if !str_.is_empty() {
                parts.push(TokenSpan(Token::ConstantEncapsedString(mem::replace(str_, String::new())), mk_span(span.start as usize -str_.len(), span.start as usize)));
            }
            parts.push(TokenSpan(Token::Variable(label), mk_span(span.start as usize -1, span.end as usize)));
            parts.extend(tmp_parts);
        } else {
            str_.push('$');
        }
    }

    fn str_block(&mut self, str_: &mut String, parts: &mut Vec<TokenSpan>) {
        self.advance(1);
        if self.input().starts_with('$') {
            let bak_state = self.state.clone();
            // temporary state transition to use the same instance to match
            self.state.state = State::InScripting;
            self.state.state_stack = vec![State::DoNothing];
            let mut tokens = vec![TokenSpan(Token::CurlyBracesOpen, mk_span(self.input_pos()-1, self.input_pos()))];
            while let Ok(tok) = self.next_token() {
                tokens.push(tok);
            }
            if let Some(&TokenSpan(Token::CurlyBracesClose, _)) = tokens.last() {
                parts.extend(tokens);
                // undo the temporary tokenizer state transition
                self.state.state = bak_state.state;
                self.state.state_stack = bak_state.state_stack;
            } else {
                self.state = bak_state;
            }
        } else {
            str_.push('{');
        }
    }

    /// matches a double-quoted string literal
    fn match_dq_string(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().len() < 2 {
            return Err(SyntaxError::None)
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
        let mut str_ = String::new();
        loop {
            let end_pos = match self.input().chars().position(|x| x == '\\' || x == '"' || x == '$' || x == '\n' || x == '{') {
                Some(end_pos) => end_pos,
                None => self.input().len() - 1,
            };
            str_.push_str(self.advance(end_pos));

            match self.input().chars().nth(0) {
                Some('\n') => {
                    self.advance(1);
                    self.state.next_line();
                    str_.push('\n');
                },
                Some('\\') => try!(self.str_escape(&mut str_, false)),
                Some('"') => {
                    self.advance(1);
                    break
                },
                Some('$') => self.str_variable(&mut str_, &mut parts),
                // match {$<IN_SCRIPTING>} block
                Some('{') => self.str_block(&mut str_, &mut parts),
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
            str_, parts
        ))
    }

    /// backquote handling
    fn match_backquote(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().len() < 2 {
            return Err(SyntaxError::None)
        }
        let bak_state_str = if self.input().starts_with('`') {
            let bak_state = self.state.clone();
            self.advance(1);
            bak_state
        } else {
            return Err(SyntaxError::None);
        };
        let mut parts = vec![];
        let mut str_ = String::new();
        loop {
            let end_pos = match self.input().chars().position(|x| x == '\\' || x == '"' || x == '$' || x == '\n' || x == '{') {
                Some(end_pos) => end_pos,
                None => self.input().len() - 1,
            };
            str_.push_str(self.advance(end_pos));

            match self.input().chars().nth(0) {
                Some('\n') => {
                    self.advance(1);
                    self.state.next_line();
                    str_.push('\n');
                },
                Some('`') => {
                    self.advance(1);
                    break
                },
                Some('$') => self.str_variable(&mut str_, &mut parts),
                // match {$<IN_SCRIPTING>} block
                Some('{') => self.str_block(&mut str_, &mut parts),
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
            str_, parts
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
        let label = if let Some(label) = self._label().map(|x| x.0.to_owned()) { label } else {
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
        let mut str_ = String::new();
        let mut parts = vec![];

        let is_now_doc = match doc_ty {
            DocType::NowDoc => true,
            _ => false
        };

        // match characters until we find the required end_tag
        let end_tag = label;
        loop {
            let end_pos = match self.input().chars().position(|x| x == '\\' || x == '"' || x == '$' || x == '\n' || x == '{') {
                Some(end_pos) => end_pos,
                None => self.input().len(),
            };
            str_.push_str(self.advance(end_pos));

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
                        break
                    } else {
                        str_.push('\n');
                    }
                },
                (Some('\\'), _) => try!(self.str_escape(&mut str_, is_now_doc)),
                (Some('$'), false) => self.str_variable(&mut str_, &mut parts),
                (Some('{'), false) => self.str_block(&mut str_, &mut parts),
                _ => {
                    let old_pos = self.input_pos();
                    self.state = bak_state_str;
                    return Err(SyntaxError::Unterminated("Here/Nowdoc", mk_span(self.state.src_pos, old_pos)));
                }
            }
        }
        let span = mk_span(bak_state_str.src_pos, self.input_pos() - 1);
        if is_now_doc {
            assert!(parts.is_empty());
            return Ok(TokenSpan(Token::ConstantEncapsedString(str_), mk_span(bak_state_str.src_pos, self.input_pos())));
        }
        let current_pos = self.input_pos();
        Ok(self.return_tokens_from_parts(
            TokenSpan(Token::HereDocStart, mk_span(bak_state_str.src_pos, bak_state_str.src_pos + 1)),
            TokenSpan(Token::HereDocEnd, mk_span(current_pos - 1, current_pos)),
            str_, parts
        ))
    }

    /// match a comment
    pub fn match_comments(&mut self) -> Result<TokenSpan, SyntaxError> {
        let old_pos = self.input_pos();
        let mut doc_comment = false;
        // single line comment
        let start_tokens_count =
            if self.input().starts_with('#') { 1 }
            else if self.input().starts_with("//") { 2 }
        else { 0 };
        let end_pos = if start_tokens_count > 0 {
            self.advance(start_tokens_count);
            match self.input().chars().position(|x| x == '\n') {
                Some(end_pos) => end_pos,
                None => self.input().len(),
            }
        } else {
            // block comment
            let start_tokens_count =
                if self.input().starts_with("/*") { 2 }
                else if self.input().starts_with("/**") { doc_comment = true; 3 }
            else { 0 };
            if start_tokens_count > 0 {
                self.advance(start_tokens_count);
                match self.input().find("*/") {
                    Some(end_pos) => end_pos,
                    None => {
                        let old_pos = self.state.src_pos;
                        self.state.src_pos = old_pos;
                        return Err(SyntaxError::Unterminated("comment", mk_span(self.input_pos(), old_pos)));
                    },
                }
            } else {
                return Err(SyntaxError::None);
            }
        };
        let comment = self.advance(end_pos).to_owned();
        let mut span = mk_span(old_pos, self.input_pos());
        if doc_comment {
            // For a doc comment the parser'll use the content of Token::Comment as doc_comment
            // for this specific case `.doc_comment` merely acts as a flag
            span.doc_comment = Some("".to_owned());
        }
        return Ok(TokenSpan(Token::Comment(comment), span));
    }

    /// Try to parse a token depending on the current state
    pub fn next_token(&mut self) -> Result<TokenSpan, SyntaxError> {
        loop {
            let ret = match self.state.state {
                State::Initial => self.initial_token(),
                State::InScripting => self.in_scripting_token(),
                State::LookingForProperty => self.looking_for_property_token(),
                State::DoNothing => Err(SyntaxError::None),
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
                },
                _ => return ret,
            }
        }
    }

    /// token scanner for initial-state
    /// TODO: maybe optimize me
    fn initial_token(&mut self) -> Result<TokenSpan, SyntaxError> {
        if self.input().len() < 2 {
            return Ok(TokenSpan(Token::End, mk_span(self.code.len(), self.code.len())));
        }
        ret_token!(match_token!(self, "<?=", OpenTagWithEcho, state = InScripting));
        ret_token!(match_token!(self, "<?php", OpenTag, state = InScripting));
        if self.short_tags {
            ret_token!(match_token!(self, "<?", OpenTag, state = InScripting));
        }
        // read inline HTML until PHP starttag (keep last)
        let mut end_pos = 0;
        {
            let mut input = self.input();
            loop {
                match input.find("<?") {
                    None => break,
                    Some(x) => {
                        let is_php_tag = if self.short_tags { true } else {
                            input[x+2..].starts_with("=") ||
                            input[x+2..].starts_with("php")
                        };
                        if is_php_tag {
                            end_pos += x;
                            break
                        }
                        input = &input[x+2..]
                    }
                }
            }
        }
        if end_pos != 0 {
            let span = mk_span(self.input_pos(), end_pos);
            let str_ = self.advance(end_pos);
            return Ok(TokenSpan(Token::InlineHtml(str_.to_owned()), span));
        }
        Err(SyntaxError::None)
    }

    /// token scanner for script-seciton
    fn in_scripting_token(&mut self) -> Result<TokenSpan, SyntaxError> {
        // check if we are at the end of the input
        if self.input().is_empty() {
            return Err(SyntaxError::None)
        }
        self.whitespace();
        ret_token!(self.match_variable());
        ret_token!(self.match_dq_string());
        ret_token!(self.match_sq_string());
        ret_token!(self.match_here_now_doc());
        ret_token!(self.match_comments());
        ret_token!(self.match_backquote());

        ret_token!(match_token!(self, "?>", CloseTag, state = Initial));
        ret_token!(match_token!(self, "exit", Exit));
        ret_token!(match_token!(self, "die", Exit));
        ret_token!(match_token!(self, "function", Function));
        ret_token!(match_token!(self, "const", Const));
        ret_token!(match_token!(self, "return", Return));
        ret_token!({
            let keyword = "yield";
            if self.input().starts_with(keyword) {
                let old_pos = self.input_pos();
                self.advance(keyword.len());
                // yield_from submatch
                self.whitespace();
                let keyword = "from";
                if self.input().starts_with(keyword) {
                    self.advance(keyword.len());
                    let span = mk_span(old_pos, self.input_pos());
                    Ok(TokenSpan(Token::YieldFrom, span))
                } else {
                    let span = mk_span(old_pos, self.input_pos());
                    Ok(TokenSpan(Token::Yield, span))
                }
            } else { Err(SyntaxError::None) }
        });
        ret_token!(match_token!(self, "catch",  Catch));
        ret_token!(match_token!(self, "finally",  Finally));
        ret_token!(match_token!(self, "throw",  Throw));
        ret_token!(match_token!(self, "if",  If));
        ret_token!(match_token!(self, "elseif",  ElseIf));
        ret_token!(match_token!(self, "endif",  EndIf));
        ret_token!(match_token!(self, "else",  Else));
        ret_token!(match_token!(self, "while",  While));
        ret_token!(match_token!(self, "endwhile",  EndWhile));
        ret_token!(match_token!(self, "do",  Do));
        ret_token!(match_token!(self, "for",  For));
        ret_token!(match_token!(self, "endfor",  Endfor));
        ret_token!(match_token!(self, "foreach",  Foreach));
        ret_token!(match_token!(self, "endforeach",  EndForeach));
        ret_token!(match_token!(self, "declare",  Declare));
        ret_token!(match_token!(self, "enddeclare",  EndDeclare));
        ret_token!(match_token!(self, "instanceof",  InstanceOf));
        ret_token!(match_token!(self, "as",  As));
        ret_token!(match_token!(self, "switch",  Switch));
        ret_token!(match_token!(self, "endswitch",  EndSwitch));
        ret_token!(match_token!(self, "case",  Case));
        ret_token!(match_token!(self, "default",  Default));
        ret_token!(match_token!(self, "break",  Break));
        ret_token!(match_token!(self, "continue",  Continue));
        ret_token!(match_token!(self, "goto",  Goto));
        ret_token!(match_token!(self, "echo",  Echo));
        ret_token!(match_token!(self, "print",  Print));
        ret_token!(match_token!(self, "class",  Class));
        ret_token!(match_token!(self, "interface",  Interface));
        ret_token!(match_token!(self, "trait",  Trait));
        ret_token!(match_token!(self, "extends",  Extends));
        ret_token!(match_token!(self, "implements",  Implements));
        ret_token!(match_token!(self, "->", ObjectOp, state <- LookingForProperty));
        ret_token!(match_token!(self, "::",  ScopeOp));
        ret_token!(match_token!(self, "\\",  NsSeparator));
        ret_token!(match_token!(self, "...",  Ellipsis));
        ret_token!(match_token!(self, "??",  Coalesce));
        ret_token!(match_token!(self, "new",  New));
        ret_token!(match_token!(self, "clone",  Clone));
        ret_token!(match_token!(self, "var",  Var));

        // match cast tokens, all in one-try
        if self.input().starts_with("(") {
            #[inline]
            fn try_determine_cast_type(self_: &mut Tokenizer) -> Result<TokenSpan, SyntaxError> {
                ret_token!(match_token!(self_, "integer", CastInt));
                ret_token!(match_token!(self_, "int", CastInt));
                ret_token!(match_token!(self_, "real", CastDouble));
                ret_token!(match_token!(self_, "double", CastDouble));
                ret_token!(match_token!(self_, "float", CastDouble));
                ret_token!(match_token!(self_, "string", CastString));
                ret_token!(match_token!(self_, "binary", CastString));
                ret_token!(match_token!(self_, "array", CastArray));
                ret_token!(match_token!(self_, "object", CastObject));
                ret_token!(match_token!(self_, "boolean", CastBool));
                ret_token!(match_token!(self_, "bool", CastBool));
                ret_token!(match_token!(self_, "unset", CastUnset));
                Err(SyntaxError::None)
            }
            let old_pos = self.input_pos();
            self.advance(1);
            self.whitespace_only();
            if let Ok(ret) = try_determine_cast_type(self) {
                self.whitespace_only();
                if self.input().starts_with(")") {
                    self.advance(1);
                    return Ok(TokenSpan(ret.0, mk_span(old_pos, self.input_pos())));
                }
            }
            // restore the position if we didn't match a catch
            self.state.src_pos = old_pos;
        }
        ret_token!(match_token!(self, "eval",  Eval));
        ret_token!(match_token!(self, "include",  Include));
        ret_token!(match_token!(self, "include_once",  IncludeOnce));
        ret_token!(match_token!(self, "require",  Require));
        ret_token!(match_token!(self, "require_once",  RequireOnce));
        ret_token!(match_token!(self, "namespace",  Namespace));
        ret_token!(match_token!(self, "use",  Use));
        ret_token!(match_token!(self, "insteadof",  Insteadof));
        ret_token!(match_token!(self, "global",  Global));
        ret_token!(match_token!(self, "empty",  Empty));
        ret_token!(match_token!(self, "__halt_compiler",  HaltCompiler));
        ret_token!(match_token!(self, "static",  Static));
        ret_token!(match_token!(self, "abstract",  Abstract));
        ret_token!(match_token!(self, "final",  Final));
        ret_token!(match_token!(self, "private",  Private));
        ret_token!(match_token!(self, "protected",  Protected));
        ret_token!(match_token!(self, "public",  Public));
        ret_token!(match_token!(self, "unset",  Unset));
        ret_token!(match_token!(self, "=>",  DoubleArrow));
        ret_token!(match_token!(self, "list",  List));
        ret_token!(match_token!(self, "array",  Array));
        ret_token!(match_token!(self, "callable",  Callable));
        ret_token!(match_token!(self, "++",  Increment));
        ret_token!(match_token!(self, "--",  Decrement));
        ret_token!(match_token!(self, "===",  IsIdentical));
        ret_token!(match_token!(self, "!==",  IsNotIdentical));
        ret_token!(match_token!(self, "==",  IsEqual));
        ret_token!(match_token!(self, "!=", IsNotEqual));
        ret_token!(match_token!(self, "<>", IsNotEqual));
        ret_token!(match_token!(self, "<=>",  SpaceShip));
        ret_token!(match_token!(self, "<=",  IsSmallerOrEqual));
        ret_token!(match_token!(self, ">=",  IsGreaterOrEqual));
        ret_token!(match_token!(self, "+=",  PlusEqual));
        ret_token!(match_token!(self, "-=",  MinusEqual));
        ret_token!(match_token!(self, "*=",  MulEqual));
        ret_token!(match_token!(self, "**",  Pow));
        ret_token!(match_token!(self, "**=",  PowEqual));
        ret_token!(match_token!(self, "/=",  DivEqual));
        ret_token!(match_token!(self, ".=",  ConcatEqual));
        ret_token!(match_token!(self, "%=",  ModEqual));
        ret_token!(match_token!(self, "<<=",  SlEqual));
        ret_token!(match_token!(self, ">>=",  SrEqual));
        ret_token!(match_token!(self, "&=",  AndEqual));
        ret_token!(match_token!(self, "|=",  OrEqual));
        ret_token!(match_token!(self, "^=",  XorEqual));
        ret_token!(match_token!(self, "||",  BoolOr));
        ret_token!(match_token!(self, "&&",  BoolAnd));
        ret_token!(match_token!(self, "OR",  LogicalOr));
        ret_token!(match_token!(self, "AND",  LogicalAnd));
        ret_token!(match_token!(self, "XOR",  LogicalXor));
        ret_token!(match_token!(self, "<<",  Sl));
        ret_token!(match_token!(self, ">>",  Sr));
        ret_token!(match_token!(self, "{", CurlyBracesOpen, state <- InScripting));
        ret_token!(match match_token!(self, "}", CurlyBracesClose, state ->) {
            Ok(TokenSpan(token, mut span)) => {
                // equivalent of RESET_DOC_COMMENT()
                span.doc_comment = Some("".to_owned());
                Ok(TokenSpan(token, span))
            },
            x => x,
        }); //TODO: stack is allowed to be empty! (dont fail once error handling is implemented)
        ret_token!(self._bnum());
        ret_token!(self._hnum());
        ret_token!(self._dnum_lnum());
        ret_token!(match_token!(self, "__CLASS__", MagicClass));
        ret_token!(match_token!(self, "__TRAIT__", MagicTrait));
        ret_token!(match_token!(self, "__FUNCTION__", MagicFunction));
        ret_token!(match_token!(self, "__METHOD__", MagicMethod));
        ret_token!(match_token!(self, "__LINE__", MagicLine));
        ret_token!(match_token!(self, "__FILE__", MagicFile));
        ret_token!(match_token!(self, "__DIR__", MagicDir));
        ret_token!(match_token!(self, "__NAMESPACE__", MagicNamespace));
        match self._label() {
            Some((label, span)) => return Ok(TokenSpan(Token::String(label.to_owned()), span)),
            None => (),
        };
        ret_token!(self._token()); //{TOKENS}, keep this last
        Err(SyntaxError::UnknownCharacter(mk_span(self.input_pos(), self.input_pos() + 1)))
    }

    /// token-scanner for looking-for-property state
    fn looking_for_property_token(&mut self) -> Result<TokenSpan, SyntaxError> {
        self.whitespace();
        ret_token!(match_token!(self, "->", ObjectOp));
        match self._label().map(|(x, span)| (x.to_owned(), span)) {
            None => (),
            Some((x, span)) => {
                state_helper!(pop, self);
                return Ok(TokenSpan(Token::String(x), span));
            },
        }
        // ANY_CHAR: pop_state
        self.pop_state();
        self.state.restart = true;
        Err(SyntaxError::None)
    }
}

#[cfg(test)]
mod tests {
    use super::{State};
    use super::*;
    use test::Bencher;

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
        assert_eq!(get_n_tokens(&mut tokenizer, 3), vec![Ok(Token::OpenTag), Ok(Token::ObjectOp), Ok(Token::String("test".to_owned()))]);
        let mut tokenizer = Tokenizer::new("<?php  ->test->gest2 ?>");
        assert_eq!(get_n_tokens(&mut tokenizer, 5), vec![Ok(Token::OpenTag), Ok(Token::ObjectOp), Ok(Token::String("test".to_owned())),
            Ok(Token::ObjectOp), Ok(Token::String("gest2".to_owned()))
        ]);
    }

    #[test]
    fn simple_cast() {
        let mut tokenizer = Tokenizer::new("<?php  (string) ?>");
        assert_eq!(get_n_tokens(&mut tokenizer, 3), vec![Ok(Token::OpenTag), Ok(Token::CastString), Ok(Token::CloseTag)]);
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
        assert_eq!(get_n_tokens(&mut tokenizer, 3), vec![Ok(Token::InlineHtml("a?>b".to_owned())), Ok(Token::OpenTag), Ok(Token::CloseTag)]);
    }

    #[test]
    fn simple_lnum() {
        let mut tokenizer = Tokenizer::new("<?php  42");
        assert_eq!(get_n_tokens(&mut tokenizer, 2), vec![Ok(Token::OpenTag), Ok(Token::Int(42))]);
    }

    #[test]
    fn simple_bnum() {
        let mut tokenizer = Tokenizer::new("<?php  0b101");
        assert_eq!(get_n_tokens(&mut tokenizer, 2), vec![Ok(Token::OpenTag), Ok(Token::Int(5))]);
    }

    #[test]
    fn simple_hnum() {
        let mut tokenizer = Tokenizer::new("<?php  0xff");
        assert_eq!(get_n_tokens(&mut tokenizer, 2), vec![Ok(Token::OpenTag), Ok(Token::Int(255))]);
    }

    #[test]
    fn simple_dnum() {
        let mut tokenizer = Tokenizer::new("<?php  .10");
        assert_eq!(get_n_tokens(&mut tokenizer, 2), vec![Ok(Token::OpenTag), Ok(Token::Double(0.1))]);
        let mut tokenizer = Tokenizer::new("<?php  1.");
        assert_eq!(get_n_tokens(&mut tokenizer, 2), vec![Ok(Token::OpenTag), Ok(Token::Double(1.0))]);
        let mut tokenizer = Tokenizer::new("<?php  1.1");
        assert_eq!(get_n_tokens(&mut tokenizer, 2), vec![Ok(Token::OpenTag), Ok(Token::Double(1.1))]);
    }

    #[test]
    fn dq_string() {
        let mut tokenizer = Tokenizer::new("<?php \"testhallo\\nwelt\"");
        assert_eq!(get_n_tokens(&mut tokenizer, 4), vec![Ok(Token::OpenTag), Ok(Token::DoubleQuote),
            Ok(Token::ConstantEncapsedString("testhallo\nwelt".to_owned())), Ok(Token::DoubleQuote)]
        );
        let mut tokenizer = Tokenizer::new(r#"<?php "testhallo\"welt\"""#);
        assert_eq!(get_n_tokens(&mut tokenizer, 3), vec![Ok(Token::OpenTag), Ok(Token::DoubleQuote), Ok(Token::ConstantEncapsedString("testhallo\"welt\"".to_owned()))]);
        // check if we can handle UTF-8 without crashing
        let mut tokenizer = Tokenizer::new("<?php \"转\\t注\\t字\"");
        assert_eq!(get_n_tokens(&mut tokenizer, 3), vec![Ok(Token::OpenTag), Ok(Token::DoubleQuote), Ok(Token::ConstantEncapsedString("转\t注\t字".to_owned()))]);
    }

    #[test]
    fn sq_string() {
        let mut tokenizer = Tokenizer::new("<?php 'testhallo\\nwelt \\'g\\''");
        assert_eq!(get_n_tokens(&mut tokenizer, 2), vec![Ok(Token::OpenTag), Ok(Token::ConstantEncapsedString("testhallo\\nwelt 'g'".to_owned()))]);
    }

    #[test]
    fn dq_string_var() {
        let mut tokenizer = Tokenizer::new("<?php \"ab $world cd\"");
        assert_eq!(get_n_tokens(&mut tokenizer, 6), vec![Ok(Token::OpenTag), Ok(Token::DoubleQuote), Ok(Token::ConstantEncapsedString("ab ".to_owned())),
                Ok(Token::Variable("world".to_owned())), Ok(Token::ConstantEncapsedString(" cd".to_owned())), Ok(Token::DoubleQuote)
        ]);
        let mut tokenizer = Tokenizer::new("<?php \"$world->ab\"");
        assert_eq!(get_n_tokens(&mut tokenizer, 6), vec![Ok(Token::OpenTag), Ok(Token::DoubleQuote), Ok(Token::Variable("world".to_owned())), Ok(Token::ObjectOp),
            Ok(Token::String("ab".to_owned())), Ok(Token::DoubleQuote),
        ]);
        let mut tokenizer = Tokenizer::new("<?php \"$world->ab->cd\"");
        assert_eq!(get_n_tokens(&mut tokenizer, 7), vec![Ok(Token::OpenTag), Ok(Token::DoubleQuote), Ok(Token::Variable("world".to_owned())), Ok(Token::ObjectOp),
            Ok(Token::String("ab".to_owned())), Ok(Token::ConstantEncapsedString("->cd".to_owned())), Ok(Token::DoubleQuote)
        ]);
        let mut tokenizer = Tokenizer::new("<?php \"{$world->ab->cd}\"");
        assert_eq!(get_n_tokens(&mut tokenizer, 9), vec![Ok(Token::OpenTag), Ok(Token::DoubleQuote), Ok(Token::CurlyBracesOpen), Ok(Token::Variable("world".to_owned())),
            Ok(Token::ObjectOp), Ok(Token::String("ab".to_owned())), Ok(Token::ObjectOp), Ok(Token::String("cd".to_owned())), Ok(Token::CurlyBracesClose)
        ]);
    }

    #[test]
    fn backquote() {
        /*let mut tokenizer = Tokenizer::new("<?php `ab $world cd`");
        assert_eq!(get_n_tokens(&mut tokenizer, 2), vec![Ok(Token::OpenTag),
            Ok(Token::StringLiteralComplex(StringType::Backquote, vec![Token::StringLiteral(StringType::None, "ab ".to_owned()),
                Token::Variable("world".to_owned()), Token::StringLiteral(StringType::None, " cd".to_owned())])
        )]);*/
    }

    #[test]
    fn heredoc() {
        let mut tokenizer = Tokenizer::new("<?php <<<EOT\ntest\nEOT;\n");
        assert_eq!(get_n_tokens(&mut tokenizer, 4), vec![Ok(Token::OpenTag), Ok(Token::HereDocStart), Ok(Token::ConstantEncapsedString("test".to_owned())), Ok(Token::HereDocEnd) ]);
        let mut tokenizer = Tokenizer::new("<?php <<<\"EOT\"\nte\\tst\nEOT;\n");
        assert_eq!(get_n_tokens(&mut tokenizer, 4), vec![Ok(Token::OpenTag), Ok(Token::HereDocStart), Ok(Token::ConstantEncapsedString("te\tst".to_owned())), Ok(Token::HereDocEnd) ]);
    }

    #[test]
    fn nowdoc() {
        let mut tokenizer = Tokenizer::new("<?php <<<'EOT'\nte\\tst\nEOT;\n");
        assert_eq!(get_n_tokens(&mut tokenizer, 2), vec![Ok(Token::OpenTag), Ok(Token::ConstantEncapsedString("te\\tst".to_owned()))]);
    }

    #[test]
    fn single_line_comment() {
        let mut tokenizer = Tokenizer::new("<?php //test");
        assert_eq!(get_n_tokens(&mut tokenizer, 2), vec![Ok(Token::OpenTag), Ok(Token::Comment("test".to_owned()))]);
        let mut tokenizer = Tokenizer::new("<?php //test\n$a");
        assert_eq!(get_n_tokens(&mut tokenizer, 2), vec![Ok(Token::OpenTag), Ok(Token::Comment("test".to_owned()))]);
    }

    #[test]
    fn block_comment() {
        let mut tokenizer = Tokenizer::new("<?php /* test\ntest2 */bb");
        assert_eq!(get_n_tokens(&mut tokenizer, 2), vec![Ok(Token::OpenTag), Ok(Token::Comment(" test\ntest2 ".to_owned()))]);
    }

    #[test]
    fn resolve_line_nums() {
        let mut tokenizer = Tokenizer::new("<?php \n\n\n/* test */");
        assert_eq!(get_n_tokens(&mut tokenizer, 1), vec![Ok(Token::OpenTag)]);
        match tokenizer.next_token() {
            Ok(TokenSpan(Token::Comment(comment), span)) => {
                assert_eq!(tokenizer.state.line_num, 4);
                assert_eq!(comment, " test ");
                println!("{:?}", span);
                assert_eq!(tokenizer.state.line_from_position(span.start as usize), 4 - 1); //0-based lines
            },
            _ => {assert!(false);},
        }
    }

    // TODO: use own error type?
    // TODO: error handling tests

    #[bench]
    fn bench_simple_set_of_variables(b: &mut Bencher) {
        let argc: usize = 10000;
        let mut inp = "<?php ".to_owned();
        for i in 0..argc {
            inp.push_str("$tte");
        }
        b.iter(|| {
            let mut tokenizer = Tokenizer::new(&inp);
            assert_eq!(get_n_tokens(&mut tokenizer, argc).len(), argc);
        });
    }
}
