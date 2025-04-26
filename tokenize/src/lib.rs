// TODO: remove allow unused when done
#![allow(unused)]

#[cfg(test)]
mod test;
pub mod tokens;

use std::error::Error;
use std::fmt::Display;
use std::mem;

pub use tokens::{Token, TokenType};

use unicode_segmentation::UnicodeSegmentation;

/// Tokenizes `input`. Returns a vector of [`Token`]s on success.
///
/// # Errors
/// - If tokenization fails, returns a [`TokenizeError`].
pub fn tokenize(input: &str) -> Result<Vec<Token>, TokenizeError> {
    // (line-number, pos-in-line, grapheme)
    let input_iter = input.lines().enumerate().flat_map(|(lnum, s)| {
        s.graphemes(true)
            .enumerate()
            .map(move |(idx, gr)| (lnum + 1, idx + 1, gr))
    });
    let mut dfa = TokDfa::default();
    for (line, pos, grapheme) in input_iter {
        dfa = dfa.transition(line, pos, grapheme)?;
    }
    dfa.finalize()?;

    Ok(dfa.tok_vec)
}

#[derive(Debug, Clone)]
pub enum TokenizeErrorType {
    InvalidToken(String),
    UnfinishedToken,
    InternalErr(&'static str),
}

impl Display for TokenizeErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #[allow(clippy::enum_glob_use)]
        use TokenizeErrorType::*;
        match self {
            InvalidToken(s) => {
                write!(f, "Invalid token {s}")
            }
            InternalErr(s) => {
                write!(f, "Internal tokenization error {s}")
            }
            UnfinishedToken => {
                write!(f, "Unfinished expression")
            }
        }
    }
}

impl Error for TokenizeErrorType {}

/// Error-reporting when [`tokenize`] fails.
#[derive(Debug)]
pub struct TokenizeError {
    err_type: TokenizeErrorType,
    cause: Option<Box<dyn Error>>,
    line: usize,
    pos: usize,
}

impl TokenizeError {
    /// Pass to `cause` in [`new`] when there's no cause.
    const INNOCENCE: Option<std::fmt::Error> = None;

    #[must_use]
    fn new(
        err_type: TokenizeErrorType,
        cause: Option<impl Error + 'static>,
        line: usize,
        pos: usize,
    ) -> Self {
        Self {
            err_type,
            cause: cause.map(std::convert::Into::into),
            line,
            pos,
        }
    }
    #[must_use]
    pub fn line(&self) -> usize {
        self.line
    }

    #[must_use]
    pub fn pos(&self) -> usize {
        self.pos
    }
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n\tAt position {}:{}",
            self.err_type, self.line, self.pos
        )
    }
}

impl Error for TokenizeError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        self.cause.as_deref()
    }
}

type StateFn = fn(TokDfa, usize, usize, &str) -> Result<TokDfa, TokenizeError>;

struct TokDfa {
    tok_vec: Vec<Token>,
    curr_tok: Option<Token>,
    state_fn: StateFn,
}

impl Default for TokDfa {
    fn default() -> Self {
        Self {
            tok_vec: Vec::new(),
            curr_tok: None,
            state_fn: TokDfa::init_state,
        }
    }
}

impl TokDfa {
    /// Transition from the current state to the appropriate state given the
    /// input `grapheme`, or to a fail state.
    ///
    /// The two indices `line` and `pos` are used for error-reporting.
    ///
    /// If successful, always return the same `self` passed in.
    ///
    /// # Errors:
    /// - When the state machine reaches a fail state, it returns a
    ///   ``TokenizeError``
    pub fn transition(
        mut self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<Self, TokenizeError> {
        (self.state_fn)(self, line, pos, grapheme)
    }

    /// Returns whether the current state is an accepting state.
    ///
    /// Currently, the only non-accepting state is ``string_state``.
    pub fn is_at_accepting_state(&self) -> bool {
        !std::ptr::fn_addr_eq(self.state_fn, Self::string_state as StateFn)
    }

    pub fn finalize(&mut self) -> Result<(), TokenizeError> {
        if !self.is_at_accepting_state() {
            if let Some(tok) = &self.curr_tok {
                return Err(TokenizeError::new(
                    TokenizeErrorType::UnfinishedToken,
                    TokenizeError::INNOCENCE,
                    tok.line_number(),
                    tok.line_position(),
                ));
            }
            return Err(TokenizeError::new(
                TokenizeErrorType::UnfinishedToken,
                TokenizeError::INNOCENCE,
                1,
                0,
            ));
        }
        let old_tok = mem::take(&mut self.curr_tok);
        if old_tok.is_none() {
            return Ok(());
        }
        let old_tok = old_tok.unwrap();
        let kw = if let TokenType::Identifier(s) = old_tok.token_type() {
            tokens::keyword_lookup(s.as_str())
        } else {
            None
        };
        if let Some(kw) = kw {
            self.tok_vec.push(Token::new(
                kw,
                old_tok.line_number(),
                old_tok.line_position(),
            ));
        } else {
            self.tok_vec.push(old_tok);
        }

        Ok(())
    }

    // TODO: tryna also document the (side) state changes. E,g, when
    // transitioning, a character/grapheme is pushed into a buffer sometimes,
    // and sometimes it converts that into a token and then pushes that into
    // the vector of tokens.

    /// The initial state.
    ///
    /// Parameter passing follows the rule defined in [`TokDfa::transition`].
    ///
    /// # Transitions:
    /// - States preceded by "fwd" indicate the input is forwarded to that
    ///   state. Otherwise, the input is consumed by the current state.
    ///
    /// | input | next-state |
    /// | ----- | ---------- |
    /// | \[a-zA-z_\] | fwd [`TokDfa::identifier_state`] |
    /// | \[0-9\] | fwd [`TokDfa::number_state`] |
    /// | """ | [`TokDfa::string_state`] |
    /// | <whitespace>+ | [`TokDfa::init_state`] |
    ///
    /// # Extra rules
    /// - \<whitespace\> ::= LF, SPACE, TAB, FF, CR
    fn init_state(
        mut self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<Self, TokenizeError> {
        let chr = grapheme.parse::<char>().map_err(|e| {
            TokenizeError::new(
                TokenizeErrorType::InvalidToken(grapheme.into()),
                Some(e),
                line,
                pos,
            )
        })?;

        if chr.is_ascii_alphabetic() || chr as u8 == b'_' {
            self.state_fn = Self::identifier_state;
            self.identifier_state(line, pos, grapheme)
        } else if chr.is_ascii_digit() {
            self.number_state(line, pos, grapheme)
        } else if chr as u8 == b'"' {
            self.state_fn = Self::string_state;
            Ok(self)
        } else if chr.is_ascii_whitespace() {
            Ok(self)
        } else {
            self.symbol_state(line, pos, grapheme)
        }
    }

    // must maintain interface.
    /// String state.
    ///
    /// Parameter passing follows the rule defined in [`TokDfa::transition`].
    ///
    /// # Transition
    /// - States preceded by "fwd" indicate the input is forwarded to that
    ///   state. Otherwise, the input is consumed by the current state.
    ///
    /// | input | next-state |
    /// | ----- | ---------- |
    /// | [<char>](tokens::TokenType::Char) | [`TokDfa::string_state`] |
    /// | """ | [`TokDfa::init_state`] |
    #[allow(clippy::unnecessary_wraps)]
    fn string_state(
        mut self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<Self, TokenizeError> {
        if grapheme == "\"" {
            let old_tok = mem::take(&mut self.curr_tok);
            match old_tok {
                None => self.tok_vec.push(Token::new(
                    TokenType::String(String::new()),
                    line,
                    pos,
                )),
                Some(tok) => {
                    if let TokenType::String(_) = tok.token_type() {
                        self.tok_vec.push(tok);
                    } else {
                        panic!(
                            "Internal error: tokenize string_state:
                            token type is NOT string"
                        )
                    }
                }
            }
            self.state_fn = Self::init_state;
            return Ok(self);
        }

        let old_tok = mem::take(&mut self.curr_tok);
        match old_tok {
            None => {
                self.curr_tok = Some(Token::new(
                    TokenType::String(String::from(grapheme)),
                    line,
                    pos,
                ));
                // self.state_fn = Self::init_state;
                Ok(self)
            }
            Some(tok) => {
                if let (TokenType::String(mut s), line, pos) = tok.bind() {
                    s.push_str(grapheme);
                    self.curr_tok =
                        Some(Token::new(TokenType::String(s), line, pos));
                    return Ok(self);
                }
                panic!(
                    "Internal error: string_state: old token type is NOT
                    string"
                )
            }
        }
    }

    fn number_state(
        mut self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<Self, TokenizeError> {
        // ensure grapheme is 1 ASCII character.
        debug_assert!(grapheme.is_ascii() && grapheme.len() == 1);
        let chr = grapheme.parse::<char>().map_err(|e| {
            panic!("Internal error: identifier_state: char conversion failed")
        })?;

        if chr.is_ascii_whitespace() {
            debug_assert!(self.curr_tok.is_some());
            self.tok_vec.push(mem::take(&mut self.curr_tok).unwrap());
            self.state_fn = Self::init_state;
            return Ok(self);
        }
        if chr.is_ascii_digit() {
            if let Some(tok) = &mut self.curr_tok {
                let Token {
                    token_type:
                        TokenType::Integer(num_str) | TokenType::Double(num_str),
                    ..
                } = tok
                else {
                    panic!("Internal error: number_state: wrong token type");
                };
                num_str.push(chr);
                return Ok(self);
            }
            self.curr_tok = Some(Token::new(
                TokenType::Integer(String::from(chr)),
                line,
                pos,
            ));
            return Ok(self);
        }
        // for now, characters like e, u, and such, still return error.

        Err(TokenizeError::new(
            TokenizeErrorType::InvalidToken(String::from(chr)),
            TokenizeError::INNOCENCE,
            line,
            pos,
        ))
    }

    /// Identifier state.
    ///
    /// Parameter passing follows the rule defined in [`TokDfa::transition`].
    ///
    /// # Transition
    ///
    /// | input | next-state |
    /// | ----- | ---------- |
    /// | [a-zA-z0-9_] | [`TokDfa::identifier_state`] |
    /// | <space> | [`TokDfa::init_state`] |
    fn identifier_state(
        mut self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<Self, TokenizeError> {
        // ensure grapheme is 1 ASCII character.
        debug_assert!(grapheme.is_ascii() && grapheme.len() == 1);
        let chr = grapheme.parse::<char>().map_err(|e| {
            panic!("Internal error: identifier_state: char conversion failed")
        })?;
        #[cfg(debug_assertions)]
        if self.curr_tok.is_none() {
            debug_assert!(chr.is_ascii_alphabetic() || chr == '_');
        }

        if chr.is_ascii_whitespace() {
            debug_assert!(self.curr_tok.is_some());
            let old_tok = mem::take(&mut self.curr_tok).unwrap();

            // in case the token is a reserved keyword
            if let (TokenType::Identifier(s), line, pos) = old_tok.bind_ref() {
                if let Some(kw) = tokens::keyword_lookup(s.as_str()) {
                    self.tok_vec.push(Token::new(kw, line, pos));
                } else {
                    self.tok_vec.push(old_tok);
                }
            } else {
                panic!("Internal error: identifier_state: wrong token type")
            }
            self.state_fn = Self::init_state;
            return Ok(self);
        }
        let old_tok = mem::take(&mut self.curr_tok);
        if old_tok.is_none() {
            self.curr_tok = Some(Token::new(
                TokenType::Identifier(String::from(chr)),
                line,
                pos,
            ));
            return Ok(self);
        }
        let old_tok = old_tok.unwrap();
        if let (TokenType::Identifier(mut s), line, pos) = old_tok.bind() {
            s.push(chr);
            self.curr_tok =
                Some(Token::new(TokenType::Identifier(s), line, pos));
            Ok(self)
        } else {
            panic!("Internal error: identifier_state: wrong token type")
        }
    }

    fn symbol_state(
        self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<Self, TokenizeError> {
        todo!()
    }

    fn equal_state(
        &mut self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<(), TokenizeError> {
        todo!()
    }

    fn lpbrace_state(
        &mut self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<(), TokenizeError> {
        todo!()
    }

    fn rpbrace_state(
        &mut self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<(), TokenizeError> {
        todo!()
    }

    fn slash_state(
        &mut self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<(), TokenizeError> {
        todo!()
    }

    fn bang_state(
        &mut self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<(), TokenizeError> {
        todo!()
    }
}
