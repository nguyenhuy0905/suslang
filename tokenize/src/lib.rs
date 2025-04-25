// TODO: remove allow unused when done
#![allow(unused)]

#[cfg(test)]
mod test;
pub mod tokens;

use std::error::Error;
use std::fmt::Display;

pub use tokens::{Token, TokenType};

use unicode_segmentation::UnicodeSegmentation;

/// Tokenizes the input string.
///
/// * `input`:
///
/// # Errors
/// - If we meet a dumb token, report an error.
pub fn tokenize(input: &str) -> Result<Vec<Token>, TokenizeError> {
    // (line-number, pos-in-line, grapheme)
    let input_iter = input.lines().enumerate().flat_map(|(lnum, s)| {
        s.graphemes(true)
            .enumerate()
            .map(move |(idx, gr)| (lnum + 1, idx + 1, gr))
    });
    let mut dfa = TokDfa::default();
    for (line, pos, grapheme) in input_iter {
        dfa = dfa.call(line, pos, grapheme)?;
    }

    Ok(dfa.tok_vec)
}

#[derive(Debug, Clone)]
pub enum TokenizeErrorType {
    InvalidToken(String),
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
        }
    }
}

impl Error for TokenizeErrorType {}

#[derive(Debug)]
pub struct TokenizeError {
    err_type: TokenizeErrorType,
    cause: Option<Box<dyn Error>>,
    line: usize,
    pos: usize,
}

impl TokenizeError {
    #[must_use]
    pub fn new(
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
    pub fn call(
        mut self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<Self, TokenizeError> {
        // fuck you borrow checker
        let state_func =
            std::mem::replace(&mut self.state_fn, Self::init_state);
        self.state_fn = state_func;
        state_func(self, line, pos, grapheme)
    }

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

    // the wrap is the mandatory interface.
    #[allow(clippy::unnecessary_wraps)]
    fn string_state(
        mut self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<Self, TokenizeError> {
        if grapheme == "\"" {
            let mut old_tok = std::mem::take(&mut self.curr_tok);
            debug_assert!(old_tok.is_some());
            self.tok_vec.push(old_tok.unwrap());
            self.state_fn = Self::init_state;
            return Ok(self);
        }

        let mut old_tok = std::mem::take(&mut self.curr_tok);
        match old_tok {
            None => {
                self.curr_tok = Some(Token::new(
                    TokenType::String(String::from(grapheme)),
                    line,
                    pos,
                ));
                Ok(self)
            }
            Some(tok) => {
                if let (TokenType::String(mut s), line, pos) = tok.bind() {
                    s.push_str(grapheme);
                    self.curr_tok =
                        Some(Token::new(TokenType::String(s), line, pos));
                    return Ok(self);
                }
                Err(TokenizeError::new(
                    TokenizeErrorType::InternalErr("while tokenizing string"),
                    // using some dummy generics.
                    Option::<std::fmt::Error>::None,
                    line,
                    pos,
                ))
            }
        }
    }

    fn number_state(
        self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<Self, TokenizeError> {
        todo!()
    }

    fn identifier_state(
        self,
        line: usize,
        pos: usize,
        grapheme: &str,
    ) -> Result<Self, TokenizeError> {
        todo!()
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
