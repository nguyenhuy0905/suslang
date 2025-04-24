// TODO: remove allow unused when done
#![allow(unused)]

pub mod tokens;
use std::error::Error;
use std::fmt::Display;

pub use tokens::TokenType;

use unicode_segmentation::UnicodeSegmentation;

/// Tokenizes the input string.
///
/// * `input`:
///
/// # Errors
/// - If we meet a dumb token, report an error.
#[must_use]
pub fn tokenize(input: &str) -> Vec<TokenType> {
    let input_iter = UnicodeSegmentation::graphemes(input, true).enumerate();
    let ret = Vec::new();
    let mut line: usize = 0;
    let mut state = TokenizerState::default();
    for (idx, grapheme) in input_iter {
        match grapheme {
            "\n" => {
                line += 1;
            }
            _ => todo!(),
        }
    }
    ret
}

#[derive(Debug, Clone)]
pub enum TokenizeErrorType<'a> {
    InvalidToken(&'a str),
}

impl Display for TokenizeErrorType<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenizeErrorType::InvalidToken(s) => {
                write!(f, "Invalid token {s}")
            }
        }
    }
}

impl Error for TokenizeErrorType<'_> {}

#[derive(Debug)]
pub struct TokenizeError<'a> {
    err_type: TokenizeErrorType<'a>,
    line: usize,
    pos: usize,
}

impl<'a> TokenizeError<'a> {
    #[must_use]
    pub fn new(
        err_type: TokenizeErrorType<'a>,
        line: usize,
        pos: usize,
    ) -> Self {
        Self {
            err_type,
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

impl Display for TokenizeError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n\tAt position {}:{}",
            self.err_type, self.line, self.pos
        )
    }
}

impl Error for TokenizeError<'_> {}

#[allow(dead_code)]
struct TokenizerState {
    transit_func:
        fn(usize, usize, &str) -> Result<TokenizerState, TokenizeError>,
}

/// Basically a DFA.
#[allow(dead_code)]
impl TokenizerState {
    pub fn new(
        transit_func: fn(
            usize,
            usize,
            &str,
        ) -> Result<TokenizerState, TokenizeError>,
    ) -> Self {
        Self { transit_func }
    }

    /// The initial state's transit function.
    /// ASCII only.
    /// Match input:
    ///
    /// # Errors
    /// - ``InvalidToken`` if non-ASCII.
    fn init_transit(
        line: usize,
        pos: usize,
        input: &str,
    ) -> Result<TokenizerState, TokenizeError> {
        if (!input.is_ascii()) {
            return Err(TokenizeError::new(
                TokenizeErrorType::InvalidToken(input),
                line,
                pos,
            ));
        }

        match input.parse::<char>().unwrap() {
            // TODO: write a num_transit function.
            '0'..='9' => Ok(Self::new(Self::init_transit)),
            _ => Err(TokenizeError::new(
                TokenizeErrorType::InvalidToken(input),
                line,
                pos,
            )),
        }
    }
}

impl Default for TokenizerState {
    fn default() -> Self {
        Self {
            transit_func: TokenizerState::init_transit,
        }
    }
}
