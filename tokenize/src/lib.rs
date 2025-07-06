// TODO: remove allow unused when done
// #![allow(unused)]

#[cfg(test)]
mod test;
pub mod tokens;

use std::collections::VecDeque;
use std::error::Error;
use std::fmt::Display;
use std::mem;

use unicode_segmentation as us;

use tokens::CharPosition;
pub use tokens::{Token, TokenKind};

/// Tokenizes `input`. Returns a vector of [`Token`]s on success.
///
/// # Errors
/// - If tokenization fails, returns a [`TokenizeError`].
///
/// # Panics
/// - Shouldn't happen unless ``graphemes`` works incorrectly.
pub fn tokenize(input: &str) -> Result<VecDeque<Token>, TokenizeError> {
    // // (line-number, pos-in-line, grapheme)
    // let input_iter = input.lines().enumerate().flat_map(|(lnum, s)| {
    //     s.graphemes(true).enumerate().map(move |(idx, gr)| {
    //         (lnum + 1, idx + 1, gr.parse::<char>().unwrap())
    //     })
    // });
    // let mut dfa = TokDfa::default();
    // for (line, pos, grapheme) in input_iter {
    //     dfa = dfa.transition(line, pos, grapheme)?;
    // }
    // dfa.finalize()?;

    // Ok(dfa.tok_vec.into())
    todo!()
}

#[derive(Debug, Clone)]
pub enum TokenizeErrorType {
    InvalidChar(char),
    ExpectChar,
}

impl Display for TokenizeErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Error for TokenizeErrorType {}

/// Error-reporting when [`tokenize`] fails.
#[derive(Debug)]
pub struct TokenizeError {
    err_type: TokenizeErrorType,
    pos: CharPosition,
}

impl TokenizeError {
    #[inline]
    #[must_use]
    fn new(err_type: TokenizeErrorType, line: usize, column: usize) -> Self {
        Self {
            err_type,
            pos: CharPosition { line, column },
        }
    }

    #[inline]
    #[must_use]
    pub fn line(&self) -> usize {
        self.pos.line
    }

    #[inline]
    #[must_use]
    pub fn column(&self) -> usize {
        self.pos.column
    }
}

impl Display for TokenizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\n\tAt position {}:{}",
            self.err_type, self.pos.line, self.pos.column
        )
    }
}

impl Error for TokenizeError {}

/// Basically a state machine.
struct Tokenizer<'a> {
    /// Dictates which transition function to use.
    ///
    /// This is a tad bit more reliable than comparing function pointers
    /// themselves: if there are two identical functions, the compiler is
    /// allowed to just generate one implementation block; and both
    /// function pointers now refer to the same block.
    ///
    /// This *can* be an issue, because we could very well have a
    /// non-final state with identical implementation as a final state.
    state: TokenizeState,
    /// String to slice from.
    refstr: &'a str,
    /// `next` or `peek` should always start at `self.refstr[self.window_end]`.
    graphemes: std::iter::Peekable<us::GraphemeIndices<'a>>,
    /// `self.refstr[window_begin..window_end]` represents the current slice.
    window_begin: usize,
    /// `self.refstr[window_begin..window_end]` represents the current slice.
    window_end: usize,
    /// position of `window_begin`
    begin_pos: CharPosition,
    /// position of `window_end`
    end_pos: CharPosition,
    /// resulting token list
    tokens: Vec<Token>,
}

/// Function pointers aren't guaranteed to *not* be equal evn if their
/// identifiers are *not* equal. So, we're coping with this.
enum TokenizeState {
    Init,
    Ident,
    Number,
    Float,
    String,
    Char,
    Slash,
    Amper,
    Beam,
    Equal,
    Bang,
    Less,
    Greater,
}

macro_rules! match_all_symbols {
    () => {
        '\"' | '\'' | '/' | '&' | '|' | '=' | '<' | '>' | '!' | '.'
    };
    // hacky i know
    (no_dot) => {
        '\"' | '\'' | '/' | '&' | '|' | '=' | '<' | '>' | '!'
    };
}

impl<'a> Tokenizer<'a> {
    pub fn new(refstr: &'a str) -> Self {
        Self {
            state: TokenizeState::Init,
            refstr,
            graphemes: us::UnicodeSegmentation::grapheme_indices(refstr, true)
                .peekable(),
            window_begin: 0,
            window_end: 0,
            begin_pos: CharPosition { line: 1, column: 1 },
            end_pos: CharPosition { line: 1, column: 1 },
            tokens: Vec::new(),
        }
    }

    /// ## Transitions:
    ///
    /// - [a-zA-Z_] => advance to `TokenizeState::Ident`
    /// - [0-9] => advance to `TokenizeState::Number`
    /// - '\n' | '\t' | ' ' => advance to itself
    /// - any symbol => advance to the corresponding symbol state
    ///
    /// ## Safety
    /// - This method assumes the current window is empty.
    fn init_transit(&mut self) -> Result<(), TokenizeError> {
        debug_assert!(!self.is_done());
        // we assume at this state, the window is empty, for now
        debug_assert!(self.window_begin == self.window_end);
        self.peek_next_char()
            .ok_or(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: self.end_pos,
            })
            .and_then(|gr| match gr {
                'a'..='z' | 'A'..='Z' | '_' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Ident;
                    Ok(())
                }
                '0'..='9' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Number;
                    Ok(())
                }
                '\"' => {
                    self.consume_next_char();
                    self.state = TokenizeState::String;
                    Ok(())
                }
                '\'' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Char;
                    Ok(())
                }
                '/' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Slash;
                    Ok(())
                }
                '&' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Amper;
                    Ok(())
                }
                '|' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Beam;
                    Ok(())
                }
                '=' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Equal;
                    Ok(())
                }
                '<' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Less;
                    Ok(())
                }
                '>' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Greater;
                    Ok(())
                }
                '!' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Bang;
                    Ok(())
                }
                // TODO: do we support the madlads that use '\f'
                '\n' | '\t' | ' ' => {
                    self.consume_next_char();
                    Ok(())
                }
                '.' => {
                    // a dot is a dot
                    self.consume_next_char();
                    self.empty_window();

                    self.tokens.push(Token {
                        kind: TokenKind::Dot,
                        pos: self.begin_pos,
                    });
                    Ok(())
                }
                _ => Err(TokenizeError {
                    err_type: TokenizeErrorType::InvalidChar(gr),
                    pos: self.end_pos,
                }),
            })
    }

    fn ident_transit(&mut self) -> Result<(), TokenizeError> {
        debug_assert!(self.window_begin < self.window_end);
        self.peek_next_char()
            .ok_or(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: self.end_pos,
            })
            .and_then(|gr| match gr {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                    self.consume_next_char();
                    Ok(())
                }
                '\n' | '\t' | ' ' | match_all_symbols!() => {
                    // finalize the current token
                    let kind = tokens::keyword_lookup(self.get_window_slice());
                    self.tokens.push(Token {
                        kind,
                        pos: self.begin_pos,
                    });
                    self.empty_window();
                    self.state = TokenizeState::Init;
                    Ok(())
                }
                _ => Err(TokenizeError {
                    err_type: TokenizeErrorType::InvalidChar(gr),
                    pos: self.end_pos,
                }),
            })
    }

    fn number_transit(&mut self) -> Result<(), TokenizeError> {
        debug_assert!(self.window_begin < self.window_end);
        self.peek_next_char()
            .ok_or(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: self.end_pos,
            })
            .and_then(|gr| match gr {
                '0'..'9' => {
                    self.consume_next_char();
                    Ok(())
                }
                '.' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Float;
                    Ok(())
                }
                // NOTE: no_dot is not an identifier, but a goofy way to
                // introduce a match arm.
                match_all_symbols!(no_dot) => {
                    self.tokens.push(Token {
                        kind: TokenKind::Integer(
                            self.get_window_slice().parse::<u64>().unwrap(),
                        ),
                        pos: self.begin_pos,
                    });
                    self.empty_window();
                    self.state = TokenizeState::Init;
                    Ok(())
                }
                _ => Err(TokenizeError {
                    err_type: TokenizeErrorType::ExpectChar,
                    pos: self.end_pos,
                }),
            })
    }

    fn float_transit(&mut self) -> Result<(), TokenizeError> {
        debug_assert!(self.window_begin < self.window_end);
        // almost similar to `number_transit`
        self.peek_next_char()
            .ok_or(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: self.end_pos,
            })
            .and_then(|gr| match gr {
                '0'..'9' => {
                    self.consume_next_char();
                    Ok(())
                }
                match_all_symbols!() => {
                    self.tokens.push(Token {
                        kind: TokenKind::Integer(
                            self.get_window_slice().parse::<u64>().unwrap(),
                        ),
                        pos: self.begin_pos,
                    });
                    self.empty_window();
                    self.state = TokenizeState::Init;
                    Ok(())
                }
                _ => Err(TokenizeError {
                    err_type: TokenizeErrorType::ExpectChar,
                    pos: self.end_pos,
                }),
            })
    }

    fn string_transit(&mut self) -> Result<(), TokenizeError> {
        // now this can start empty
        self.peek_next_char()
            .ok_or(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: self.begin_pos,
            })
            .and_then(|gr| match gr {
                '\"' => {
                    self.tokens.push(Token {
                        kind: TokenKind::String(Box::from(
                            self.get_window_slice(),
                        )),
                        pos: self.begin_pos,
                    });
                    self.consume_next_char();
                    self.empty_window();
                    Ok(())
                }
                '\\' => {
                    todo!(
                        "Tokenizer::string_transit: Create a new state that \
                         parses the string escape sequence"
                    )
                }
                _ => {
                    self.consume_next_char();
                    Ok(())
                }
            })
    }

    fn char_transit(&mut self) -> Result<(), TokenizeError> {
        debug_assert!(self.window_begin == self.window_end);
        self.consume_next_char()
            .ok_or(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: self.end_pos,
            })
            .and_then(|gr| {
                if gr.is_alphanumeric() {
                    self.tokens.push(Token {
                        kind: TokenKind::Char(gr),
                        pos: self.begin_pos,
                    });
                    Ok(())
                } else {
                    Err(TokenizeError {
                        err_type: TokenizeErrorType::InvalidChar(gr),
                        pos: self.begin_pos,
                    })
                }
            })
            .and_then(|()| {
                self.consume_next_char().ok_or(TokenizeError {
                    err_type: TokenizeErrorType::ExpectChar,
                    pos: self.end_pos,
                })
            })
            .and_then(|gr| {
                if gr == '\'' {
                    self.empty_window();
                    Ok(())
                } else {
                    Err(TokenizeError {
                        err_type: TokenizeErrorType::InvalidChar(gr),
                        pos: self.begin_pos,
                    })
                }
            })
    }

    fn slash_transit(&mut self) -> Result<(), TokenizeError> {
        todo!()
    }

    fn amper_transit(&mut self) -> Result<(), TokenizeError> {
        todo!()
    }

    fn beam_transit(&mut self) -> Result<(), TokenizeError> {
        todo!()
    }

    fn equal_transit(&mut self) -> Result<(), TokenizeError> {
        todo!()
    }

    fn bang_transit(&mut self) -> Result<(), TokenizeError> {
        todo!()
    }

    fn less_transit(&mut self) -> Result<(), TokenizeError> {
        todo!()
    }

    fn greater_transit(&mut self) -> Result<(), TokenizeError> {
        todo!()
    }

    /// If this returns true, we should stop tokenizing.
    #[inline]
    #[must_use]
    fn is_done(&self) -> bool {
        self.window_end >= self.refstr.len()
    }

    #[inline]
    #[must_use]
    fn get_window_slice(&self) -> &str {
        &self.refstr[self.window_begin..self.window_end]
    }

    /// Basically, `self.window_begin = self.window_end`.
    ///
    /// Of course, if window is already empty, this does nothing.
    ///
    /// If this is called while `peek`ing a grapheme (let's call it `p`):
    /// [<beg>..<end = `p`>] -> [<beg = end (peek)>]
    /// So, next time we `peek`, it's still the same grapheme.
    ///
    /// If this is called while consuming a grapheme (let's call it `c`):
    /// [<beg>..<`c`><end (peek)>] -> [<beg = end `peek`>]
    /// So, next time we `peek`, it's the next grapheme.
    #[inline]
    fn empty_window(&mut self) {
        self.begin_pos = self.end_pos;
        self.window_begin = self.window_end;
    }

    /// Moves the window's begin by one grapheme.
    /// If the *current* character pointed to by `self.window_begin` is the
    /// newline, increment line count and reset column position of
    /// `self.begin_pos`.
    #[inline]
    fn slide_begin_right(&mut self) {
        self.graphemes.peek().copied().inspect(|&(idx, gr)| {
            self.window_begin = idx;
            if gr == "\n" {
                self.begin_pos.line += 1;
                self.begin_pos.column = 1;
                return;
            }
            self.begin_pos.column += gr.len();
        });
    }

    /// Moves the window's end by one grapheme.
    /// If the *current* character pointed to by `self.window_end` is the
    /// newline, increment line count and reset column position of
    /// `self.end_pos`.
    #[inline]
    fn slide_end_right(&mut self) {
        self.graphemes.peek().copied().inspect(|&(idx, gr)| {
            self.window_end = idx;
            if gr == "\n" {
                self.end_pos.line += 1;
                self.end_pos.column = 1;
                return;
            }
            self.end_pos.column += gr.len();
        });
    }

    /// Returns the next character, after sliding the window's end rightwards.
    #[inline]
    fn consume_next_char(&mut self) -> Option<char> {
        self.slide_end_right();
        self.graphemes
            .next()
            // if this doesn't work, unicode_segmentation is broken.
            // At which point, we're gonna have to clone and fix the source
            // code.
            .map(|(_, gr)| gr.parse::<char>().unwrap())
    }

    /// NOTE: the `peek` method takes a `mut` reference. However, what we can
    /// observe won't change with multiple calls of this method.
    #[inline]
    fn peek_next_char(&mut self) -> Option<char> {
        self.graphemes
            .peek()
            .map(|&(_, gr)| gr.parse::<char>().unwrap())
    }
}
