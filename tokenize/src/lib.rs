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
        todo!()
    }

    fn float_transit(&mut self) -> Result<(), TokenizeError> {
        todo!()
    }

    fn string_transit(&mut self) -> Result<(), TokenizeError> {
        todo!()
    }

    fn char_transit(&mut self) -> Result<(), TokenizeError> {
        todo!()
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

// impl Default for TokDfa {
//     fn default() -> Self {
//         Self {
//             tok_vec: Vec::new(),
//             curr_tok: None,
//             state_fn: TokDfa::init_state,
//         }
//     }
// }

// impl TokDfa {
//     /// Transition from the current state to the appropriate state given the
//     /// input `grapheme`, or to a fail state.
//     ///
//     /// The two indices `line` and `pos` are used for error-reporting.
//     ///
//     /// If successful, always return the same `self` passed in.
//     ///
//     /// # Errors:
//     /// - When the state machine reaches a fail state, it returns a
//     ///   ``TokenizeError``
//     pub fn transition(
//         self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         (self.state_fn)(self, line, pos, grapheme)
//     }

//     /// Returns whether the current state is an accepting state.
//     ///
//     /// Currently, the only non-accepting state is ``string_state``.
//     pub fn is_at_accepting_state(&self) -> bool {
//         !std::ptr::fn_addr_eq(self.state_fn, Self::string_state as StateFn)
//     }

//     /// Checks to see if:
//     /// 1. The state machine stops at an accepting state
//     ///    [`TokDfa::is_at_accepting_state`].
//     /// 2. There's any non-empty token left not put into the token vector.
//     ///
//     /// If 1, return an error of type [`TokenizeErrorType::UnfinishedToken`].
//     /// If 2, simply push that token onto the vector.
//     pub fn finalize(&mut self) -> Result<(), TokenizeError> {
//         if !self.is_at_accepting_state() {
//             if let Some(tok) = &self.curr_tok {
//                 return Err(TokenizeError::new(
//                     TokenizeErrorType::UnfinishedToken,
//                     TokenizeError::INNOCENCE,
//                     tok.line_number(),
//                     tok.line_position(),
//                 ));
//             }
//             return Err(TokenizeError::new(
//                 TokenizeErrorType::UnfinishedToken,
//                 TokenizeError::INNOCENCE,
//                 1,
//                 0,
//             ));
//         }
//         if let Some(Token {
//             tok_typ: TokenType::SlashSlash,
//             ..
//         }) = self.tok_vec.last()
//         {
//             debug_assert!(self.curr_tok.is_none());
//             self.tok_vec.pop();
//             return Ok(());
//         }
//         let old_tok = mem::take(&mut self.curr_tok);
//         if old_tok.is_none() {
//             return Ok(());
//         }
//         let old_tok = old_tok.unwrap();
//         let kw = if let TokenType::Identifier(s) = old_tok.token_type() {
//             tokens::keyword_lookup(s.as_str())
//         } else {
//             None
//         };
//         if let Some(kw) = kw {
//             self.tok_vec.push(Token::new(
//                 kw,
//                 old_tok.line_number(),
//                 old_tok.line_position(),
//             ));
//         } else {
//             self.tok_vec.push(old_tok);
//         }

//         Ok(())
//     }

//     // TODO: tryna also document the (side) state changes. E,g, when
//     // transitioning, a character/grapheme is pushed into a buffer sometimes,
//     // and sometimes it converts that into a token and then pushes that into
//     // the vector of tokens.

//     /// The initial state.
//     ///
//     /// Parameter passing follows the rule defined in [`TokDfa::transition`].
//     ///
//     /// # Transitions:
//     /// - States preceded by "fwd" indicate the input is forwarded to that
//     ///   state. Otherwise, the input is consumed by the current state.
//     ///
//     /// | input | next-state |
//     /// | ----- | ---------- |
//     /// | \[a-zA-z_\] | fwd [`TokDfa::identifier_state`] |
//     /// | \[0-9\] | fwd [`TokDfa::number_state`] |
//     /// | """ | [`TokDfa::string_state`] |
//     /// | <whitespace>+ | [`TokDfa::init_state`] |
//     ///
//     /// # Extra rules
//     /// - \<whitespace\> ::= LF, SPACE, TAB, FF, CR
//     fn init_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         if !grapheme.is_ascii() {
//             return Err(TokenizeError::new(
//                 TokenizeErrorType::InvalidToken(grapheme.into()),
//                 TokenizeError::INNOCENCE,
//                 line,
//                 pos,
//             ));
//         }

//         if grapheme.is_ascii_alphabetic() || grapheme == '_' {
//             self.state_fn = Self::identifier_state;
//             self.identifier_state(line, pos, grapheme)
//         } else if grapheme.is_ascii_digit() {
//             self.state_fn = Self::number_state;
//             self.number_state(line, pos, grapheme)
//         } else if grapheme == '"' {
//             self.state_fn = Self::string_state;
//             Ok(self)
//         } else if grapheme == '\'' {
//             self.state_fn = Self::char_state;
//             Ok(self)
//         } else if grapheme.is_ascii_whitespace() {
//             Ok(self)
//         } else {
//             self.symbol_state(line, pos, grapheme)
//         }
//     }

//     fn char_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         if grapheme == '\'' && self.curr_tok.is_none() {
//             return Err(TokenizeError::new(
//                 TokenizeErrorType::InvalidToken(grapheme.into()),
//                 TokenizeError::INNOCENCE,
//                 line,
//                 pos,
//             ));
//         }
//         if self.curr_tok.is_none() {
//             self.curr_tok =
//                 Some(Token::new(TokenType::Char(grapheme), line, pos));
//             return Ok(self);
//         }
//         if grapheme == '\'' {
//             debug_assert!(self.curr_tok.is_some());
//             self.tok_vec.push(mem::take(&mut self.curr_tok).unwrap());
//             return Ok(self);
//         }
//         Err(TokenizeError::new(
//             TokenizeErrorType::InvalidToken(grapheme.into()),
//             TokenizeError::INNOCENCE,
//             line,
//             pos,
//         ))
//     }

//     // must maintain interface.
//     /// String state.
//     ///
//     /// Parameter passing follows the rule defined in [`TokDfa::transition`].
//     ///
//     /// # Transition
//     /// - States preceded by "fwd" indicate the input is forwarded to that
//     ///   state. Otherwise, the input is consumed by the current state.
//     ///
//     /// | input | next-state |
//     /// | ----- | ---------- |
//     /// | [<char>](tokens::TokenType::Char) | [`TokDfa::string_state`] |
//     /// | """ | [`TokDfa::init_state`] |
//     #[allow(clippy::unnecessary_wraps)]
//     fn string_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         if grapheme == '"' {
//             let old_tok = mem::take(&mut self.curr_tok);
//             match old_tok {
//                 None => self.tok_vec.push(Token::new(
//                     TokenType::String(String::new()),
//                     line,
//                     pos,
//                 )),
//                 Some(tok) => {
//                     if let TokenType::String(_) = tok.token_type() {
//                         self.tok_vec.push(tok);
//                     } else {
//                         panic!(
//                             "Internal error: tokenize string_state:
//                             token type is NOT string"
//                         )
//                     }
//                 }
//             }
//             self.state_fn = Self::init_state;
//             return Ok(self);
//         }

//         let old_tok = mem::take(&mut self.curr_tok);
//         match old_tok {
//             None => {
//                 self.curr_tok = Some(Token::new(
//                     TokenType::String(String::from(grapheme)),
//                     line,
//                     pos,
//                 ));
//                 // self.state_fn = Self::init_state;
//                 Ok(self)
//             }
//             Some(tok) => {
//                 if let (TokenType::String(mut s), line, pos) = tok.bind() {
//                     s.push(grapheme);
//                     self.curr_tok =
//                         Some(Token::new(TokenType::String(s), line, pos));
//                     return Ok(self);
//                 }
//                 panic!(
//                     "Internal error: string_state: old token type is NOT
//                     string"
//                 )
//             }
//         }
//     }

//     /// Number state.
//     ///
//     /// Parameter passing follows the rule defined in [`TokDfa::transition`].
//     ///
//     /// # Transition
//     /// | input | next-state |
//     /// | ----- | ---------- |
//     /// | [0-9] | [`TokDfa::number_state`] |
//     /// | "." | todo: ``double_state`` |
//     /// | <whitespace> | [`TokDfa::init_state`] |
//     ///
//     /// # Extra rules
//     /// - \<whitespace\> ::= LF, SPACE, TAB, FF, CR
//     ///
//     /// TODO: add a ``double_state`` to tokenize a double (in integer state then
//     /// receive a dot).
//     ///
//     /// # Note
//     /// - Some special symbols (dot, e) not handled yet.
//     fn number_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         if grapheme.is_ascii_whitespace() {
//             debug_assert!(self.curr_tok.is_some());
//             let curr_tok = mem::take(&mut self.curr_tok).unwrap();
//             self.tok_vec.push(curr_tok);
//             self.state_fn = Self::init_state;
//             return Ok(self);
//         }
//         if grapheme.is_ascii_digit() {
//             if let Some(tok) = mem::take(&mut self.curr_tok) {
//                 let Token {
//                     tok_typ: TokenType::Integer(mut num_str),
//                     ..
//                 } = tok
//                 else {
//                     panic!("Internal error: number_state: wrong token type");
//                 };
//                 if line == tok.line_number {
//                     num_str.push(grapheme);
//                     self.curr_tok = Some(Token::new(
//                         TokenType::Integer(num_str),
//                         tok.line_number,
//                         tok.line_position,
//                     ));
//                 } else {
//                     self.tok_vec.push(Token::new(
//                         TokenType::Integer(num_str),
//                         tok.line_number,
//                         tok.line_position,
//                     ));
//                     self.curr_tok = Some(Token::new(
//                         TokenType::Integer(grapheme.into()),
//                         line,
//                         pos,
//                     ));
//                 }
//                 return Ok(self);
//             }
//             self.curr_tok = Some(Token::new(
//                 TokenType::Integer(grapheme.into()),
//                 line,
//                 pos,
//             ));
//             return Ok(self);
//         }
//         if grapheme == '.' {
//             let (tok_type, line, pos) =
//                 mem::take(&mut self.curr_tok).unwrap().bind();
//             self.curr_tok = Some(Token::new(
//                 TokenType::Double(format!(
//                     "{}.",
//                     match tok_type {
//                         TokenType::Integer(s) => s,
//                         _ => panic!(
//                             "Internal error: number_state: expected integer"
//                         ),
//                     }
//                 )),
//                 line,
//                 pos,
//             ));
//             self.state_fn = Self::double_state;
//             return Ok(self);
//         }
//         // // for now, characters like e, u, and such, still return error.
//         //
//         // Err(TokenizeError::new(
//         //     TokenizeErrorType::InvalidToken(grapheme.into()),
//         //     TokenizeError::INNOCENCE,
//         //     line,
//         //     pos,
//         // ))
//         self.init_state(line, pos, grapheme)
//     }

//     #[allow(unused)]
//     fn double_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         // the assumption is that one can only call double_state if they have
//         // been parsing an integer, then meet a dot.
//         debug_assert!(self.curr_tok.as_ref().is_some_and(|tok| {
//             match &tok.tok_typ {
//                 TokenType::Double(s) => {
//                     s.chars().filter(|&c| c == '.').count() == 1
//                 }
//                 _ => false,
//             }
//         }));
//         if let Some((TokenType::Double(mut s), curr_line, curr_pos)) =
//             mem::take(&mut self.curr_tok).map(Token::bind)
//         {
//             // NOTE: if planning to support number suffixes (eg, e, u), remember
//             // to modify double_state.
//             if grapheme.is_ascii_digit() && curr_line != line {
//                 // expecting more numbers after the dot.
//                 if s.ends_with('.') {
//                     return Err(TokenizeError::new(
//                         TokenizeErrorType::InvalidToken(s),
//                         TokenizeError::INNOCENCE,
//                         curr_line,
//                         curr_pos,
//                     ));
//                 }
//                 self.tok_vec.push(Token::new(
//                     TokenType::Double(s),
//                     curr_line,
//                     curr_pos,
//                 ));
//                 self.state_fn = Self::number_state;
//                 return self.number_state(line, pos, grapheme);
//             } else if grapheme.is_ascii_digit() && curr_line == line {
//                 s.push(grapheme);
//                 self.curr_tok =
//                     Some(Token::new(TokenType::Double(s), curr_line, curr_pos));
//                 return Ok(self);
//             } else if grapheme.is_ascii_whitespace() {
//                 // expecting more numbers after the dot.
//                 if s.ends_with('.') {
//                     return Err(TokenizeError::new(
//                         TokenizeErrorType::InvalidToken(s),
//                         TokenizeError::INNOCENCE,
//                         curr_line,
//                         curr_pos,
//                     ));
//                 }
//                 self.tok_vec.push(Token::new(
//                     TokenType::Double(s),
//                     curr_line,
//                     curr_pos,
//                 ));
//                 return Ok(self);
//             } else if grapheme == '.' {
//                 return Err(TokenizeError::new(
//                     TokenizeErrorType::InvalidToken(grapheme.to_string()),
//                     TokenizeError::INNOCENCE,
//                     curr_line,
//                     curr_pos,
//                 ));
//             }
//             // expecting more numbers after the dot.
//             if s.ends_with('.') {
//                 return Err(TokenizeError::new(
//                     TokenizeErrorType::InvalidToken(s),
//                     TokenizeError::INNOCENCE,
//                     curr_line,
//                     curr_pos,
//                 ));
//             }
//             self.curr_tok =
//                 Some(Token::new(TokenType::Double(s), curr_line, curr_pos));
//             self.state_fn = Self::init_state;
//             return self.init_state(line, pos, grapheme);
//         }

//         panic!("Internal error: double_state: called when type is not double")
//     }

//     /// Identifier state.
//     ///
//     /// Parameter passing follows the rule defined in [`TokDfa::transition`].
//     ///
//     /// # Transition
//     ///
//     /// | input | next-state |
//     /// | ----- | ---------- |
//     /// | [a-zA-z0-9_] | [`TokDfa::identifier_state`] |
//     /// | <whitespace> | [`TokDfa::init_state`] |
//     ///
//     /// # Extra rules
//     /// - When processing a \<whitespace\>, performs a keyword lookup for the
//     ///   currently held identifier.
//     ///   - If the identifier matches a keyword (e.g. "let"), change the token
//     ///     type to the corresponding keyword type (e.g. [`TokenType::Let`]).
//     fn identifier_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         // ensure grapheme is 1 ASCII character.
//         if !grapheme.is_ascii() {
//             return Err(TokenizeError::new(
//                 TokenizeErrorType::InvalidToken(grapheme.into()),
//                 TokenizeError::INNOCENCE,
//                 line,
//                 pos,
//             ));
//         }
//         #[cfg(debug_assertions)]
//         if self.curr_tok.is_none() {
//             debug_assert!(grapheme.is_ascii_alphabetic() || grapheme == '_');
//         }

//         if grapheme.is_ascii_whitespace() {
//             debug_assert!(self.curr_tok.is_some());
//             let old_tok = mem::take(&mut self.curr_tok).unwrap();

//             // in case the token is a reserved keyword
//             if let (TokenType::Identifier(s), line, pos) = old_tok.bind_ref() {
//                 if let Some(kw) = tokens::keyword_lookup(s.as_str()) {
//                     self.tok_vec.push(Token::new(kw, line, pos));
//                 } else {
//                     self.tok_vec.push(old_tok);
//                 }
//             } else {
//                 panic!("Internal error: identifier_state: wrong token type")
//             }
//             self.state_fn = Self::init_state;
//             return Ok(self);
//         }
//         if grapheme.is_ascii_alphanumeric() || grapheme == '_' {
//             let old_tok = mem::take(&mut self.curr_tok);
//             if old_tok.is_none() {
//                 self.curr_tok = Some(Token::new(
//                     TokenType::Identifier(grapheme.into()),
//                     line,
//                     pos,
//                 ));
//                 return Ok(self);
//             }
//             let old_tok = old_tok.unwrap();
//             if let (TokenType::Identifier(mut s), line, pos) = old_tok.bind() {
//                 s.push(grapheme);
//                 self.curr_tok =
//                     Some(Token::new(TokenType::Identifier(s), line, pos));
//                 return Ok(self);
//             }
//             panic!("Internal error: identifier_state: wrong token type")
//         }
//         if let Some(tok) = mem::take(&mut self.curr_tok) {
//             self.tok_vec.push(tok);
//         }
//         self.init_state(line, pos, grapheme)
//     }

//     /// Symbol state
//     ///
//     /// Parameter passing follows the rule defined in [`TokDfa::transition`].
//     ///
//     /// # Transition
//     ///
//     /// | input | next-state |
//     /// | ----- | ---------- |
//     /// | \[+\-*/\(\)"\\;,:.\] | [`TokDfa::init_state`] |
//     /// | "/" | [`TokDfa::slash_state`] |
//     /// | "!" | [`TokDfa::bang_state`] |
//     /// | "<" | [`TokDfa::lpbrace_state`] |
//     /// | ">" | [`TokDfa::rpbrace_state`] |
//     /// | "=" | [`TokDfa::equal_state`] |
//     fn symbol_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         let mut set_state_and_return =
//             |state_fn: StateFn, rettok: TokenType| {
//                 if let Some(tok) = mem::take(&mut self.curr_tok) {
//                     self.tok_vec.push(tok);
//                 }
//                 self.state_fn = state_fn;
//                 Ok(rettok)
//             };

//         let tok_type = match grapheme {
//             '+' => set_state_and_return(Self::init_state, TokenType::Plus),
//             '-' => set_state_and_return(Self::init_state, TokenType::Dash),
//             '*' => set_state_and_return(Self::init_state, TokenType::Star),
//             '/' => set_state_and_return(Self::slash_state, TokenType::Slash),
//             '%' => set_state_and_return(Self::init_state, TokenType::Percent),
//             '^' => set_state_and_return(Self::init_state, TokenType::Hat),
//             '&' => set_state_and_return(
//                 Self::ampersand_state,
//                 TokenType::Ampersand,
//             ),
//             '|' => set_state_and_return(Self::beam_state, TokenType::Beam),
//             '\\' => {
//                 set_state_and_return(Self::init_state, TokenType::Backslash)
//             }
//             '(' => set_state_and_return(Self::init_state, TokenType::LParen),
//             ')' => set_state_and_return(Self::init_state, TokenType::RParen),
//             '{' => set_state_and_return(Self::init_state, TokenType::LCParen),
//             '}' => set_state_and_return(Self::init_state, TokenType::RCParen),
//             '.' => set_state_and_return(Self::init_state, TokenType::Dot),
//             ':' => set_state_and_return(Self::colon_state, TokenType::Colon),
//             ';' => set_state_and_return(Self::init_state, TokenType::Semicolon),
//             ',' => set_state_and_return(Self::init_state, TokenType::Comma),
//             '=' => set_state_and_return(Self::equal_state, TokenType::Equal),
//             '!' => set_state_and_return(Self::bang_state, TokenType::Bang),
//             '<' => {
//                 set_state_and_return(Self::lpbrace_state, TokenType::LPBrace)
//             }
//             '>' => {
//                 set_state_and_return(Self::rpbrace_state, TokenType::RPBrace)
//             }
//             _ => Err(TokenizeError::new(
//                 TokenizeErrorType::InvalidToken(grapheme.into()),
//                 TokenizeError::INNOCENCE,
//                 line,
//                 pos,
//             )),
//         }?;
//         self.tok_vec.push(Token::new(tok_type, line, pos));
//         Ok(self)
//     }

//     /// Equal state
//     ///
//     /// Parameter passing follows the rule defined in [`TokDfa::transition`].
//     /// Next states marked with "fwd" simply forwards the information passed in
//     /// to the next state. Otherwise, this current state consumes the input.
//     ///
//     /// # Transition
//     ///
//     /// | input | next-state |
//     /// | "=" | [`TokDfa::init_state`] |
//     /// | else | fwd [`TokDfa::init_state`] |
//     fn equal_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         if grapheme == '=' && self.tok_vec.last().unwrap().line_number == line {
//             *self.tok_vec.last_mut().unwrap() =
//                 Token::new(TokenType::EqualEqual, line, pos);
//             self.state_fn = Self::init_state;
//             return Ok(self);
//         }
//         self.init_state(line, pos, grapheme)
//     }

//     /// Ampersand state
//     ///
//     /// Parameter passing follows the rule defined in [`TokDfa::transition`].
//     /// Next states marked with "fwd" simply forwards the information passed in
//     /// to the next state. Otherwise, this current state consumes the input.
//     ///
//     /// # Transition
//     ///
//     /// | input | next-state |
//     /// | "&" | [`TokDfa::init_state`] |
//     /// | else | fwd [`TokDfa::init_state`] |
//     fn ampersand_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         debug_assert!(matches!(
//             self.tok_vec.last(),
//             Some(Token {
//                 tok_typ: TokenType::Ampersand,
//                 ..
//             })
//         ));
//         let (_, curr_line, curr_pos) = self.tok_vec.last().unwrap().bind_ref();
//         if grapheme == '&' && curr_line == line {
//             self.tok_vec.pop();
//             self.tok_vec.push(Token::new(
//                 TokenType::AmpersandAmpersand,
//                 curr_line,
//                 curr_pos,
//             ));
//             self.state_fn = Self::init_state;
//             Ok(self)
//         } else {
//             self.state_fn = Self::init_state;
//             self.init_state(line, pos, grapheme)
//         }
//     }

//     /// Beam state
//     ///
//     /// Parameter passing follows the rule defined in [`TokDfa::transition`].
//     /// Next states marked with "fwd" simply forwards the information passed in
//     /// to the next state. Otherwise, this current state consumes the input.
//     ///
//     /// # Transition
//     ///
//     /// | input | next-state |
//     /// | "|" | [`TokDfa::init_state`] |
//     /// | else | fwd [`TokDfa::init_state`] |
//     fn beam_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         debug_assert!(matches!(
//             self.tok_vec.last(),
//             Some(Token {
//                 tok_typ: TokenType::Beam,
//                 ..
//             })
//         ));

//         let (_, curr_line, curr_pos) = self.tok_vec.last().unwrap().bind_ref();
//         if grapheme == '|' && curr_line == line {
//             self.tok_vec.pop();
//             self.tok_vec.push(Token::new(
//                 TokenType::BeamBeam,
//                 curr_line,
//                 curr_pos,
//             ));
//             self.state_fn = Self::init_state;
//             Ok(self)
//         } else {
//             self.state_fn = Self::init_state;
//             self.init_state(line, pos, grapheme)
//         }
//     }

//     /// Left-pointy-brace state
//     ///
//     /// Parameter passing follows the rule defined in [`TokDfa::transition`].
//     /// Next states marked with "fwd" simply forwards the information passed in
//     /// to the next state. Otherwise, this current state consumes the input.
//     ///
//     /// # Transition
//     ///
//     /// | input | next-state |
//     /// | "=" | [`TokDfa::init_state`] |
//     /// | else | fwd [`TokDfa::init_state`] |
//     fn lpbrace_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         if grapheme == '=' && self.tok_vec.last().unwrap().line_number == line {
//             *self.tok_vec.last_mut().unwrap() =
//                 Token::new(TokenType::LPBraceEqual, line, pos);
//             self.state_fn = Self::init_state;
//             return Ok(self);
//         }
//         self.init_state(line, pos, grapheme)
//     }

//     /// Right-pointy-brace state
//     ///
//     /// Parameter passing follows the rule defined in [`TokDfa::transition`].
//     /// Next states marked with "fwd" simply forwards the information passed in
//     /// to the next state. Otherwise, this current state consumes the input.
//     ///
//     /// # Transition
//     ///
//     /// | input | next-state |
//     /// | "=" | [`TokDfa::init_state`] |
//     /// | else | fwd [`TokDfa::init_state`] |
//     fn rpbrace_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         if grapheme == '=' && self.tok_vec.last().unwrap().line_number == line {
//             *self.tok_vec.last_mut().unwrap() =
//                 Token::new(TokenType::RPBraceEqual, line, pos);
//             self.state_fn = Self::init_state;
//             return Ok(self);
//         }
//         self.init_state(line, pos, grapheme)
//     }

//     /// Slash state
//     ///
//     /// Parameter passing follows the rule defined in [`TokDfa::transition`].
//     /// Next states marked with "fwd" simply forwards the information passed in
//     /// to the next state. Otherwise, this current state consumes the input.
//     ///
//     /// # Transition
//     ///
//     /// | input | next-state |
//     /// | "/" | [`TokDfa::comment_state`] |
//     /// | else | fwd [`TokDfa::init_state`] |
//     fn slash_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         if grapheme == '/' && self.tok_vec.last().unwrap().line_number == line {
//             *self.tok_vec.last_mut().unwrap() =
//                 Token::new(TokenType::SlashSlash, line, pos);
//             self.state_fn = Self::comment_state;
//             return Ok(self);
//         }
//         self.init_state(line, pos, grapheme)
//     }

//     /// Colon state
//     ///
//     /// Parameter passing follows the rule defined in [`TokDfa::transition`].
//     /// Next states marked with "fwd" simply forwards the information passed in
//     /// to the next state. Otherwise, this current state consumes the input.
//     ///
//     /// # Transition
//     ///
//     /// | input | next-state |
//     /// | ":" | [`TokDfa::init_state`] |
//     /// | else | fwd [`TokDfa::init_state`] |
//     fn colon_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         if grapheme == ':' && self.tok_vec.last().unwrap().line_number == line {
//             *self.tok_vec.last_mut().unwrap() =
//                 Token::new(TokenType::ColonColon, line, pos);
//             self.state_fn = Self::init_state;
//             return Ok(self);
//         }
//         self.init_state(line, pos, grapheme)
//     }

//     /// Bang state
//     ///
//     /// Parameter passing follows the rule defined in [`TokDfa::transition`].
//     /// Next states marked with "fwd" simply forwards the information passed in
//     /// to the next state. Otherwise, this current state consumes the input.
//     ///
//     /// # Transition
//     ///
//     /// | input | next-state |
//     /// | "=" | [`TokDfa::init_state`] |
//     /// | else | fwd [`TokDfa::init_state`] |
//     fn bang_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         if grapheme == '=' {
//             *self.tok_vec.last_mut().unwrap() =
//                 Token::new(TokenType::BangEqual, line, pos);
//             self.state_fn = Self::init_state;
//             return Ok(self);
//         }
//         self.init_state(line, pos, grapheme)
//     }

//     /// Comment state
//     ///
//     /// Parameter passing follows the rule defined in [`TokDfa::transition`].
//     /// Next states marked with "fwd" simply forwards the information passed in
//     /// to the next state. Otherwise, this current state consumes the input.
//     ///
//     /// # Transition
//     ///
//     /// | input | next-state |
//     /// | LF or CRLF | [`TokDfa::init_state`] |
//     /// | else | [`TokDfa::comment_state`] |
//     ///
//     /// # Extra rules
//     /// - We check the line number to determine whether we did meet a LF or
//     ///   CRLF. We don't have the newline character because we trimmed it in
//     ///   the main tokenize function.
//     #[allow(clippy::unnecessary_wraps)]
//     fn comment_state(
//         mut self,
//         line: usize,
//         pos: usize,
//         grapheme: char,
//     ) -> Result<Self, TokenizeError> {
//         // we trim newlines anyways
//         debug_assert_ne!(grapheme, '\n');

//         debug_assert!(self.tok_vec.last().is_some());
//         debug_assert_eq!(
//             self.tok_vec.last().unwrap().tok_typ,
//             TokenType::SlashSlash
//         );
//         let curr_line = self.tok_vec.last().unwrap().line_number;
//         if line > curr_line {
//             self.tok_vec.pop();
//             self.init_state(line, pos, grapheme)
//         } else {
//             Ok(self)
//         }
//     }
// }
