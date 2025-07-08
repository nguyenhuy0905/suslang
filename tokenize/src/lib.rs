// TODO: remove allow unused when done
// #![allow(unused)]

#[cfg(test)]
mod test;
pub mod tokens;

use std::error::Error;
use std::fmt::Display;

use unicode_segmentation as us;

use tokens::CharPosition;
pub use tokens::{Token, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenizeError {
    err_type: TokenizeErrorType,
    pos: CharPosition,
}

impl TokenizeError {
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
pub struct Tokenizer<'a> {
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
    Comment,
}

macro_rules! match_all_symbols {
    () => {
        match_all_symbols!(no_dot) | '.'
    };
    // hacky i know
    (no_dot) => {
        '\"' | '\''
            | '/'
            | '&'
            | '|'
            | '='
            | '<'
            | '>'
            | '!'
            | '+'
            | '-'
            | '*'
            | ';'
            | ','
            | '('
            | ')'
            | '{'
            | '}'
    };
}

impl<'a> Tokenizer<'a> {
    /// Transforms the `refstr` into a list of [`Token`]s.
    ///
    /// # Errors
    /// - If tokenization ends in a non-final state (e.g. string state,
    ///   expecting a closing double-quote), an error is returned.
    /// - If a non-ASCII character appears without being inside a string or
    ///   char quote, that's also an error.
    pub fn tokenize(refstr: &'a str) -> Result<Vec<Token>, TokenizeError> {
        let mut tok = Tokenizer::new(refstr);
        while !tok.is_done() {
            tok.transit()?;
        }

        match tok.state {
            TokenizeState::Init => Ok(()),
            TokenizeState::Ident => {
                let lookup = tokens::keyword_lookup(tok.get_window_slice());
                tok.tokens.push(Token {
                    kind: lookup,
                    pos: tok.begin_pos,
                    repr: if lookup == TokenKind::Identifier {
                        Some(Box::from(tok.get_window_slice()))
                    } else {
                        None
                    },
                });
                Ok(())
            }
            TokenizeState::Number => {
                tok.tokens.push(Token {
                    kind: TokenKind::Integer,
                    pos: tok.begin_pos,
                    repr: Some(Box::from(tok.get_window_slice())),
                });
                Ok(())
            }
            TokenizeState::Float => {
                tok.tokens.push(Token {
                    kind: TokenKind::Float,
                    pos: tok.begin_pos,
                    repr: Some(Box::from(tok.get_window_slice())),
                });
                Ok(())
            }
            TokenizeState::String | TokenizeState::Char => Err(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: tok.begin_pos,
            }),
            TokenizeState::Slash => {
                tok.tokens.push(Token {
                    kind: TokenKind::Slash,
                    pos: tok.begin_pos,
                    repr: None,
                });
                Ok(())
            }
            TokenizeState::Amper => {
                tok.tokens.push(Token {
                    kind: TokenKind::Ampersand,
                    pos: tok.begin_pos,
                    repr: None,
                });
                Ok(())
            }
            TokenizeState::Beam => {
                tok.tokens.push(Token {
                    kind: TokenKind::Beam,
                    pos: tok.begin_pos,
                    repr: None,
                });
                Ok(())
            }
            TokenizeState::Equal => {
                tok.tokens.push(Token {
                    kind: TokenKind::Equal,
                    pos: tok.begin_pos,
                    repr: None,
                });
                Ok(())
            }
            TokenizeState::Bang => {
                tok.tokens.push(Token {
                    kind: TokenKind::Bang,
                    pos: tok.begin_pos,
                    repr: None,
                });
                Ok(())
            }
            TokenizeState::Less => {
                tok.tokens.push(Token {
                    kind: TokenKind::Less,
                    pos: tok.begin_pos,
                    repr: None,
                });
                Ok(())
            }
            TokenizeState::Greater => {
                tok.tokens.push(Token {
                    kind: TokenKind::Greater,
                    pos: tok.begin_pos,
                    repr: None,
                });
                Ok(())
            }
            TokenizeState::Comment => Ok(()),
        }?;

        Ok(tok.tokens)
    }

    fn new(refstr: &'a str) -> Self {
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

    fn transit(&mut self) -> Result<(), TokenizeError> {
        match self.state {
            TokenizeState::Init => self.init_transit(),
            TokenizeState::Ident => self.ident_transit(),
            TokenizeState::Number => self.number_transit(),
            TokenizeState::Float => self.float_transit(),
            TokenizeState::String => self.string_transit(),
            TokenizeState::Char => self.char_transit(),
            TokenizeState::Slash => self.slash_transit(),
            TokenizeState::Amper => self.amper_transit(),
            TokenizeState::Beam => self.beam_transit(),
            TokenizeState::Equal => self.equal_transit(),
            TokenizeState::Bang => self.bang_transit(),
            TokenizeState::Less => self.less_transit(),
            TokenizeState::Greater => self.greater_transit(),
            TokenizeState::Comment => self.comment_transit(),
        }
    }

    /// ## Transitions:
    ///
    /// - [a-zA-Z_] => advance to `TokenizeState::Ident`
    /// - [0-9] => advance to `TokenizeState::Number`
    /// - '\n' | '\t' | ' ' => advance to itself
    /// - '.' | '+' | '-' | '*' | ';' | ',' | '(' | ')' | '{' | '}' => add the
    ///   corresponding token, does not change state.
    /// - '\"' => advance to string state.
    /// - '\'' => advance to char state.
    /// - '&' | '|' | '=' | '<' | '>' | '!' => advance to their corresponding
    ///   states.
    ///
    /// ## Safety
    /// - This method assumes the current window is empty.
    /// - So, before returning to init state, make sure to `self.empty_window`.
    ///   If that isn't possible, consider adding another state.
    fn init_transit(&mut self) -> Result<(), TokenizeError> {
        debug_assert!(!self.is_done());
        // we assume at this state, the window is empty, for now
        debug_assert!(self.window_begin == self.window_end);

        let add_single_symbol = |curr_inst: &mut Self, kind: TokenKind| {
            let (_, pos) = curr_inst.consume_next_char().unwrap();
            curr_inst.tokens.push(Token {
                kind,
                pos,
                repr: None,
            });
            curr_inst.slide_begin_right();
        };

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
                    self.slide_begin_right();
                    self.state = TokenizeState::String;
                    Ok(())
                }
                '\'' => {
                    self.consume_next_char();
                    self.slide_begin_right();
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
                    self.empty_window();
                    Ok(())
                }
                '.' => {
                    add_single_symbol(self, TokenKind::Dot);
                    Ok(())
                }
                '+' => {
                    add_single_symbol(self, TokenKind::Plus);
                    Ok(())
                }
                '-' => {
                    add_single_symbol(self, TokenKind::Dash);
                    Ok(())
                }
                '*' => {
                    add_single_symbol(self, TokenKind::Star);
                    Ok(())
                }
                ',' => {
                    add_single_symbol(self, TokenKind::Comma);
                    Ok(())
                }
                ';' => {
                    add_single_symbol(self, TokenKind::Semicolon);
                    Ok(())
                }
                '(' => {
                    add_single_symbol(self, TokenKind::LParen);
                    Ok(())
                }
                ')' => {
                    add_single_symbol(self, TokenKind::RParen);
                    Ok(())
                }
                '{' => {
                    add_single_symbol(self, TokenKind::LBrace);
                    Ok(())
                }
                '}' => {
                    add_single_symbol(self, TokenKind::RBrace);
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
                        repr: if kind == TokenKind::Identifier {
                            Some(Box::from(self.get_window_slice()))
                        } else {
                            None
                        },
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
                '0'..='9' => {
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
                '\n' | '\t' | ' ' | match_all_symbols!(no_dot) => {
                    self.tokens.push(Token {
                        kind: TokenKind::Integer,
                        pos: self.begin_pos,
                        repr: Some(Box::from(self.get_window_slice())),
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
                '0'..='9' => {
                    self.consume_next_char();
                    Ok(())
                }

                '\n' | '\t' | ' ' | match_all_symbols!(no_dot) => {
                    self.tokens.push(Token {
                        kind: TokenKind::Integer,
                        pos: self.begin_pos,
                        // a bit complicated, but:
                        // - if last character is not a number, bork it.
                        // - HACK: for now; we don't allow suffixes.
                        repr: if gr.is_numeric() {
                            Ok(Some(Box::from(self.get_window_slice())))
                        } else {
                            Err(TokenizeError {
                                err_type: TokenizeErrorType::InvalidChar(gr),
                                pos: self.end_pos,
                            })
                        }?,
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
                        kind: TokenKind::String,
                        pos: self.begin_pos,
                        repr: Some(Box::from(self.get_window_slice())),
                    });
                    self.consume_next_char();
                    self.state = TokenizeState::Init;
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
            .and_then(|(gr, gr_pos)| {
                if gr != '\'' {
                    self.tokens.push(Token {
                        kind: TokenKind::Char,
                        pos: gr_pos,
                        repr: Some(Box::from(gr.to_string())),
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
            .and_then(|(gr, gr_pos)| {
                if gr == '\'' {
                    self.empty_window();
                    self.state = TokenizeState::Init;
                    Ok(())
                } else {
                    Err(TokenizeError {
                        err_type: TokenizeErrorType::InvalidChar(gr),
                        pos: gr_pos,
                    })
                }
            })
    }

    fn slash_transit(&mut self) -> Result<(), TokenizeError> {
        self.peek_next_char()
            .ok_or(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: self.end_pos,
            })
            .and_then(|gr| match gr {
                '/' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Comment;
                    Ok(())
                }
                _ => {
                    self.tokens.push(Token {
                        kind: TokenKind::Slash,
                        pos: self.begin_pos,
                        repr: None,
                    });
                    self.empty_window();
                    self.state = TokenizeState::Init;
                    Ok(())
                }
            })
    }

    fn amper_transit(&mut self) -> Result<(), TokenizeError> {
        self.peek_next_char()
            .ok_or(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: self.end_pos,
            })
            .and_then(|gr| match gr {
                '&' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Init;
                    self.tokens.push(Token {
                        kind: TokenKind::AmpersandAmpersand,
                        pos: self.begin_pos,
                        repr: None,
                    });
                    self.empty_window();
                    Ok(())
                }
                _ => {
                    self.tokens.push(Token {
                        kind: TokenKind::Ampersand,
                        pos: self.begin_pos,
                        repr: None,
                    });
                    self.empty_window();
                    self.state = TokenizeState::Init;
                    Ok(())
                }
            })
    }

    fn beam_transit(&mut self) -> Result<(), TokenizeError> {
        self.peek_next_char()
            .ok_or(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: self.end_pos,
            })
            .and_then(|gr| match gr {
                '|' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Init;
                    self.tokens.push(Token {
                        kind: TokenKind::BeamBeam,
                        pos: self.begin_pos,
                        repr: None,
                    });
                    self.empty_window();
                    Ok(())
                }
                _ => {
                    self.tokens.push(Token {
                        kind: TokenKind::Beam,
                        pos: self.begin_pos,
                        repr: None,
                    });
                    self.empty_window();
                    self.state = TokenizeState::Init;
                    Ok(())
                }
            })
    }

    fn equal_transit(&mut self) -> Result<(), TokenizeError> {
        self.peek_next_char()
            .ok_or(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: self.end_pos,
            })
            .and_then(|gr| match gr {
                '=' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Init;
                    self.tokens.push(Token {
                        kind: TokenKind::EqualEqual,
                        pos: self.begin_pos,
                        repr: None,
                    });
                    self.empty_window();
                    Ok(())
                }
                _ => {
                    self.tokens.push(Token {
                        kind: TokenKind::Equal,
                        pos: self.begin_pos,
                        repr: None,
                    });
                    self.empty_window();
                    self.state = TokenizeState::Init;
                    Ok(())
                }
            })
    }

    fn bang_transit(&mut self) -> Result<(), TokenizeError> {
        self.peek_next_char()
            .ok_or(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: self.end_pos,
            })
            .and_then(|gr| match gr {
                '=' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Init;
                    self.tokens.push(Token {
                        kind: TokenKind::BangEqual,
                        pos: self.begin_pos,
                        repr: None,
                    });
                    self.empty_window();
                    Ok(())
                }
                _ => {
                    self.tokens.push(Token {
                        kind: TokenKind::Bang,
                        pos: self.begin_pos,
                        repr: None,
                    });
                    self.empty_window();
                    self.state = TokenizeState::Init;
                    Ok(())
                }
            })
    }

    fn less_transit(&mut self) -> Result<(), TokenizeError> {
        self.peek_next_char()
            .ok_or(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: self.end_pos,
            })
            .and_then(|gr| match gr {
                '=' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Init;
                    self.tokens.push(Token {
                        kind: TokenKind::LessEqual,
                        pos: self.begin_pos,
                        repr: None,
                    });
                    self.empty_window();
                    Ok(())
                }
                _ => {
                    self.tokens.push(Token {
                        kind: TokenKind::Less,
                        pos: self.begin_pos,
                        repr: None,
                    });
                    self.empty_window();
                    self.state = TokenizeState::Init;
                    Ok(())
                }
            })
    }

    fn greater_transit(&mut self) -> Result<(), TokenizeError> {
        self.peek_next_char()
            .ok_or(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: self.end_pos,
            })
            .and_then(|gr| match gr {
                '=' => {
                    self.consume_next_char();
                    self.state = TokenizeState::Init;
                    self.tokens.push(Token {
                        kind: TokenKind::GreaterEqual,
                        pos: self.begin_pos,
                        repr: None,
                    });
                    self.empty_window();
                    Ok(())
                }
                _ => {
                    self.tokens.push(Token {
                        kind: TokenKind::Greater,
                        pos: self.begin_pos,
                        repr: None,
                    });
                    self.empty_window();
                    self.state = TokenizeState::Init;
                    Ok(())
                }
            })
    }

    fn comment_transit(&mut self) -> Result<(), TokenizeError> {
        self.peek_next_char()
            .ok_or(TokenizeError {
                err_type: TokenizeErrorType::ExpectChar,
                pos: self.end_pos,
            })
            .and_then(|gr| match gr {
                '\n' => {
                    self.consume_next_char();
                    self.empty_window();
                    self.state = TokenizeState::Init;
                    Ok(())
                }
                _ => {
                    self.consume_next_char();
                    Ok(())
                }
            })
    }

    /// If this returns true, we should stop tokenizing.
    #[inline]
    #[must_use]
    fn is_done(&mut self) -> bool {
        // nothing to peek == nothing to consume
        self.graphemes.peek().is_none()
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
        // create a grapheme iterator starting at the window's begin,
        us::UnicodeSegmentation::graphemes(
            &self.refstr[self.window_begin..],
            true,
        )
        .next()
        .inspect(|&gr| {
            self.window_begin += gr.len();
            if gr == "\n" {
                self.begin_pos.line += 1;
                self.begin_pos.column = 1;
                return;
            }
            self.begin_pos.column += 1;
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
            self.end_pos.column += 1;
        });
    }

    /// Returns the next character and its position, after sliding the window's
    /// end rightwards.
    #[inline]
    fn consume_next_char(&mut self) -> Option<(char, CharPosition)> {
        let ret_pos = self.end_pos;
        self.slide_end_right();
        self.graphemes
            .next()
            .inspect(|(_, gr)| self.window_end += gr.len())
            // if this doesn't work, unicode_segmentation is broken.
            // At which point, we're gonna have to clone and fix the source
            // code.
            .map(|(_, gr)| (gr.parse::<char>().unwrap(), ret_pos))
    }

    /// NOTE: the `peek` method takes a `mut` reference. However, what we can
    /// observe won't change with multiple calls of this method.
    ///
    /// With this method, the peeked character's current position is
    /// `self.end_pos`.
    #[inline]
    fn peek_next_char(&mut self) -> Option<char> {
        self.graphemes
            .peek()
            .map(|&(_, gr)| gr.parse::<char>().unwrap())
    }
}
