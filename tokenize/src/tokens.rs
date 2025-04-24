/// All the possible tokens.
/// String-type owns its string.
#[derive(Debug, PartialEq)]
pub enum TokenType {
    /// Just an identifier. ASCII.
    /// Rule:
    /// <identifier> ::= [a-zA-Z_][a-zA-Z0-9_]*
    Identifier(String),
    /// Holds an i64
    /// Rule:
    /// <integer> ::= [0-9]+
    Integer(i64),
    /// Holds a f64
    /// Rule:
    /// <double> ::= [0-9]* "." [0-9]+
    Double(f64),
    /// Literal string, Unicode.
    /// Rule:
    /// <string> ::= """ <char>* """
    String(String),
    /// single character, one or more Unicode code points.
    /// Rule:
    /// <char>
    Char(char),
    // single-character symbols, <symbol>
    /// +
    Plus,
    /// -
    Dash,
    /// *
    Star,
    /// /
    Slash,
    /// \
    Backslash,
    /// (
    LParen,
    /// )
    RParen,
    /// .
    Dot,
    /// :
    Colon,
    /// ;
    Semicolon,
    /// ,
    Comma,
    /// =
    Equal,
    /// !
    Bang,
    /// <
    LPBrace,
    /// >
    RPBrace,
    // multiple-character symbols, <symbol>
    /// !=
    BangEqual,
    /// <=
    LPBraceEqual,
    /// >=
    RPBraceEqual,
    /// ==
    EqualEqual,
    // keywords
    Ya,
    Na,
    Let,
    Static,
    Proc,
    Return,
}

/// A token. Duh.
/// A tokenizer reads an input string and spits out smaller chunks, which are
/// these.
///
/// * `token_type`: The type of token.
/// * `line_number`: The line number.
/// * `line_position`: The position in that line.
#[derive(Debug, PartialEq)]
pub struct Token {
    token_type: TokenType,
    line_number: usize,
    line_position: usize,
}

impl Token {
    /// Constructs a new Token
    #[must_use]
    pub fn new(
        token_type: TokenType,
        line_number: usize,
        line_position: usize,
    ) -> Self {
        Self {
            token_type,
            line_number,
            line_position,
        }
    }

    /// Gets the contained token type.
    #[must_use]
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }

    /// Gets the line number of the current token.
    #[must_use]
    pub fn line_number(&self) -> usize {
        self.line_number
    }

    /// Gets the line position of the current token.
    #[must_use]
    pub fn line_position(&self) -> usize {
        self.line_position
    }
}
