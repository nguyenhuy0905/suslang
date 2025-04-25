/// All the possible tokens.
/// String-type owns its string.
///
/// While it's certainly possible for this enum to only hold references,
/// letting it own the string allows more flexible ways of receiving
/// input, such as line-by-line (which, the non-owning scheme forces one to
/// hold each line until the end of time).
#[derive(Debug, PartialEq)]
pub enum TokenType {
    /// Just an identifier. ASCII.
    /// Rule:
    /// <identifier> ::= \[a-zA-Z_\]\[a-zA-Z0-9_\]*
    Identifier(String),
    /// Holds an i64
    /// Rule:
    /// <integer> ::= ("+" | "-")? \[0-9\]+
    /// Integer and Double hold a string.
    Integer(String),
    /// Holds a f64
    /// Rule:
    /// <double> ::= ("+" | "-")? \[0-9\]* "." \[0-9\]+
    /// Integer and Double hold a string.
    Double(String),
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
    /// //
    SlashSlash,
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

    #[must_use]
    pub fn move_token_type(self) -> TokenType {
        self.token_type
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

    #[must_use]
    pub fn bind(self) -> (TokenType, usize, usize) {
        (self.token_type, self.line_number, self.line_position)
    }
}
