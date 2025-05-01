use std::collections::HashMap;
use std::sync::LazyLock;

/// All the possible tokens.
/// String-type owns its string.
///
/// While it's certainly possible for this enum to only hold references,
/// letting it own the string allows more flexible ways of receiving
/// input, such as line-by-line (which, the non-owning scheme forces one to
/// hold each line until the end of time).
#[derive(Debug, PartialEq, Clone)]
#[repr(u8)]
pub enum TokenType {
    /// Just an identifier. ASCII.
    /// # Rule
    /// \<identifier\> ::= \[a-zA-Z_\]\[a-zA-Z0-9_\]*
    Identifier(String) = 0,
    /// Holds an i64
    /// # Rule
    /// \<integer\> ::= \[0-9\]+
    /// Integer and Double hold a string.
    Integer(String),
    /// Holds a f64
    /// # Rule
    /// \<double\> ::= \[0-9\]* "." \[0-9\]+
    /// Integer and Double hold a string.
    Double(String),
    /// Literal string, Unicode.
    /// # Rule
    /// \<string\> ::= """ \<char\>* """
    String(String),
    /// single character, one or more Unicode code points.
    /// Rule:
    /// \<char\> ::= \<unicode-code-points\>
    /// \<unicode-code-point\> ::= \
    /// \[0b10000000-0b11111111\]{0, 3}\[0b00000000-0b01111111\]
    Char(char),
    // single-character symbols, <symbol>
    /// Literal symbol "+"
    Plus,
    /// Literal symbol "-"
    Dash,
    /// Literal symbol "*"
    Star,
    /// Literal symbol "/"
    Slash,
    /// Literal symbol "\"
    Backslash,
    /// Literal symbol "("
    LParen,
    /// Literal symbol ")"
    RParen,
    /// Literal symbol "{"
    LCParen,
    /// Literal symbol "}"
    RCParen,
    /// Literal symbol "."
    Dot,
    /// Literal symbol ":"
    Colon,
    /// Literal symbol ";"
    Semicolon,
    /// Literal symbol ","
    Comma,
    /// Literal symbol "="
    Equal,
    /// Literal symbol "!"
    Bang,
    /// Literal symbol "<"
    LPBrace,
    /// Literal symbol ">"
    RPBrace,
    // multiple-character symbols, <symbol>
    /// Literal symbol "!="
    BangEqual,
    /// Literal symbol "<="
    LPBraceEqual,
    /// Literal symbol ">="
    RPBraceEqual,
    /// Literal symbol "=="
    EqualEqual,
    /// Literal symbol "//"
    SlashSlash,
    /// Literal symbol "::"
    ColonColon,
    // keywords
    /// Keyword "ya"
    Ya,
    /// Keyword "na"
    Na,
    /// Keyword "if"
    If,
    /// Keyword "elif"
    Elif,
    /// Keyword "else"
    Else,
    /// Keyword "let"
    Let,
    /// Keyword "ref"
    Ref,
    /// Keyword "static"
    Static,
    /// Keyword "proc"
    Proc,
    /// Keyword "return"
    Return,
}

#[must_use]
pub fn keyword_lookup(key: &str) -> Option<TokenType> {
    static LOOKUP_TBL: LazyLock<HashMap<&'static str, TokenType>> =
        LazyLock::new(|| {
            HashMap::<&'static str, TokenType>::from([
                ("ya", TokenType::Ya),
                ("na", TokenType::Na),
                ("if", TokenType::If),
                ("elif", TokenType::Elif),
                ("else", TokenType::Else),
                ("let", TokenType::Let),
                ("ref", TokenType::Ref),
                ("static", TokenType::Static),
                ("proc", TokenType::Proc),
                ("return", TokenType::Return),
            ])
        });

    // none of the enum in here contains anything inside, so Imma just clone
    // them.
    LOOKUP_TBL.get(key).cloned()
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
    // y'know what, fuck safety.
    pub(super) token_type: TokenType,
    pub(super) line_number: usize,
    pub(super) line_position: usize,
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

    // TODO: use a struct to better name these 2 usizes
    #[must_use]
    pub fn bind(self) -> (TokenType, usize, usize) {
        (self.token_type, self.line_number, self.line_position)
    }

    #[must_use]
    pub fn bind_ref(&self) -> (&TokenType, usize, usize) {
        (&self.token_type, self.line_number, self.line_position)
    }
}
