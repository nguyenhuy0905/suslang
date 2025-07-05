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
pub enum TokenKind {
    /// Just an identifier. ASCII.
    /// # Rule
    /// \<identifier\> ::= \[a-zA-Z_\]\[a-zA-Z0-9_\]*
    Identifier(Box<str>),
    /// Holds an i64
    /// # Rule
    /// \<integer\> ::= \[0-9\]+
    /// Integer and Double hold a string.
    Integer(u64),
    /// Holds a f64
    /// # Rule
    /// \<double\> ::= \[0-9\]* "." \[0-9\]+
    /// Integer and Double hold a string.
    Float(f64),
    /// Literal string, Unicode.
    /// # Rule
    /// \<string\> ::= """ \<char\>* """
    String(Box<str>),
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
    /// Literal symbol "&"
    Ampersand,
    /// Literal symbol "%"
    Percent,
    /// Literal symbol "^"
    Hat,
    /// Literal symbol "|"
    Beam,
    /// Literal symbol "\"
    Backslash,
    /// Literal symbol "("
    LParen,
    /// Literal symbol ")"
    RParen,
    /// Literal symbol "{"
    LBrace,
    /// Literal symbol "}"
    RBrace,
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
    Less,
    /// Literal symbol ">"
    Greater,
    // multiple-character symbols, <symbol>
    /// Literal symbol "!="
    BangEqual,
    /// Literal symbol "<="
    LPBraceEqual,
    /// Literal symbol ">="
    RPBraceEqual,
    /// Literal symbol "=="
    EqualEqual,
    /// Literal symbol "&&"
    AmpersandAmpersand,
    /// Literal symbol "||"
    BeamBeam,
    /// Literal symbol "//"
    SlashSlash,
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
    /// Keyword "while"
    While,
    /// Keyword "def"
    Def,
    /// Keyword "mut"
    Mut,
    /// Keyword "proc"
    Proc,
    /// Keyword "return"
    Return,
    /// Keyword "block_return"
    BlockReturn,
}

/// If the `key` matches a keyword, return the `TokenKind` corresponding to
/// that keyword.
/// Otherwise, return a `TokenKind::Identifier`.
///
/// Yes, that means this function may allocate some memory.
#[must_use]
pub fn keyword_lookup(key: &str) -> TokenKind {
    static LOOKUP_TBL: LazyLock<HashMap<&'static str, TokenKind>> =
        LazyLock::new(|| {
            HashMap::<&'static str, TokenKind>::from([
                ("ya", TokenKind::Ya),
                ("na", TokenKind::Na),
                ("if", TokenKind::If),
                ("elif", TokenKind::Elif),
                ("else", TokenKind::Else),
                ("let", TokenKind::Let),
                ("while", TokenKind::While),
                ("def", TokenKind::Def),
                ("mut", TokenKind::Mut),
                ("proc", TokenKind::Proc),
                ("return", TokenKind::Return),
                ("block_return", TokenKind::BlockReturn),
            ])
        });

    // none of the enum in here contains anything inside, so Imma just clone
    // them.
    LOOKUP_TBL
        .get(key)
        .cloned()
        .unwrap_or(TokenKind::Identifier(Box::from(key)))
}

/// NOTE: tab is considered one column.
///
/// TODO: do we allow configurable tab width?
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct CharPosition {
    pub line: usize,
    pub column: usize,
}

/// A token. Duh.
/// A tokenizer reads an input string and spits out smaller chunks, which are
/// these.
///
/// * `token_type`: The type of token.
/// * `line_number`: The line number.
/// * `line_position`: The position in that line.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: CharPosition,
}

impl Token {
    /// Constructs a new Token
    #[inline]
    #[must_use]
    pub fn new(kind: TokenKind, line: usize, column: usize) -> Self {
        Self {
            kind,
            pos: CharPosition { line, column },
        }
    }
}
