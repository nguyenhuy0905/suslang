use std::collections::HashMap;
use std::sync::LazyLock;

/// All the possible tokens.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[repr(u8)]
pub enum TokenKind {
    /// Just an identifier. ASCII.
    /// # Rule
    /// \<identifier\> ::= \[a-zA-Z_\]\[a-zA-Z0-9_\]*
    Identifier,
    /// Holds an i64
    /// # Rule
    /// \<integer\> ::= \[0-9\]+
    /// Integer and Double hold a string.
    Integer,
    /// Holds a f64
    /// # Rule
    /// \<double\> ::= \[0-9\]* "." \[0-9\]+
    /// Integer and Double hold a string.
    Float,
    /// Literal string, Unicode.
    /// # Rule
    /// \<string\> ::= """ \<char\>* """
    String,
    /// single character, one or more Unicode code points.
    /// Rule:
    /// \<char\> ::= \<unicode-code-points\>
    /// \<unicode-code-point\> ::= \
    /// \[0b10000000-0b11111111\]{0, 3}\[0b00000000-0b01111111\]
    Char,
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
    /// Literal symbol "<"
    Less,
    /// Literal symbol ">"
    Greater,
    // hash is never pushed into the token list, but it's a symbol regardless.
    // Hash,
    // multiple-character symbols, <symbol>
    /// Literal symbol "<="
    LessEqual,
    /// Literal symbol ">="
    GreaterEqual,
    // keywords
    /// Keyword "mod"
    Mod,
    /// Keyword "ya"
    Ya,
    /// Keyword "na"
    Na,
    /// Keyword "and"
    And,
    /// Keyword "or"
    Or,
    /// Keyword "not"
    Not,
    /// Keyword "eq"
    Eq,
    /// Keyword "neq"
    Neq,
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
    #[allow(clippy::doc_markdown)]
    /// Keyword "block_return"
    BlockReturn,
    /// Keyword "struct"
    Struct,
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
                ("mod", TokenKind::Mod),
                ("ya", TokenKind::Ya),
                ("na", TokenKind::Na),
                ("and", TokenKind::And),
                ("or", TokenKind::Or),
                ("not", TokenKind::Not),
                ("eq", TokenKind::Eq),
                ("neq", TokenKind::Neq),
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
                ("struct", TokenKind::Struct),
            ])
        });

    // none of the enum in here contains anything inside, so Imma just clone
    // them.
    LOOKUP_TBL
        .get(key)
        .copied()
        .unwrap_or(TokenKind::Identifier)
}

/// NOTE: tab is considered one column.
///
/// TODO: do we allow configurable tab width?
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: CharPosition,
    pub repr: Option<Box<str>>,
}
