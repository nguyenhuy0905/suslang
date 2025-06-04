use super::Expr;
use crate::{ExprAst, ExprBoxWrap, ExprParse, ParseError};
use std::collections::VecDeque;
use tokenize::{Token, TokenType};

// TODO: add array and tuple.

/// Primary data types. May not be primary any more in the future.
///
/// # Rule
/// \<prim-expr\> ::= NUMBER | FLOAT | STRING | "ya" | "na"
#[derive(Debug, PartialEq, Clone)]
pub enum PrimaryExpr {
    Integer(u64),
    Float(f64),
    String(String),
    Boolean(bool),
    Identifier(String),
}

impl ExprAst for PrimaryExpr {}

impl ExprParse for PrimaryExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        _line: usize,
        _pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        if let Some((typ, line, pos)) = tokens.pop_front().map(Token::bind) {
            match typ {
                TokenType::Integer(s_in) => {
                    let parsed_int = s_in.parse::<u64>();
                    debug_assert!(parsed_int.is_ok());
                    Ok((
                        ExprBoxWrap::new(PrimaryExpr::Integer(
                            parsed_int.unwrap(),
                        )),
                        line,
                        pos,
                    ))
                }
                TokenType::Double(s_dbl) => {
                    let parsed_dbl = s_dbl.parse::<f64>();
                    debug_assert!(parsed_dbl.is_ok());
                    Ok((
                        ExprBoxWrap::new(PrimaryExpr::Float(
                            parsed_dbl.unwrap(),
                        )),
                        line,
                        pos,
                    ))
                }
                TokenType::String(s) => {
                    Ok((ExprBoxWrap::new(PrimaryExpr::String(s)), line, pos))
                }
                TokenType::Identifier(s) => {
                    // if the next token is a left-paren, it should be a
                    // proc-call.
                    if tokens.front().map(Token::token_type)
                        == Some(&TokenType::LParen)
                    {
                        ProcCallExpr::parse_with_name(s, tokens, line, pos).map(
                            |(retval, ret_ln, ret_pos)| {
                                (ExprBoxWrap::new(retval), ret_ln, ret_pos)
                            },
                        )
                    } else {
                        Ok((
                            ExprBoxWrap::new(PrimaryExpr::Identifier(s)),
                            line,
                            pos,
                        ))
                    }
                }
                TokenType::Ya => Ok((
                    ExprBoxWrap::new(PrimaryExpr::Boolean(true)),
                    line,
                    pos,
                )),
                TokenType::Na => Ok((
                    ExprBoxWrap::new(PrimaryExpr::Boolean(false)),
                    line,
                    pos,
                )),
                TokenType::LParen => {
                    let ret = Expr::parse(tokens, line, pos)?;
                    if let Some((&TokenType::RParen, rp_line, rp_pos)) =
                        tokens.front().map(Token::bind_ref)
                    {
                        tokens.pop_front();
                        Ok((ret.0, rp_line, rp_pos))
                    } else if tokens.is_empty() {
                        Err(Some(ParseError::UnclosedParen { line, pos }))
                    } else {
                        Err(Some(ParseError::UnexpectedToken(
                            tokens.pop_front().unwrap(),
                        )))
                    }
                }
                _ => Err(Some(ParseError::UnexpectedToken(Token::new(
                    typ, line, pos,
                )))),
            }
        } else {
            Err(None)
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProcCallExpr {
    pub ident: String,
    pub params: Vec<ExprBoxWrap>,
}

#[macro_export]
macro_rules! new_proc_call_expr {
    ($name:expr, ($($first_param:expr $(, $follow_param:expr)* $(,)?)?)) => {
        ProcCallExpr {
            ident: String::from($name),
            params: vec![$(ExprBoxWrap::new($first_param) $(,ExprBoxWrap::new($follow_param))*)?],
        }
    };
}

impl ProcCallExpr {
    /// Parse into a `ProcCallExpr`, given the name.
    ///
    /// The design is like so because when parsing an identifier, it's only a
    /// procedure call if that identifier is immediately followed by a left-paren,
    /// then a list of expressions that is the parameters, and finally a closing
    /// paren.
    ///
    /// # Errors
    /// - If at any point, there's no token left in `tokens` but there should be
    ///   one, return `Some(`[`ParseError::ExpectedToken`]`)`.
    /// - If the first token isn't a [`TokenType::LParen`], return
    ///   `Some(`[`ParseError::UnexpectedToken`]`)`.
    /// - If the token following that left-paren isn't a
    ///   [`TokenType::Identifier`] or [`TokenType::RParen`], return
    ///   `Some(`[`ParseError::UnexpectedToken`]`)`.
    /// - A [`TokenType::RParen`] is expected as the very last token of this
    ///   expression.
    #[allow(clippy::missing_panics_doc)]
    pub fn parse_with_name(
        name: String,
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(Self, usize, usize), Option<ParseError>> {
        let (lp_line, lp_pos) = tokens
            .pop_front()
            .ok_or(Some(ParseError::ExpectedToken { line, pos }))
            .map(|tok| (tok.line_number, tok.line_position))?;
        let (mut ret_ln, mut ret_pos) = (lp_line, lp_pos);
        let mut params: Vec<ExprBoxWrap> = Vec::new();
        if !matches!(
            tokens.front().map(Token::token_type),
            Some(&TokenType::RParen)
        ) {
            loop {
                let (expr, expr_ln, expr_pos) = Expr::parse(
                    tokens, ret_ln, ret_pos,
                )
                .map_err(|e| match e {
                    Some(err) => err,
                    None => ParseError::ExpectedToken {
                        line: ret_ln,
                        pos: ret_pos,
                    },
                })?;
                params.push(expr);
                // check for comma
                // see if there's a comma following the identifier.
                // There should be a next token or it's an `ExpectedToken`.
                // If yes, remove it so that the next iteration doesn't return
                // error because of the comma. (UnexpectedToken)
                // If it's a RParen instead, done with the loop.
                // Otherwise, it's an `UnexpectedToken`.
                match tokens.front().map(Token::bind_ref) {
                    Some((&TokenType::Comma, comma_ln, comma_pos)) => {
                        tokens.pop_front();
                        (ret_ln, ret_pos) = (comma_ln, comma_pos);
                    }
                    Some((&TokenType::RParen, rp_ln, rp_pos)) => {
                        (ret_ln, ret_pos) = (rp_ln, rp_pos);
                        break;
                    }
                    Some(_) => {
                        return Err(Some(ParseError::UnexpectedToken(
                            tokens.pop_front().unwrap(),
                        )));
                    }
                    None => {
                        return Err(Some(ParseError::ExpectedToken {
                            line: expr_ln,
                            pos: expr_pos,
                        }));
                    }
                }
            }
        }
        // remove the right-paren
        let (rp_line, rp_pos) = tokens
            .pop_front()
            .ok_or(Some(ParseError::ExpectedToken {
                line: ret_ln,
                pos: ret_pos,
            }))
            .and_then(|tok| match tok.tok_typ {
                TokenType::RParen => Ok((tok.line_number, tok.line_position)),
                _ => Err(Some(ParseError::UnexpectedToken(tok))),
            })?;
        (ret_ln, ret_pos) = (rp_line, rp_pos);
        Ok((
            Self {
                ident: name,
                params,
            },
            ret_ln,
            ret_pos,
        ))
    }
}

impl ExprAst for ProcCallExpr {}

/// Unary expressions
///
/// # Rule
/// \<unary-expr\> ::= ("+" | "-" | "*" | "&" | "!") \<prim-expr\>
///
/// # See also
/// [`PrimaryExpr`]
#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr {
    pub primary: ExprBoxWrap,
    pub op: UnaryOp,
}

#[macro_export]
macro_rules! new_unary_expr {
    ($primary:expr, $op:expr) => {
        UnaryExpr {
            primary: ExprBoxWrap::new($primary),
            op: $op,
        }
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Plus,   // +
    Minus,  // -
    Deref,  // *
    Ref,    // &
    Negate, // !
}

impl ExprAst for UnaryExpr {}

impl ExprParse for UnaryExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        _line: usize,
        _pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        if let Some((typ, line, pos)) = tokens.pop_front().map(Token::bind) {
            let Some(op) = (match typ {
                TokenType::Plus => Some(UnaryOp::Plus),
                TokenType::Dash => Some(UnaryOp::Minus),
                TokenType::Star => Some(UnaryOp::Deref),
                TokenType::Ampersand => Some(UnaryOp::Ref),
                TokenType::Bang => Some(UnaryOp::Negate),
                _ => None,
            }) else {
                tokens.push_front(Token::new(typ, line, pos));
                return PrimaryExpr::parse(tokens, line, pos);
            };

            let (primary, prim_ln, prim_pos) =
                PrimaryExpr::parse(tokens, line, pos).map_err(|e| {
                    if e.is_none() {
                        Some(ParseError::ExpectedToken { line, pos })
                    } else {
                        e
                    }
                })?;
            Ok((
                ExprBoxWrap::new(UnaryExpr { primary, op }),
                prim_ln,
                prim_pos,
            ))
        } else {
            Err(None)
        }
    }
}

/// Factor expression, that is, multiply or divide.
///
/// # Rule
/// \<fac-expr\> ::= \<unary-expr\> ("*" | "/" \<unary-expr\>)*
///
/// # See also
/// [`UnaryExpr`]
#[derive(Debug, PartialEq, Clone)]
pub struct FactorExpr {
    pub first_fac: ExprBoxWrap,
    pub follow_facs: Vec<(FactorOp, ExprBoxWrap)>,
}

/// Convenience macro to construct a new `FactorExpr`
#[macro_export]
macro_rules! new_factor_expr {
    ($first_fac:expr$(,$follow_fac_op:expr,$follow_fac_exp:expr)+$(,)?) => {
        FactorExpr {
            first_fac: ExprBoxWrap::new($first_fac),
            follow_facs: vec![$(($follow_fac_op,ExprBoxWrap::new($follow_fac_exp)),)+],
        }
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum FactorOp {
    Multiply, // *
    Divide,   // /
    Mod,      // %
}

impl ExprAst for FactorExpr {}

impl ExprParse for FactorExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        let (first_fac, fst_line, fst_pos) =
            UnaryExpr::parse(tokens, line, pos)?;
        // if I don't wrap this inside a closure, the return statement inside
        // that else, returns through the entire function.
        let (follow_facs, fl_line, fl_pos) =
            || -> Result<(Vec<_>, usize, usize), Option<ParseError>> {
                let mut ret = Vec::new();
                let (mut fin_line, mut fin_pos) = (fst_line, fst_pos);
                while let Some((typ, front_ln, front_pos)) =
                    tokens.front().map(Token::bind_ref)
                {
                    let Some(fac_op) = (match typ {
                        TokenType::Star => Some(FactorOp::Multiply),
                        TokenType::Slash => Some(FactorOp::Divide),
                        TokenType::Percent => Some(FactorOp::Mod),
                        _ => None,
                    }) else {
                        return Ok((ret, front_ln, front_pos));
                    };
                    tokens.pop_front();
                    let (next_fac, fac_line, fac_pos) =
                        UnaryExpr::parse(tokens, line, pos).map_err(|e| {
                            if e.is_none() {
                                Some(ParseError::ExpectedToken {
                                    line: front_ln,
                                    pos: front_pos,
                                })
                            } else {
                                e
                            }
                        })?;
                    (fin_line, fin_pos) = (fac_line, fac_pos);
                    ret.push((fac_op, next_fac));
                }
                Ok((ret, fin_line, fin_pos))
            }()?;
        if follow_facs.is_empty() {
            Ok((first_fac, fl_line, fl_pos))
        } else {
            Ok((
                ExprBoxWrap::new(FactorExpr {
                    first_fac,
                    follow_facs,
                }),
                fl_line,
                fl_pos,
            ))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TermExpr {
    pub first_term: ExprBoxWrap,
    pub follow_terms: Vec<(TermOp, ExprBoxWrap)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TermOp {
    Plus,
    Minus,
}

impl ExprAst for TermExpr {}

#[macro_export]
macro_rules! new_term_expr {
    ($first_term:expr$(,$follow_op:expr,$follow_term:expr)+$(,)?) => {
        TermExpr {
            first_term: ExprBoxWrap::new($first_term),
            follow_terms: vec![$(($follow_op, ExprBoxWrap::new($follow_term)),)+]
        }
    };
}

impl ExprParse for TermExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        let (first_term, fac_ln, fac_pos) =
            FactorExpr::parse(tokens, line, pos)?;
        let (follow_terms, follow_ln, follow_pos) =
            || -> Result<(Vec<_>, usize, usize), Option<ParseError>> {
                let mut ret = Vec::new();
                let (mut fin_ln, mut fin_pos) = (fac_ln, fac_pos);
                while let Some((typ, front_ln, front_pos)) =
                    tokens.front().map(Token::bind_ref)
                {
                    let Some(term_op) = (match typ {
                        TokenType::Plus => Some(TermOp::Plus),
                        TokenType::Dash => Some(TermOp::Minus),
                        _ => None,
                    }) else {
                        return Ok((ret, front_ln, front_pos));
                    };
                    tokens.pop_front();
                    let (next_term, next_ln, next_pos) =
                        FactorExpr::parse(tokens, line, pos).map_err(|e| {
                            if e.is_none() {
                                Some(ParseError::ExpectedToken {
                                    line: front_ln,
                                    pos: front_pos,
                                })
                            } else {
                                e
                            }
                        })?;
                    (fin_ln, fin_pos) = (next_ln, next_pos);
                    ret.push((term_op, next_term));
                }
                Ok((ret, fin_ln, fin_pos))
            }()?;
        if follow_terms.is_empty() {
            Ok((first_term, follow_ln, follow_pos))
        } else {
            Ok((
                ExprBoxWrap::new(TermExpr {
                    first_term,
                    follow_terms,
                }),
                follow_ln,
                follow_pos,
            ))
        }
    }
}

/// Bitwise and
///
/// # Rule
/// \<bit-and-expr\> ::= \<term-expr\> ("&" \<term-expr\>)*
///
/// # See also
/// [`TermExpr`]
#[derive(Debug, PartialEq, Clone)]
pub struct BitAndExpr {
    pub first_bit_and: ExprBoxWrap,
    pub follow_bit_ands: Vec<ExprBoxWrap>,
}

#[macro_export]
macro_rules! new_bit_and_expr {
    ($first_bit_and:expr$(,$bit_and:expr)+$(,)?) => {
        BitAndExpr {
            first_bit_and: ExprBoxWrap::new($first_bit_and),
            follow_bit_ands: vec![$(ExprBoxWrap::new($bit_and))+],
        }
    }
}

impl ExprAst for BitAndExpr {}

impl ExprParse for BitAndExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        let (first_bit_and, bit_ln, bit_pos) =
            TermExpr::parse(tokens, line, pos)?;
        let (follow_bit_ands, follow_ln, follow_pos) =
            || -> Result<(Vec<_>, usize, usize), Option<ParseError>> {
                let mut ret = Vec::new();
                let (mut ret_ln, mut ret_pos) = (bit_ln, bit_pos);
                while let Some((&TokenType::Ampersand, amp_ln, amp_pos)) =
                    tokens.front().map(Token::bind_ref)
                {
                    tokens.pop_front();
                    let (term_expr, term_ln, term_pos) = TermExpr::parse(
                        tokens, ret_ln, ret_pos,
                    )
                    .map_err(|e| {
                        if e.is_none() {
                            Some(ParseError::ExpectedToken {
                                line: amp_ln,
                                pos: amp_pos,
                            })
                        } else {
                            e
                        }
                    })?;
                    ret.push(term_expr);
                    (ret_ln, ret_pos) = (term_ln, term_pos);
                }
                Ok((ret, ret_ln, ret_pos))
            }()?;
        if follow_bit_ands.is_empty() {
            Ok((first_bit_and, follow_ln, follow_pos))
        } else {
            Ok((
                ExprBoxWrap::new(Self {
                    first_bit_and,
                    follow_bit_ands,
                }),
                follow_ln,
                follow_pos,
            ))
        }
    }
}

/// Bitwise xor
///
/// # Rule
/// \<bit-xor-expr\> ::= \<bit-and-expr\> ("&" \<bit-and-expr\>)*
///
/// # See also
/// [`BitAndExpr`]
#[derive(Debug, Clone, PartialEq)]
pub struct BitXorExpr {
    pub first_bit_xor: ExprBoxWrap,
    pub follow_bit_xors: Vec<ExprBoxWrap>,
}

impl ExprAst for BitXorExpr {}

#[macro_export]
macro_rules! new_bit_xor_expr {
    ($first_bit_xor:expr$(,$bit_xor:expr)+$(,)?) => {
        BitXorExpr {
            first_bit_xor: ExprBoxWrap::new($first_bit_xor),
            follow_bit_xors: vec![$(ExprBoxWrap::new($bit_xor))+],
        }
    }
}

impl ExprParse for BitXorExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        let (first_bit_xor, first_ln, first_pos) =
            BitAndExpr::parse(tokens, line, pos)?;
        let (follow_bit_xors, follow_ln, follow_pos) =
            || -> Result<(Vec<_>, usize, usize), Option<ParseError>> {
                let mut ret = Vec::new();
                let (mut ret_ln, mut ret_pos) = (first_ln, first_pos);
                while let Some((&TokenType::Hat, line, pos)) =
                    tokens.front().map(Token::bind_ref)
                {
                    tokens.pop_front();
                    let (bit_and_expr, band_line, band_pos) =
                        BitAndExpr::parse(tokens, ret_ln, ret_pos).map_err(
                            |e| {
                                if e.is_none() {
                                    Some(ParseError::ExpectedToken {
                                        line,
                                        pos,
                                    })
                                } else {
                                    e
                                }
                            },
                        )?;
                    (ret_ln, ret_pos) = (band_line, band_pos);
                    ret.push(bit_and_expr);
                }
                Ok((ret, ret_ln, ret_pos))
            }()?;
        if follow_bit_xors.is_empty() {
            Ok((first_bit_xor, follow_ln, follow_pos))
        } else {
            Ok((
                ExprBoxWrap::new(Self {
                    first_bit_xor,
                    follow_bit_xors,
                }),
                follow_ln,
                follow_pos,
            ))
        }
    }
}

/// Bitwise or
///
/// # Rule
/// \<bit-or-expr\> ::= \<bit-xor-expr\> ("&" \<bit-xor-expr\>)*
///
/// # See also
/// [`BitXorExpr`]
#[derive(Debug, Clone, PartialEq)]
pub struct BitOrExpr {
    pub first_bit_or: ExprBoxWrap,
    pub follow_bit_ors: Vec<ExprBoxWrap>,
}

impl ExprAst for BitOrExpr {}

#[macro_export]
macro_rules! new_bit_or_expr {
    ($first_bit_or:expr $(,$bit_or:expr)+$(,)?) => {
        BitOrExpr {
            first_bit_or: ExprBoxWrap::new($first_bit_or),
            follow_bit_ors: vec![$(ExprBoxWrap::new($bit_or),)*],
        }
    }
}

impl ExprParse for BitOrExpr {
    fn parse(
        tokens: &mut VecDeque<Token>,
        line: usize,
        pos: usize,
    ) -> Result<(ExprBoxWrap, usize, usize), Option<ParseError>> {
        let (first_bit_or, first_ln, first_pos) =
            BitXorExpr::parse(tokens, line, pos)?;
        let (follow_bit_ors, follow_ln, follow_pos) =
            || -> Result<(Vec<_>, usize, usize), Option<ParseError>> {
                let mut ret = Vec::new();
                let (mut ret_ln, mut ret_pos) = (first_ln, first_pos);
                while let Some((&TokenType::Beam, line, pos)) =
                    tokens.front().map(Token::bind_ref)
                {
                    tokens.pop_front();
                    let (bit_xor, bxor_ln, bxor_pos) = BitXorExpr::parse(
                        tokens, ret_ln, ret_pos,
                    )
                    .map_err(|e| {
                        if e.is_none() {
                            Some(ParseError::ExpectedToken { line, pos })
                        } else {
                            e
                        }
                    })?;
                    (ret_ln, ret_pos) = (bxor_ln, bxor_pos);
                    ret.push(bit_xor);
                }
                Ok((ret, ret_ln, ret_pos))
            }()?;
        if follow_bit_ors.is_empty() {
            Ok((first_bit_or, follow_ln, follow_pos))
        } else {
            Ok((
                ExprBoxWrap::new(Self {
                    first_bit_or,
                    follow_bit_ors,
                }),
                follow_ln,
                follow_pos,
            ))
        }
    }
}
