#[derive(Debug, Eq, PartialEq)]
pub enum TokenType {
    Illegal,
    Eof,

    // 識別子 + リテラル
    Ident, // add, foobar, x, y, ...
    Int,   // 12345

    // 演算子
    Assign,   // =
    Plus,     // +
    Minus,    // -
    Bang,     // !
    Asterisk, // *
    Slash,    // /

    Lt,    // <
    Gt,    // >
    Eq,    // ==
    NotEq, // !=

    // デリミタ
    Comma,     // ,
    Semicolon, // ;

    LParen, // (
    RParen, // )
    LBrace, // {
    RBrace, // }

    // キーワード
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,

    String,

    LBracket, // [
    RBracket, // ]

    Colon, // :

    Macro, // macro
}

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    pub t: TokenType,
    pub literal: String,
}

pub fn lookup_ident(literal: &str) -> TokenType {
    match literal {
        "fn" => TokenType::Function,
        "let" => TokenType::Let,
        "true" => TokenType::True,
        "false" => TokenType::False,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        "macro" => TokenType::Macro,
        _ => TokenType::Ident,
    }
}
