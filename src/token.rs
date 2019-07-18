#[derive(Debug,Eq,PartialEq)]
pub enum TokenType {
    Illegal,
    Eof,

    // 識別子 + リテラル
    Ident, // add, foobar, x, y, ...
    Int, // 12345

    // 演算子
    Assign, // =
    Plus, // +

    // デリミタ
    Comma, // ,
    Semicolon, // ;

    LParen, // (
    RParen, // )
    LBrace, // {
    RBrace, // }

    // キーワード
    Function,
    Let
}

pub struct Token {
    pub t: TokenType,
    pub literal: String,
}

pub fn lookup_ident(literal: &str) -> TokenType {
    match literal {
        "fn" => TokenType::Function,
        "let" => TokenType::Let,
        _ => TokenType::Ident
    }
}
