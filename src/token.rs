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

    Lparen, // (
    Rparen, // )
    Lbrace, // {
    Rbrace, // }

    // キーワード
    Function,
    Let
}

pub struct Token {
    pub t: TokenType,
    pub literal: String,
}

