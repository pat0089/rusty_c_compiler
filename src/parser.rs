use crate::lexer::{TokenStream, TokenType, Token};

pub enum Statement {
    Return(Expression),
}

pub enum Expression {
    Integer(i32),
}

//for now, only read through the tokens
pub struct Parser<T: TokenStream> {
    token_stream: T,
    //AST goes here
}

impl<T: TokenStream> Parser<T> {
    pub fn new(token_stream: T) -> Parser<T> {
        Parser {
            token_stream,
        }
    }

    pub fn parse(&mut self) -> Result<(), String> {
        while let Some(token) = self.token_stream.next_token() {
            match token {
                Ok(token) => {
                    match token.get_type() {
                        TokenType::Illegal(e) => {
                            eprintln!("{:?}, ({}:{}-{})", e, token.get_line(), token.get_start(), token.get_end());
                        }
                        _ => {
                            println!("{:?}", token)
                        }
                    }
                },
                _ => {
                    println!("{:?}", token)
                },
            }
        }
        Ok(())
    }
}