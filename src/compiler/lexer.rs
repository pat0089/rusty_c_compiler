use std::{collections::{HashSet, VecDeque}, fmt };
use regex::Regex;

#[derive(Debug, Clone, PartialEq)]
pub enum IdentifierType {
    Int,
    Illegal,
}


#[derive(Debug, Clone, PartialEq)]
pub enum KeywordType {
    Return,
    Illegal,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Illegal(LexerError),
    EOF,
    Identifier(String),
    IdentifierType(IdentifierType),
    Equals,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    LChevron,
    RChevron,
    IntegerLiteral(i32),
    Keyword(KeywordType),
    /*
    I need to be able to compile this:

    int main() {
        return 0;
    }
    
    */
}

impl TokenType {
    fn from_str(to_string: &str) -> TokenType {
        match to_string {
            "=" => TokenType::Equals,
            ";" => TokenType::Semicolon,
            "(" => TokenType::LParen,
            ")" => TokenType::RParen,
            "{" => TokenType::LBrace,
            "}" => TokenType::RBrace,
            "[" => TokenType::LBracket,
            "]" => TokenType::RBracket,
            "<" => TokenType::LChevron,
            ">" => TokenType::RChevron,
            "int" => TokenType::IdentifierType(IdentifierType::Int),
            "return" => TokenType::Keyword(KeywordType::Return),
            _ => TokenType::Illegal(LexerError::new(format!("Invalid token: {}", to_string))),
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
    pub line: i32,
    pub start: i32,
    pub end: i32,
}

impl Token {
    pub fn new(token_type: TokenType, literal: String, line: i32, start: i32, end: i32) -> Token {
        Token {
            token_type,
            literal,
            line,
            start,
            end,
        }
    }

    pub fn get_type(&self) -> TokenType {
        self.token_type.clone()
    }

    pub fn get_literal(&self) -> &str {
        &self.literal
    }

    pub fn get_line(&self) -> i32 {
        self.line
    }

    pub fn get_start(&self) -> i32 {
        self.start
    }

    pub fn get_end(&self) -> i32 {
        self.end
    }

}

pub trait TokenStream {
    fn next_token(&mut self) -> Option<Result<Token, LexerError>>;
    fn peek_token(&mut self) -> Option<Result<&Token, LexerError>>;
    fn peek_tokens(&mut self, n: usize) -> Vec<Result<Token, LexerError>>;
    fn putback_token(&mut self, token: Token) -> Result<(), LexerError>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct LexerError {
    message: String,
}

impl LexerError {
    pub fn new(message: String) -> LexerError {
        LexerError { message }
    }
}

impl std::error::Error for LexerError {
    fn description(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Lexer error: {}", self.message)
    }
}

#[derive(Debug)]
pub struct Lexer {
    buffer: VecDeque<Token>,
}

impl Lexer {
    pub fn new(input: &String) -> Lexer {
        let mut l = Lexer {
            buffer: VecDeque::new(),
        };
        //loop through the input string
        //for each character
        //determine if the character is part of a token or a token itself
        //if the character is part of a token
        //add it to the string for the token
        //if the character is not part of a token
        //add the token to the buffer
        //and reset the string for the next token
        //if the character is whitespace, 
        //depending on the state of the lexer, 
        //add the string for the token to the buffer
        //or reset the string for the next token
        let mut tokens: Vec<Token> = Vec::new();
        let mut current_token = String::new();
        let valid_identifier_regex = Regex::new(r"[_a-zA-Z][_a-zA-Z0-9]{0,30}").unwrap();
        let valid_number_regex = Regex::new(r"^[0-9]+$").unwrap();
        let keywords = HashSet::from(["int".to_owned(), "return".to_owned()]);
        let symbols = HashSet::from([';', '(', ')', '{', '}']);
        let final_line_num = input.lines().count();
        let mut final_char_num = 0;
        //loop through the input on each line
        for (line_num, line) in input.lines().enumerate() {
            //loop through the line by character
            for (char_num, c) in line.chars().enumerate() {
                //if the character is whitespace and the current token is not empty, 
                //add current_token to the buffer
                //then reset the string for the next token
                if c.is_whitespace() || symbols.contains(&c) {
                    if !current_token.is_empty() {
                        let token_type : TokenType;
                        if keywords.contains(&current_token) {
                            token_type = TokenType::from_str(&current_token);
                        } else if valid_identifier_regex.is_match(&current_token) {
                            token_type = TokenType::Identifier(current_token.clone());
                        } else if valid_number_regex.is_match(&current_token) {
                            token_type = TokenType::IntegerLiteral(current_token.parse().unwrap());
                        } else {
                            token_type = TokenType::Illegal(LexerError::new(format!("Invalid token: {}", current_token)));
                        };
                        tokens.push(
                            Token::new(
                                token_type,
                                current_token.clone(),
                                line_num as i32 + 1, 
                                (char_num - current_token.len()) as i32, 
                                char_num as i32
                            ));
                        current_token.clear();
                    }
                    if symbols.contains(&c) {
                        let symbol_token_type = TokenType::from_str(&c.to_string());
                        tokens.push(
                            Token::new(
                                symbol_token_type,
                                c.to_string(),
                                line_num as i32 + 1,
                                char_num as i32,
                                char_num as i32 + 1,
                            ),
                        );
                    }
                } else {
                    //handle other cases here
                    current_token.push(c);
                }
                final_char_num = char_num;
            }
        }

        //add the last token to the buffer
        if !current_token.is_empty() {
            tokens.push(
                Token::new(
                    Lexer::validate_identifier(&current_token, &valid_identifier_regex),
                    current_token.clone(),
                    final_line_num as i32 + 1,
                    (final_char_num - current_token.len()) as i32,
                    final_char_num as i32
                )
            );
        }

        //add the EOF token
        tokens.push(
            Token::new(
                TokenType::EOF, 
                String::new(), 
                final_line_num as i32 + 1, 
                final_char_num as i32, 
                final_char_num as i32
            ));

        l.buffer = tokens.into();
        l
    }

    fn validate_identifier(identifier: &str, regex: &Regex) -> TokenType {
        if regex.is_match(identifier) {
            TokenType::Identifier(identifier.to_string())
        } else {
            TokenType::Illegal(LexerError::new(format!("Invalid identifier: {}", identifier)))
        }
    }

}

impl TokenStream for Lexer {
    fn next_token(&mut self) -> Option<Result<Token, LexerError>> {
        if self.buffer.front().is_some() {
            Some(Ok(self.buffer.pop_front().unwrap()))
        } else {
            None
        }
    }
    fn peek_token(&mut self) -> Option<Result<&Token, LexerError>> {
        if self.buffer.front().is_some() {
            Some(Ok(self.buffer.front()?))
        } else {
            None
        }
    }
    fn peek_tokens(&mut self, n: usize) -> Vec<Result<Token, LexerError>> {
        Vec::new()
    }
    fn putback_token(&mut self, token: Token) -> Result<(), LexerError> {
        Ok(())
    }
}