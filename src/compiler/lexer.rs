use std::{collections::{HashMap, HashSet, VecDeque}, fmt::{self, Display} };
use regex::Regex;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IdentifierType {
    Int,
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum KeywordType {
    Return,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

impl Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
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
    fn peek_tokens(&mut self, n: usize) -> Vec<Result<&Token, LexerError>>;
    fn putback_token(&mut self, token: Token) -> Result<(), LexerError>;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    //Map from open bracket to close bracket
    open_closed_map: HashMap<TokenType, TokenType>,
    open_brackets: HashSet<TokenType>,
    close_brackets: HashSet<TokenType>,
    bracket_stack: Vec<(TokenType, String, i32, i32, i32)>,
}

impl Lexer {
    pub fn new(input: &String) -> Lexer {
        let open_brackets = HashSet::from([
            TokenType::LParen, 
            TokenType::LBrace,
            TokenType::LBracket,
            TokenType::LChevron,
        ]);

        let close_brackets = HashSet::from([
            TokenType::RParen,
            TokenType::RBrace,
            TokenType::RBracket,
            TokenType::RChevron,
        ]);

        let open_closed_map = HashMap::from([
            (TokenType::RParen, TokenType::LParen),
            (TokenType::RBrace, TokenType::LBrace),
            (TokenType::RBracket, TokenType::LBracket),
            (TokenType::RChevron, TokenType::LChevron),
        ]);

        let mut l = Lexer {
            buffer: VecDeque::new(),
            open_brackets,
            close_brackets,
            open_closed_map,
            bracket_stack: Vec::new(),
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
        let symbols = HashSet::from([';', '(', ')', '{', '}', '[', ']', '<', '>', '=', '+', '-', '*', '/']);
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

    pub fn validate(&mut self) -> Result<(), LexerError> {
        //check for illegal tokens
        for token in &self.buffer {
            if let TokenType::Illegal(e) = token.get_type() {
                return Err(e);
            }
        }

        self.brackets_check()?;

        Ok(())
    }

    fn brackets_check(&mut self) -> Result<(), LexerError> {
        for token in &self.buffer {
            match (self.open_brackets.contains(&token.get_type()), self.close_brackets.contains(&token.get_type())) {
                (true, false) => {
                    self.bracket_stack.push((token.get_type(), token.get_literal().to_string(), token.get_line(), token.get_start(), token.get_end()));
                }
                (false, true) => {
                    if self.bracket_stack.is_empty() {
                        return Err(LexerError::new(format!("Unmatched close bracket: '{}' ({}:{}-{})", token.get_literal(), token.get_line(), token.get_start(), token.get_end())));
                    }
                    if self.bracket_stack.last().unwrap().0 != *self.open_closed_map.get(&token.get_type()).unwrap() {
                        return Err(LexerError::new(format!("Unmatched open bracket: '{}' ({}:{}-{})", token.get_literal(), token.get_line(), token.get_start(), token.get_end())));
                    }
                    self.bracket_stack.pop();
                }
                _ => {}
            }
        }
        if !self.bracket_stack.is_empty() {
            let last = self.bracket_stack.last().unwrap();
            return Err(LexerError::new(format!("Unmatched open bracket: '{}' ({}:{}-{})", last.1, last.2, last.3, last.4)));
        }
        Ok(())
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
    fn peek_tokens(&mut self, n: usize) -> Vec<Result<&Token, LexerError>> {
        let mut tokens = Vec::new();
        for i in 0..n {
            match self.buffer.get(i) {
                Some(token) => {
                    match token.get_type() {
                        TokenType::Illegal(e) => tokens.push(Err(e)),
                        _ => tokens.push(Ok(token)),
                    }
                },
                None => break,
            }
        }
        tokens
    }
    fn putback_token(&mut self, token: Token) -> Result<(), LexerError> {
        self.buffer.push_front(token);
        Ok(())
    }
}