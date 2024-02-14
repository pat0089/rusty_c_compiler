use std::{collections::{HashMap, HashSet, VecDeque}, fmt::{self, Display} };
use regex::Regex;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IdentifierType {
    Int,
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum KeywordType {
    Return,
    IdentifierType(IdentifierType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    Illegal(LexerError),
    EOF,
    Identifier(String),
    Equality,
    Inequality,
    LessThanOrEqual,
    GreaterThanOrEqual,
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
    Negation,
    BitwiseComplement,
    LogicalNegation,
    Addition,
    Multiplication,
    Division,
}

impl TokenType {
    fn from_str(to_string: &str) -> TokenType {
        match to_string {
            "==" => TokenType::Equality,
            "!=" => TokenType::Inequality,
            "<=" => TokenType::LessThanOrEqual,
            ">=" => TokenType::GreaterThanOrEqual,
            ";" => TokenType::Semicolon,
            "(" => TokenType::LParen,
            ")" => TokenType::RParen,
            "{" => TokenType::LBrace,
            "}" => TokenType::RBrace,
            "[" => TokenType::LBracket,
            "]" => TokenType::RBracket,
            "<" => TokenType::LChevron,
            ">" => TokenType::RChevron,
            "-" => TokenType::Negation,
            "~" => TokenType::BitwiseComplement,
            "!" => TokenType::LogicalNegation,
            "+" => TokenType::Addition,
            "*" => TokenType::Multiplication,
            "/" => TokenType::Division,
            "int" => TokenType::Keyword(KeywordType::IdentifierType(IdentifierType::Int)),
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
    fn peek_token(&self) -> Option<Result<&Token, LexerError>>;
    fn peek_tokens(&self, n: usize) -> Vec<Result<&Token, LexerError>>;
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
        let keywords = HashSet::from(["int", "return"]);
        let symbols = HashSet::from([";", "(", ")", "{", "}", "[", "]", "<", ">", "=", "+", "-", "*", "/", "%", "!", "~", "&", "|"]);
        let potential_double_symbol_chars = HashSet::from(["=", "<", ">", "!", "&", "|"]);
        let final_line_num = input.lines().count();
        let mut last_char_num = 0;

        let get_keyword_type = |keyword: &String| -> TokenType {
            match keyword.as_str() {
                "int" => TokenType::Keyword(KeywordType::IdentifierType(IdentifierType::Int)),
                "return" => TokenType::Keyword(KeywordType::Return),
                _ => TokenType::Illegal(LexerError::new(format!("Invalid keyword: {}", keyword))),
            }
        };

        let token_type_from_current = |current_token: String | -> TokenType {
            let token_type : TokenType;
            if keywords.contains(&current_token.as_str()) {
                token_type = get_keyword_type(&current_token);
            } else if valid_identifier_regex.is_match(&current_token) {
                token_type = TokenType::Identifier(current_token.clone());
            } else if valid_number_regex.is_match(&current_token) {
                token_type = match current_token.parse::<i32>() {
                    Ok(i) => TokenType::IntegerLiteral(i),
                    Err(_) => TokenType::Illegal(LexerError::new(format!("Invalid number: {}", current_token))),
                };
            } else {
                token_type = TokenType::from_str(&current_token);
            }
            token_type
        };

        let is_valid_double_symbol = |a: &String, b : &char| -> bool {
            match (a.as_str(), b) {
                (">", '=') | ("<", '=') | ("!", '=') | ("=", '=') | 
                ("&", '&') | ("|", '|') => true,
                _ => false
            }
        };

        //loop through the input on each line
        for (line_num, line) in input.lines().enumerate() {

            //make sure to reset the current_token and push it to the buffer if it is not empty at each new line
            if !current_token.is_empty() {
                let token_type = token_type_from_current(current_token.clone());
                tokens.push(
                    Token::new(
                        token_type,
                        current_token.clone(),
                        line_num as i32,
                        (last_char_num + 1 - current_token.len()) as i32,
                        last_char_num as i32
                    )
                );
                current_token.clear();
            }
            
            //then loop through the line by character
            for (char_num, c) in line.chars().enumerate() {
                
                //if the character is whitespace, i.e. a space (because that is the only whitespace character we can encounter)
                //and if the current token is not empty,
                //attempt to add the token to the buffer
                //check through all the valid posibilities before adding the an illegal token to the buffer
                //reset the string for the next token, update the last character number, then continue to the next character
                if c.is_whitespace() && !current_token.is_empty() {
                    let token_type = token_type_from_current(current_token.clone());
                    tokens.push(
                        Token::new(
                            token_type,
                            current_token.clone(),
                            (line_num + 1) as i32,
                            (last_char_num + 1 - current_token.len()) as i32,
                            last_char_num as i32
                        )
                    );
                    current_token.clear();
                    last_char_num = char_num;
                    continue;
                }

                //if the character is not whitespace
                //and if the current token is empty
                //add it to the current token otherwise continue to the the next character
                if current_token.is_empty() {
                    if !c.is_whitespace() {
                        current_token.push(c);
                        last_char_num = char_num;
                    }
                    continue;
                }

                //by now we have handled all the whitespace cases
                //now we need to handle the rest of the characters, that is symbols and keywords/integer literals/identifiers
                //there are now 5 possible states to be in
                // 1. current_token contains a symbol and the current character is a symbol and the symbol is a double symbol
                // 2. current_token contains a symbol and the current character is a symbol but the symbol is not a double symbol
                // 3. current_token contains a symbol and the current character is not a symbol
                // 4. current_token contains anything other than a symbol and the current character is a symbol
                // 5. current_token contains anything other than a symbol and the current character is not a symbol

                //if the current_token is a symbol
                if symbols.contains(&current_token.as_str()) {
                    //if the current character is a symbol
                    if symbols.contains(&c.to_string().as_str()) {
                        //if the symbol is a double symbol
                        if potential_double_symbol_chars.contains(&current_token.as_str()) && is_valid_double_symbol(&current_token, &c) {
                            current_token.push(c);
                            let token_type = token_type_from_current(current_token.clone());
                            tokens.push(
                                Token::new(
                                    token_type,
                                    current_token.clone(),
                                    (line_num + 1) as i32,
                                    (char_num - current_token.len() + 1) as i32,
                                    char_num as i32
                                )
                            );
                            current_token.clear();
                        //if the symbol is not a double symbol
                        } else {
                            let token_type = token_type_from_current(current_token.clone());
                            tokens.push(
                                Token::new(
                                    token_type,
                                    current_token.clone(),
                                    (line_num + 1) as i32,
                                    (char_num - current_token.len()) as i32,
                                    last_char_num as i32
                                )
                            );
                            current_token.clear();
                            current_token.push(c);
                        }
                    //if the current character is not a symbol
                    } else {
                        let token_type = token_type_from_current(current_token.clone());
                        tokens.push(
                            Token::new(
                                token_type,
                                current_token.clone(),
                                (line_num + 1) as i32,
                                (char_num - current_token.len()) as i32,
                                last_char_num as i32
                            )
                        );
                        current_token.clear();
                        current_token.push(c);
                    }
                //if the current_token is not a symbol
                } else {
                    //if the current character is not a symbol
                    if !symbols.contains(&c.to_string().as_str()) {
                        current_token.push(c);
                    //if the current character is a symbol
                    } else {
                        let token_type = token_type_from_current(current_token.clone());
                        tokens.push(
                            Token::new(
                                token_type,
                                current_token.clone(),
                                (line_num + 1) as i32,
                                (char_num - current_token.len()) as i32,
                                last_char_num as i32
                            )
                        );
                        current_token.clear();
                        current_token.push(c);
                    }
                }

                //update the last character number
                last_char_num = char_num;
            }
            
        }

        //add the last token to the buffer
        if !current_token.is_empty() {
            tokens.push(
                Token::new(
                    token_type_from_current(current_token.clone()),
                    current_token.clone(),
                    final_line_num as i32 + 1,
                    (last_char_num + 1 - current_token.len()) as i32,
                    last_char_num as i32
                )
            );
        }

        //add the EOF token
        tokens.push(
            Token::new(
                TokenType::EOF, 
                String::new(), 
                final_line_num as i32 + 1, 
                last_char_num as i32, 
                last_char_num as i32
            ));
        
        l.buffer = tokens.into();
        l
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
    fn peek_token(&self) -> Option<Result<&Token, LexerError>> {
        if self.buffer.front().is_some() {
            Some(Ok(self.buffer.front()?))
        } else {
            None
        }
    }
    fn peek_tokens(&self, n: usize) -> Vec<Result<&Token, LexerError>> {
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