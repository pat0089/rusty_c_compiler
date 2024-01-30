use std::collections::{HashMap, HashSet};

use super::lexer::{TokenStream, Token, LexerError, TokenType, IdentifierType, KeywordType};

#[derive(Debug)]
pub struct ParsingError {
    message: String,
}

impl ParsingError {
    pub fn new(message: String) -> ParsingError {
        ParsingError { message }
    }
}

impl std::error::Error for ParsingError {
    fn description(&self) -> &str {
        &self.message
    }
}

impl std::fmt::Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Parsing error: {}", self.message)
    }
}

impl From<LexerError> for ParsingError {
    fn from(err: LexerError) -> Self {
        // Convert LexerError to ParsingError
        // You might want to include some of the original error's information
        ParsingError::new(err.to_string())
    }
}

#[derive(Debug)]
pub struct Program {
    functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Integer(i32),
}

#[derive(Debug)]
pub struct Parser<T: TokenStream> {
    token_stream: T,
    //AST goes here
    ast: Program,
    bracket_stack: Vec<TokenType>,
}

impl<T: TokenStream> Parser<T> {
    pub fn new(token_stream: T) -> Parser<T> {
        Parser {
            token_stream,
            bracket_stack: Vec::new(),
            ast: Program { functions: Vec::new() },
        }
    }

    pub fn parse(&mut self) -> Result<(), ParsingError> {
        let program = self.parse_program()?;
        self.ast = program;
        Ok(())        
    }

    pub fn get_ast(&self) -> &Program {
        &self.ast
    }

    fn try_parse(&mut self, expected_type: TokenType) -> Result<Token, ParsingError> {
        let token = self.token_stream.next_token()
            .ok_or_else(|| ParsingError::new("Expected token, found EOF".to_string()))??;

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
                (TokenType::LParen, TokenType::RParen),
                (TokenType::LBrace, TokenType::RBrace),
                (TokenType::LBracket, TokenType::RBracket),
                (TokenType::LChevron, TokenType::RChevron),
            ]);

            //handle the bracket matching if the token is an open bracket
            match (close_brackets.contains(&token.get_type()), open_brackets.contains(&token.get_type())) {
                (true, false) => {
                    if self.bracket_stack.last().is_none() {
                        return Err(ParsingError::new(format!("Expected {}, found {} ({}:{}-{})", expected_type, token.get_type(), token.get_line(), token.get_start(), token.get_end())));
                    } else if let Some(open_bracket) = self.bracket_stack.pop() {
                        if open_closed_map.get(&open_bracket).unwrap() != &token.get_type() {
                            return Err(ParsingError::new(format!("Expected {}, found {} ({}:{}-{})", expected_type, token.get_type(), token.get_line(), token.get_start(), token.get_end())));
                        }
                    }
                },
                (false, true) => {
                    self.bracket_stack.push(token.get_type());
                },
                _ => (),
            }

            match expected_type {
                //handle all comple enums first
                TokenType::Identifier(_) => {
                    if let TokenType::Identifier(_) = token.get_type() {
                        Ok(token)
                    } else {
                        Err(ParsingError::new("Expected identifier".to_string()))
                    }
                }
                TokenType::IntegerLiteral(_) => {
                    if let TokenType::IntegerLiteral(_) = token.get_type() {
                        Ok(token)
                    } else {
                        Err(ParsingError::new(format!("Expected integer ({}:{}-{})", token.get_line(), token.get_start(), token.get_end())))
                    }
                }
                _ => {
                    if token.get_type() == expected_type {
                        Ok(token)
                    } else {
                        Err(ParsingError::new(format!("Expected {:?}, found {:?} ({}:{}-{})", expected_type, token.get_type(), token.get_line(), token.get_start(), token.get_end())))
                    }
                }
            }
    }

    fn parse_function(&mut self) -> Result<Function, ParsingError> {
        self.try_parse(TokenType::IdentifierType(IdentifierType::Int))?;
        let name = self.try_parse(TokenType::Identifier("".to_string()))?;
        self.try_parse(TokenType::LParen)?;
        self.try_parse(TokenType::RParen)?;
        self.try_parse(TokenType::LBrace)?;

        let mut statements = Vec::new();
        loop {

            if let Some(Ok(token)) = self.token_stream.peek_token() {
                if token.get_type() == TokenType::RBrace {
                    break;
                }
            }

            let statement = self.parse_statement();
            match statement {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }

        self.try_parse(TokenType::RBrace)?;

        Ok(Function {
            name: name.get_literal().to_string(),
            statements
        })        
    }

    fn parse_statement(&mut self) -> Result<Statement, ParsingError> {
        self.try_parse(TokenType::Keyword(KeywordType::Return))?;
        let expression = self.parse_expression()?;
        self.try_parse(TokenType::Semicolon)?;
        return Ok(Statement::Return(expression));
    }

    fn parse_expression(&mut self) -> Result<Expression, ParsingError> {
        let integer = self.try_parse(TokenType::IntegerLiteral(0))?;
        match integer.get_type() {
            TokenType::IntegerLiteral(integer) => {
                Ok(Expression::Integer(integer))
            }
            _ => {
                Err(ParsingError::new(format!("Expected integer ({}:{}-{})", integer.get_line(), integer.get_start(), integer.get_end())))
            }
        }
    }

    fn parse_program(&mut self) -> Result<Program, ParsingError> {
        let mut functions = Vec::new();
        loop {
            let function = self.parse_function();
            match function {
                Ok(function) => {
                    functions.push(function);
                    if self.token_stream.peek_token().is_some_and(|token| 
                        token.is_ok_and(
                            |token| 
                            token.get_type() == TokenType::EOF
                        )
                    ) { break; }
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }

        Ok(Program { functions })
    }
}