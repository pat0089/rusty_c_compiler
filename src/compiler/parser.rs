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
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Integer(i32),
    UnaryOperator(TokenType, Box<Expression>),
    BinaryOperator(TokenType, Box<Expression>, Box<Expression>),
}

#[derive(Debug)]
pub struct Parser<T: TokenStream> {
    token_stream: T,
    //AST goes here
    ast: Program,
}

impl<T: TokenStream> Parser<T> {
    pub fn new(token_stream: T) -> Parser<T> {
        Parser {
            token_stream,
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
        self.try_parse(TokenType::Keyword(KeywordType::IdentifierType(IdentifierType::Int)))?;
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
        let parser_func = |parser: &mut Self| parser.parse_term();
        let separator_func = |token_type: TokenType| -> bool {
            match token_type {
                TokenType::Addition | TokenType::Negation => true,
                _ => false,
            }
        };
        self.parse_none_or_more(parser_func, separator_func)
    }

    fn parse_term(&mut self) -> Result<Expression, ParsingError> {
        let parser_func = |parser: &mut Self| parser.parse_factor();
        let separator_func = |token_type: TokenType| -> bool {
            match token_type {
                TokenType::Multiplication | TokenType::Division => true,
                _ => false,
            }
        };
        self.parse_none_or_more(parser_func, separator_func)
    }

    fn parse_factor(&mut self) -> Result<Expression, ParsingError> {
        let next_token = self.token_stream.next_token();
        match next_token {
            Some(Ok(token)) => {
                match token.get_type() {
                    TokenType::LParen => {
                        let expression = self.parse_expression()?;
                        self.try_parse(TokenType::RParen)?;
                        Ok(expression)
                    }
                    TokenType::Negation | TokenType::BitwiseComplement | TokenType::LogicalNegation => {
                        let operator = token.get_type();
                        let factor = self.parse_factor()?;
                        Ok(Expression::UnaryOperator(operator, Box::new(factor)))
                    }
                    TokenType::IntegerLiteral(i) => Ok(Expression::Integer(i)),
                    _ => {
                        Err(ParsingError::new(format!("Expected Factor, found {:?}", token.get_type())))
                    }
                }
            }
            _ => Err(ParsingError::new("Expected Factor, found EOF".to_string())),
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

    fn parse_none_or_more<F, G>(&mut self, parser_func : F, separator_func : G) -> Result<Expression, ParsingError> 
    where
        F: Fn(&mut Self) -> Result<Expression, ParsingError>,
        G: Fn(TokenType) -> bool
    {
        let mut result = parser_func(self)?;
        loop {
            let token_type = match self.token_stream.peek_token() {
                Some(Ok(token)) => token.get_type(),
                _ => break, // If there's no token, or an error, we exit the loop
            };

            match separator_func(token_type) {
                true => {
                    let operator = match self.token_stream.next_token() {
                        Some(Ok(op)) => op.get_type(),
                        _ => return Err(ParsingError::new(format!("Expected operator but found none."))),
                    };

                    let next_result = parser_func(self)?;

                    result = Expression::BinaryOperator(operator, Box::new(result), Box::new(next_result));
                }
                false => break,
            }
        }
        Ok(result)
    }
}