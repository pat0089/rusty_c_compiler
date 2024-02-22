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
    pub block: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(String, Option<Expression>),
}

#[derive(Debug)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Block(Vec<BlockItem>),
}

#[derive(Debug)]
pub enum Expression {
    Assign(String, Box<Expression>),
    Variable(String),
    Integer(i32),
    UnaryOperator(TokenType, Box<Expression>),
    BinaryOperator(TokenType, Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
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
        
        let block = self.parse_block()?;

        Ok(Function {
            name: name.get_literal().to_string(),
            block,
        })        
    }

    fn parse_block_item(&mut self) -> Result<BlockItem, ParsingError> {
        let token_type = match self.token_stream.peek_token() {
            Some(Ok(token)) => token.get_type(),
            _ => return Err(ParsingError::new("Expected block item".to_string())),
        };

        match token_type {
            TokenType::Keyword(KeywordType::IdentifierType(IdentifierType::Int)) => {
                let declaration = self.parse_declaration()?;
                Ok(declaration)
            }
            _ => {
                let statement = self.parse_statement()?;
                Ok(BlockItem::Statement(statement))
            }
        }
    }

    fn parse_block(&mut self) -> Result<Vec<BlockItem>, ParsingError> {
        self.try_parse(TokenType::LBrace)?;

        let mut block = Vec::new();
        loop {

            if let Some(Ok(token)) = self.token_stream.peek_token() {
                if token.get_type() == TokenType::RBrace {
                    break;
                }
            }

            let block_item = self.parse_block_item();
            match block_item {
                Ok(block_item) => {
                    block.push(block_item);
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }

        self.try_parse(TokenType::RBrace)?;
        Ok(block)
    }

    fn parse_declaration(&mut self) -> Result<BlockItem, ParsingError> {
        self.try_parse(TokenType::Keyword(KeywordType::IdentifierType(IdentifierType::Int)))?;
        let name = self.try_parse(TokenType::Identifier("".to_string()))?;
        let optional_assignment = self.parse_optional_assignment()?;
        self.try_parse(TokenType::Semicolon)?;
        Ok(BlockItem::Declaration(name.get_literal().to_string(), optional_assignment))
    }

    fn parse_statement(&mut self) -> Result<Statement, ParsingError> {
        let token_type = match self.token_stream.peek_token() {
            Some(Ok(token)) => token.get_type(),
            _ => return Err(ParsingError::new("Expected statement".to_string())),
        };
        match token_type {
            TokenType::Keyword(KeywordType::Return) => {
                self.try_parse(TokenType::Keyword(KeywordType::Return))?;
                let expression = self.parse_expression()?;
                self.try_parse(TokenType::Semicolon)?;
                return Ok(Statement::Return(expression));
            },
            TokenType::Keyword(KeywordType::If) => {
                self.try_parse(TokenType::Keyword(KeywordType::If))?;
                self.try_parse(TokenType::LParen)?;
                let expression = self.parse_expression()?;
                self.try_parse(TokenType::RParen)?;
                let statement = self.parse_statement()?;
                let next_token_type = self.peek_type();
                if let Some(TokenType::Keyword(KeywordType::Else)) = next_token_type {
                    self.try_parse(TokenType::Keyword(KeywordType::Else))?;
                    let else_statement = self.parse_statement()?;
                    return Ok(Statement::If(expression, Box::new(statement), Some(Box::new(else_statement))));
                }
                return Ok(Statement::If(expression, Box::new(statement), None));
            },
            TokenType::LBrace => {
                let block = self.parse_block()?;
                return Ok(Statement::Block(block));
            },
            _ => {
                let expression = self.parse_expression()?;
                self.try_parse(TokenType::Semicolon)?;
                return Ok(Statement::Expression(expression));
            }
        }

    }

    fn parse_expression(&mut self) -> Result<Expression, ParsingError> {
        let token = self.token_stream.next_token().ok_or_else(|| ParsingError::new("Expected expression".to_string()))??;
        if let Some(token_type) = self.peek_type() {
            match token_type {
                TokenType::Assignment => {
                    self.try_parse(TokenType::Assignment)?;
                    let expression = self.parse_expression()?;
                    //self.try_parse(TokenType::Semicolon)?;
                    return Ok(Expression::Assign(token.get_literal().to_string(), Box::new(expression)));
                }
                _ => self.token_stream.putback_token(token)?,
            }
        }
        self.parse_conditional_expression()
    }

    fn parse_conditional_expression(&mut self) -> Result<Expression, ParsingError> {
        let expression = self.parse_logical_or()?;
        let next_token_type = self.peek_type();
        if let Some(TokenType::QuestionMark) = next_token_type {
            self.try_parse(TokenType::QuestionMark)?;
            let true_expression = self.parse_expression()?;
            self.try_parse(TokenType::Colon)?;
            let false_expression = self.parse_conditional_expression()?;
            return Ok(Expression::Conditional(Box::new(expression), Box::new(true_expression), Box::new(false_expression)));
        }
        Ok(expression)
    }

    fn parse_logical_or(&mut self) -> Result<Expression, ParsingError> {
        let parser_func = |parser: &mut Self| parser.parse_logical_and();
        let separator_func = |token_type: TokenType| -> bool {
            match token_type {
                TokenType::LogicalOr => true,
                _ => false,
            }
        };
        self.parse_none_or_more(parser_func, separator_func)
    }

    fn parse_logical_and(&mut self) -> Result<Expression, ParsingError> {
        let parser_func = |parser: &mut Self| parser.parse_equality();
        let separator_func = |token_type: TokenType| -> bool {
            match token_type {
                TokenType::LogicalAnd => true,
                _ => false,
            }
        };
        self.parse_none_or_more(parser_func, separator_func)
    }

    fn parse_equality(&mut self) -> Result<Expression, ParsingError> {
        let parser_func = |parser: &mut Self| parser.parse_relational();
        let separator_func = |token_type: TokenType| -> bool {
            match token_type {
                TokenType::Equals | TokenType::NotEquals => true,
                _ => false,
            }
        };
        self.parse_none_or_more(parser_func, separator_func)
    }

    fn parse_relational(&mut self) -> Result<Expression, ParsingError> {
        let parser_func = |parser: &mut Self| parser.parse_additive();
        let separator_func = |token_type: TokenType| -> bool {
            match token_type {
                TokenType::LessThan | TokenType::GreaterThan |
                TokenType::LessThanOrEquals | TokenType::GreaterThanOrEquals =>
                true,
                _ => false,
            }
        };
        self.parse_none_or_more(parser_func, separator_func)
    }

    fn parse_additive(&mut self) -> Result<Expression, ParsingError> {
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
                    TokenType::Identifier(name) => Ok(Expression::Variable(name)),
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

    fn peek_type(&self) -> Option<TokenType> {
        match self.token_stream.peek_token() {
            Some(Ok(token)) => Some(token.get_type()),
            _ => None,
        }
    }

    fn parse_optional_assignment(&mut self) -> Result<Option<Expression>, ParsingError> {
        match self.peek_type() {
            Some(TokenType::Assignment) => {
                self.token_stream.next_token();
                let expression= self.parse_expression()?;
                Ok(Some(expression))
            },
            _ => Ok(None),
        }
    }
}