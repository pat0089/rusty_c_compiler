pub mod lexer;
pub mod parser;
pub mod code_generator;
pub mod util;

pub use lexer::LexerError;
pub use parser::ParsingError;
pub use code_generator::CodeGeneratorError;

#[derive(Debug)]
pub struct CompilerError {
    message: String,
}

impl CompilerError {
    pub fn new(message: String) -> CompilerError {
        CompilerError { message }
    }
}

impl std::error::Error for CompilerError {
    fn description(&self) -> &str {
        &self.message
    }
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Compiler error: {}", self.message)
    }
}

impl From<LexerError> for CompilerError {
    fn from(err: LexerError) -> Self {
        // Convert LexerError to CompilerError
        // You might want to include some of the original error's information
        CompilerError::new(err.to_string())
    }
}

impl From<ParsingError> for CompilerError {
    fn from(err: ParsingError) -> Self {
        // Convert ParsingError to CompilerError
        // You might want to include some of the original error's information
        CompilerError::new(err.to_string())
    }
}

impl From<CodeGeneratorError> for CompilerError {
    fn from(err: CodeGeneratorError) -> Self {
        // Convert CodeGeneratorError to CompilerError
        // You might want to include some of the original error's information
        CompilerError::new(err.to_string())
    }
}

impl From<std::io::Error> for CompilerError {
    fn from(err: std::io::Error) -> Self {
        // Convert std::io::Error to CompilerError
        // You might want to include some of the original error's information
        CompilerError::new(err.to_string())
    }
}
