mod compiler;

use crate::compiler::CompilerError;
use crate::compiler::lexer::Lexer;
use crate::compiler::parser::Parser;
use crate::compiler::code_generator::CodeGenerator;
use std::process::Command;
use std::{env, io};
use std::fs::read_to_string;

fn read_from_args() -> Result<String, io::Error> {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        println!("Usage: {} <input>", args[0]);
        return Err(io::Error::new(io::ErrorKind::Other, "Invalid arguments"));
    }

    read_to_string(&args[1])
}

fn compile_with_gcc(assembly_file: &str, output_file: &str) -> Result<(), std::io::Error> {
    let status = Command::new("gcc")
        .arg("-m32")
        .arg(assembly_file)
        .arg("-o")
        .arg(output_file)
        .status()?;

    if status.success() {
        Ok(())
    } else {
        Err(std::io::Error::new(std::io::ErrorKind::Other, "GCC compilation failed"))
    }
}

fn main() -> Result<(), CompilerError> {

    let input = read_from_args();

    let mut lexer = Lexer::new(&input.unwrap_or("int main() { return 0; }".to_string()));
    lexer.validate()?;
    let mut parser = Parser::new(lexer);
    match parser.parse() {
        Ok(_) => {
            println!("Parsing successful!");
            println!("{:#?}", parser.get_ast());
        }
        Err(e) => {
            return Err(e.into());
        },
    }
    let mut code_generator = CodeGenerator::new();
    code_generator.generate(parser.get_ast());
    let assembly_file = "output.s";
    let output_file = "output";
    code_generator.write_to_file(assembly_file)?;
    compile_with_gcc(assembly_file, output_file)?;
    Ok(())
}
