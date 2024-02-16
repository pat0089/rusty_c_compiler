mod compiler;

use crate::compiler::CompilerError;
use crate::compiler::lexer::Lexer;
use crate::compiler::parser::Parser;
use crate::compiler::code_generator::CodeGenerator;
use crate::compiler::util::{read_from_args, compile_with_gcc};

fn main() -> Result<(), CompilerError> {

    let input = read_from_args()?;

    let (file_contents, file_name, file_path) = input;

    match file_contents.is_empty() {
        true => return Err(CompilerError::new(format!("File {} is empty", file_path))),
        _ => {}
    }

    let mut lexer = Lexer::new(&file_contents);
    lexer.validate()?;
    let mut parser = Parser::new(lexer);
    match parser.parse() {
        Err(e) => {
            return Err(e.into());
        },
        _ => {}
    }
    let mut code_generator = CodeGenerator::new();
    code_generator.generate(parser.get_ast())?;
    let assembly_file = format!("{}{}.s", file_path, file_name);
    let output_file = format!("{}{}", file_path, file_name);
    code_generator.write_to_file(assembly_file.as_str())?;
    compile_with_gcc(assembly_file.as_str(), output_file.as_str())?;
    Ok(())
}
