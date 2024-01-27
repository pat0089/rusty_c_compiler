mod lexer;

use crate::lexer::Lexer;
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

fn main() {

    let input = read_from_args();

    let lexer = Lexer::new(&input.unwrap_or("int main() { return 0; }".to_string()));
    
    for token in lexer.buffer() {
        println!("{:?}", token);
    }
    println!("Hello Rusty C Compiler!");
}
