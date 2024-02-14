use std::{env, io};
use std::fs::read_to_string;
use std::path::Path;
use std::process::Command;

pub fn read_from_args() -> Result<(String, String, String), io::Error> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage: {} <file path>", args[0]);
        return Err(io::Error::new(io::ErrorKind::Other, "Invalid arguments"));
    }

    let path = Path::new(&args[1]);

    if !path.exists() || !path.is_file() {
        return Err(io::Error::new(io::ErrorKind::Other, format!("File '{}' does not exist", path.display())));
    }

    let file_name_without_extension = path.file_stem()
        .and_then(|name| name.to_str())
        .unwrap_or_default()
        .to_string();

    let mut file_path_string = path.parent()
        .map(|parent| parent.to_string_lossy().into_owned())
        .unwrap_or_default();
        
    file_path_string.push('/');

    let file_contents = read_to_string(&args[1])?;

    Ok((file_contents, file_name_without_extension, file_path_string))
}

pub fn compile_with_gcc(assembly_file: &str, output_file: &str) -> Result<(), std::io::Error> {
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