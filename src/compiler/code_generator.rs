use super::parser::{Expression, Function, Program, Statement};

pub struct CodeGenerator {
    code: String,
}

impl CodeGenerator {
    pub fn new() -> CodeGenerator {
        CodeGenerator {
            code: String::new(),
        }
    }

    pub fn generate(&mut self, program: &Program) {
        self.code.push_str("\t.text\n");
        for function in program.functions.iter() {
            self.generate_function(function);
        }
    }

    fn generate_function(&mut self, function: &Function) {
        self.code.push_str(format!("\t.globl\t{}\n", function.name).as_str());
        self.code.push_str(format!("{}:\n", function.name).as_str());
        for statement in function.statements.iter() {
            match statement {
                Statement::Return(expression) => {
                    self.generate_return(expression);
                }
            }
        }
    }

    fn generate_return(&mut self, expression: &Expression) {
        match expression {
            Expression::Integer(integer) => {
                self.code.push_str(format!("\tmovl\t${}, %eax\n", integer).as_str());
            }
        }
        self.code.push_str("\tret\n")
    }

    pub fn get_code(&self) -> &str {
        self.code.as_str()
    }

    pub fn write_to_file(&self, assembly_file: &str) -> Result<(), std::io::Error> {
        std::fs::write(assembly_file, self.get_code())
    }
    
}

