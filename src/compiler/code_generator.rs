use super::{lexer::TokenType, parser::{Expression, Function, Program, Statement}};

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
                    self.generate_expression(expression);
                    self.code.push_str("\tret\n");
                }
            }
        }
    }

    fn generate_expression(&mut self, expression: &Expression) {
        match expression {
            Expression::Integer(integer) => {
                self.code.push_str(format!("\tmovl\t${}, %eax\n", integer).as_str());
            }
            Expression::UnaryOperator(op, expression) => {
                self.generate_expression(expression);
                match op {
                    TokenType::Negation => {
                        self.code.push_str("\tneg\t%eax\n");
                    }
                    TokenType::BitwiseComplement => {
                        self.code.push_str("\tnot\t%eax\n");
                    }
                    TokenType::LogicalNegation => {
                        self.code.push_str("\tcmpl\t$0, %eax\n");
                        self.code.push_str("\tmovl\t$0, %eax\n");
                        self.code.push_str("\tsete\t%al\n");
                    }
                    _ => {}
                }
            }
        }
    }

    pub fn get_code(&self) -> &str {
        self.code.as_str()
    }

    pub fn write_to_file(&self, assembly_file: &str) -> Result<(), std::io::Error> {
        std::fs::write(assembly_file, self.get_code())
    }
    
}

