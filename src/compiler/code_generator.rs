use super::{lexer::TokenType, parser::{Expression, Function, Program, Statement}};

pub struct CodeGenerator {
    code: String,
}

#[derive(Debug)]
pub enum Register {
    EAX,
    //EBX,
    ECX,
    //EDX,
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Register::EAX => write!(f, "%eax"),
            //Register::EBX => write!(f, "%ebx"),
            Register::ECX => write!(f, "%ecx"),
            //Register::EDX => write!(f, "%edx"),
        }
    }
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
            Expression::BinaryOperator(op, left, right) => {
                match op {
                    TokenType::Negation | TokenType::Division => {
                        self.generate_expression(right);
                        self.push_register(Register::EAX);
                        self.generate_expression(left);
                        self.pop_register(Register::ECX);
                    }
                    _ => {
                        self.generate_expression(left);
                        self.push_register(Register::EAX);
                        self.generate_expression(right);
                        self.pop_register(Register::ECX);
                    }
                }
                match op {
                    TokenType::Addition => {
                        self.code.push_str("\taddl\t%ecx, %eax\n");
                    },
                    TokenType::Negation => {
                        self.code.push_str("\tsubl\t%ecx, %eax\n");
                    },
                    TokenType::Multiplication => {
                        self.code.push_str("\timul\t%ecx, %eax\n");
                    },
                    TokenType::Division => {
                        self.code.push_str("\tcdq\n");
                        self.code.push_str("\tidivl\t%ecx\n");
                    },
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

    fn push_register(&mut self, register: Register) {
        self.code.push_str(format!("\tpush\t{}\n", register).as_str());
    }

    fn pop_register(&mut self, register: Register) {
        self.code.push_str(format!("\tpop\t{}\n", register).as_str());
    }
    
}

