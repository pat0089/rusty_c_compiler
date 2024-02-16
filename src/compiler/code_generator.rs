use std::collections::HashMap;

use super::{lexer::TokenType, parser::{Expression, Function, Program, Statement}};

#[derive(Debug)]
pub struct CodeGeneratorError {
    message: String,
}

impl CodeGeneratorError {
    pub fn new(message: String) -> CodeGeneratorError {
        CodeGeneratorError { message }
    }
}

impl std::error::Error for CodeGeneratorError {
    fn description(&self) -> &str {
        &self.message
    }
}

impl std::fmt::Display for CodeGeneratorError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "CodeGenerator error: {}", self.message)
    }
}

#[derive(Debug)]
pub struct CodeGenerator {
    code: String,
    label_num: i32,
    variables: HashMap<String, i32>,
    function_contains_return: HashMap<String, bool>,
    stack_offset: i32,
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
            label_num: 0,
            variables: HashMap::new(),
            function_contains_return: HashMap::new(),
            stack_offset: 0,
        }
    }

    pub fn generate(&mut self, program: &Program) -> Result<(), CodeGeneratorError> {
        self.code.push_str("\t.text\n");
        for function in program.functions.iter() {
            self.generate_function(function)?;
        }
        Ok(())
    }

    fn generate_function(&mut self, function: &Function) -> Result<(), CodeGeneratorError> {
        self.code.push_str(format!("\t.globl\t{}\n", function.name).as_str());
        self.code.push_str(format!("{}:\n", function.name).as_str());
        self.function_contains_return
            .insert(function.name.to_string(), false);

        self.generate_fn_prologue();
        for statement in function.statements.iter() {
            match statement {
                Statement::Return(expression) => {
                    self.generate_expression(expression)?;
                    self.function_contains_return.insert(function.name.to_string(), true);
                    //self.code.push_str("\tret\n");
                },
                Statement::Declare(name, expression) => {
                    if self.variables.contains_key(name) {
                        Err(CodeGeneratorError::new(format!("Variable {} already declared", name)))?;
                    }
                    if let Some(expression) = expression {
                        self.generate_expression(expression)?;
                    }
                    self.code.push_str("\tpushl\t%eax\n");
                    self.stack_offset -= 4;
                    self.variables.insert(name.to_string(), self.stack_offset);
                }
                Statement::Expression(expression) => {
                    self.generate_expression(expression)?;
                }
                //_ => {}
            }
        }
        if !*self.function_contains_return.get(&function.name).unwrap_or(&false) {
            self.code.push_str("\tmovl\t$0, %eax\n");
        }
        self.generate_fn_epilogue();
        Ok(())
    }

    fn generate_expression(&mut self, expression: &Expression) -> Result<(), CodeGeneratorError> {
        match expression {
            Expression::Integer(integer) => {
                self.code.push_str(format!("\tmovl\t${}, %eax\n", integer).as_str());
            }
            Expression::UnaryOperator(op, expression) => {
                self.generate_expression(expression)?;
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
                        self.generate_expression(right)?;
                        self.push_register(Register::EAX);
                        self.generate_expression(left)?;
                        self.pop_register(Register::ECX);
                    }
                    TokenType::LogicalOr | TokenType::LogicalAnd => {
                        let jump = self.generate_label();
                        let end = self.generate_label();
                        self.generate_expression(left)?;
                        self.code.push_str("\tcmpl\t$0, %eax\n");
                        match op {
                            TokenType::LogicalAnd => {
                                self.code.push_str(format!("\tjne\t{}\n", jump).as_str());
                                self.code.push_str(format!("\tjmp\t{}\n", end).as_str());
                            }
                            TokenType::LogicalOr => {
                                self.code.push_str(format!("\tje\t{}\n", jump).as_str());
                                self.code.push_str("\tmovl\t$1, %eax\n");
                                self.code.push_str(format!("\tjmp\t{}\n", end).as_str());
                            }
                            _ => {}
                        }
                        self.code.push_str(format!("{}:\n", jump).as_str());
                        self.generate_expression(right)?;

                        self.code.push_str("\tcmpl\t$0, %eax\n");
                        self.code.push_str("\tmovl\t$0, %eax\n");
                        self.code.push_str("\tsetne\t%al\n");

                        self.code.push_str(format!("{}:\n", end).as_str());
                    }
                    _ => {
                        self.generate_expression(left)?;
                        self.push_register(Register::EAX);
                        self.generate_expression(right)?;
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
                    TokenType::Equals | TokenType::NotEquals | TokenType::GreaterThan | TokenType::LessThan
                    | TokenType::GreaterThanOrEquals | TokenType::LessThanOrEquals => {
                        self.code.push_str("\tcmpl\t%eax, %ecx\n");
                        self.code.push_str("\tmovl\t$0, %eax\n");
                        match op {
                            TokenType::Equals => self.code.push_str("\tsete\t%al\n"),
                            TokenType::NotEquals => self.code.push_str("\tsetne\t%al\n"),
                            TokenType::GreaterThan => self.code.push_str("\tsetg\t%al\n"),
                            TokenType::LessThan => self.code.push_str("\tsetl\t%al\n"),
                            TokenType::GreaterThanOrEquals => self.code.push_str("\tsetge\t%al\n"),
                            TokenType::LessThanOrEquals => self.code.push_str("\tsetle\t%al\n"),
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
            Expression::Assign(name, expression) => {
                self.generate_expression(expression)?;
                let variable_offset = match self.variables.get(name) {
                    Some(offset) => *offset,
                    None => {
                        return Err(CodeGeneratorError::new(format!("Variable not found: {}", name)));
                    }
                };
                self.code.push_str(format!("\tmovl\t%eax, {}(%ebp)\n", variable_offset).as_str());
            }
            Expression::Variable(name) => {
                let variable_offset = match self.variables.get(name) {
                    Some(offset) => *offset,
                    None => {
                        return Err(CodeGeneratorError::new(format!("Variable not found, not declared: {}", name)));
                    }
                };
                self.code.push_str(format!("\tmovl\t{}(%ebp), %eax\n", variable_offset).as_str());
            }
        }
        Ok(())
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

    fn generate_label(&mut self) -> String {
        let label = format!("_lab{}", self.label_num);
        self.label_num += 1;
        label
    }

    fn generate_fn_prologue(&mut self) {
        self.code.push_str("\tpush %ebp\n");
        self.code.push_str("\tmovl %esp, %ebp\n");
    }

    fn generate_fn_epilogue(&mut self) {
        self.code.push_str("\tmovl %ebp, %esp\n");
        self.code.push_str("\tpop %ebp\n");
        self.code.push_str("\tret\n");
    }
    
}

