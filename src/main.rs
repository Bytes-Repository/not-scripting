use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use std::collections::HashMap;
use std::io::{self, Read};
use std::process::Command;
use anyhow::{anyhow, bail, Context, Result};

#[derive(Parser)]
#[grammar = "not-scripting.pest"]
pub struct NotScriptingParser;

fn main() -> Result<()> {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer)?;
    let code: &'static str = Box::leak(buffer.into_boxed_str());
    let mut interpreter = Interpreter::new();
    interpreter.execute(code)?;
    Ok(())
}

#[derive(Debug, Clone)]
enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Range(i64, i64),
    Function {
        params: Vec<String>,
        body: Vec<Pair<'static, Rule>>,
    },
}

struct Interpreter {
    variables: HashMap<String, Value>,
    functions: HashMap<String, Value>,
}

impl Interpreter {
    fn new() -> Self {
        Interpreter {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn execute(&mut self, code: &'static str) -> Result<()> {
        let mut parse_result = NotScriptingParser::parse(Rule::program, code)
            .context("Failed to parse program")?;
        let program_pair = parse_result.next().unwrap();
        let statements: Vec<Pair<'static, Rule>> = program_pair.into_inner().collect();
        for stmt in statements {
            self.exec_statement(stmt)?;
        }
        Ok(())
    }

    fn exec_statement(&mut self, pair: Pair<'static, Rule>) -> Result<()> {
        match pair.as_rule() {
            Rule::var_decl => self.exec_var_decl(pair),
            Rule::assign => self.exec_assign(pair),
            Rule::print_stmt => self.exec_print(pair),
            Rule::if_stmt => self.exec_if(pair),
            Rule::loop_stmt => self.exec_loop(pair),
            Rule::fn_decl => self.exec_fn_decl(pair),
            Rule::fn_call => self.exec_fn_call(pair),
            Rule::command => self.exec_command(pair),
            Rule::comment => Ok(()),
            Rule::expr => {
                let _ = self.eval_expr(pair.into_inner().next().unwrap())?;  // Eval but discard
                Ok(())
            }
            _ => bail!("Unexpected statement: {:?}", pair),
        }
    }

    fn exec_var_decl(&mut self, pair: Pair<'static, Rule>) -> Result<()> {
        let mut inner = pair.into_inner();
        let _keyword = inner.next().unwrap();  // let or var
        let ident = inner.next().unwrap().as_str().to_string();
        let _type_opt = inner.next();  // Optional type
        let _eq = inner.next();  // =
        let expr_pair = inner.next().unwrap();
        let value = self.eval_expr(expr_pair)?;
        self.variables.insert(ident, value);
        Ok(())
    }

    fn exec_assign(&mut self, pair: Pair<'static, Rule>) -> Result<()> {
        let mut inner = pair.into_inner();
        let ident = inner.next().unwrap().as_str().to_string();
        let _eq = inner.next();
        let expr_pair = inner.next().unwrap();
        let value = self.eval_expr(expr_pair)?;
        self.variables.insert(ident, value);
        Ok(())
    }

    fn exec_print(&mut self, pair: Pair<'static, Rule>) -> Result<()> {
        let mut inner = pair.into_inner();
        let keyword = inner.next().unwrap().as_str();
        let expr_pair = inner.next().unwrap();
        let value = self.eval_expr(expr_pair)?;
        let output = match value {
            Value::Int(n) => n.to_string(),
            Value::Float(f) => f.to_string(),
            Value::String(s) => s,
            Value::Bool(b) => b.to_string(),
            _ => bail!("Cannot print function or range"),
        };
        if keyword == "println!" || keyword == "writeln" {
            println!("{}", output);
        } else {
            print!("{}", output);
        }
        Ok(())
    }

    fn exec_if(&mut self, pair: Pair<'static, Rule>) -> Result<()> {
        let mut inner = pair.into_inner();
        let _if = inner.next();
        let cond_pair = inner.next().unwrap();
        let value = self.eval_expr(cond_pair)?;
        let cond = self.eval_as_bool(value)?;
        let then_block = inner.next().unwrap();
        let else_block_opt = inner.next();
        if cond {
            self.exec_block(then_block)?;
        } else if let Some(else_block) = else_block_opt {
            self.exec_block(else_block)?;
        }
        Ok(())
    }

    fn exec_loop(&mut self, pair: Pair<'static, Rule>) -> Result<()> {
        let mut inner = pair.into_inner();
        let keyword = inner.next().unwrap().as_str();
        match keyword {
            "loop" => {
                let count_opt = inner.next();
                let block = inner.next().unwrap();
                if let Some(count_pair) = count_opt {
                    let value = self.eval_expr(count_pair)?;
                    let count = self.eval_as_int(value)? as usize;
                    for _ in 0..count {
                        self.exec_block(block.clone())?;
                    }
                } else {
                    loop {
                        self.exec_block(block.clone())?;
                    }
                }
            }
            "while" => {
                let cond_pair = inner.next().unwrap();
                let block = inner.next().unwrap();
                loop {
                    let value = self.eval_expr(cond_pair.clone())?;
                    let cond = self.eval_as_bool(value)?;
                    if !cond {
                        break;
                    }
                    self.exec_block(block.clone())?;
                }
            }
            "for" => {
                let ident = inner.next().unwrap().as_str().to_string();
                let _in = inner.next();
                let range_expr = inner.next().unwrap();
                let block = inner.next().unwrap();
                let range = self.eval_expr(range_expr)?;
                if let Value::Range(start, end) = range {
                    for i in start..end {
                        self.variables.insert(ident.clone(), Value::Int(i));
                        self.exec_block(block.clone())?;
                    }
                } else {
                    bail!("For range must be a range");
                }
            }
            _ => bail!("Unknown loop"),
        }
        Ok(())
    }

    fn exec_fn_decl(&mut self, pair: Pair<'static, Rule>) -> Result<()> {
        let mut inner = pair.into_inner();
        let _keyword = inner.next();  // fn or def
        let name = inner.next().unwrap().as_str().to_string();
        let _lparen = inner.next();
        let param_list_opt = inner.next();
        let params: Vec<String> = if let Some(pl) = param_list_opt {
            pl.into_inner().map(|p| p.into_inner().next().unwrap().as_str().to_string()).collect()
        } else {
            vec![]
        };
        let block = inner.next().unwrap();
        let body: Vec<Pair<'static, Rule>> = block.into_inner().collect();
        self.functions.insert(name, Value::Function { params, body });
        Ok(())
    }

    fn exec_fn_call(&mut self, pair: Pair<'static, Rule>) -> Result<()> {
        let mut inner = pair.into_inner();
        let name = inner.next().unwrap().as_str().to_string();
        let arg_list_opt = inner.next();
        let args: Vec<Value> = if let Some(al) = arg_list_opt {
            al.into_inner().map(|e| self.eval_expr(e).unwrap()).collect()
        } else {
            vec![]
        };
        let result = self.call_function(&name, args)?;
        let _ = result;
        Ok(())
    }

    fn call_function(&mut self, name: &str, args: Vec<Value>) -> Result<Value> {
        if let Some(Value::Function { params, body }) = self.functions.get(name).cloned() {
            if params.len() != args.len() {
                bail!("Argument count mismatch");
            }
            let old_vars = self.variables.clone();
            for (p, a) in params.into_iter().zip(args) {
                self.variables.insert(p, a);
            }
            for stmt_pair in body {
                self.exec_statement(stmt_pair)?;
            }
            self.variables = old_vars;
            Ok(Value::Bool(true))  // Placeholder return
        } else {
            bail!("Unknown function: {}", name);
        }
    }

    fn exec_command(&mut self, pair: Pair<'static, Rule>) -> Result<()> {
        let cmd_str = pair.into_inner().next().unwrap().as_str();
        let output = Command::new("sh")
            .arg("-c")
            .arg(cmd_str)
            .output()?;
        if !output.status.success() {
            bail!("Command failed: {}", String::from_utf8_lossy(&output.stderr));
        }
        Ok(())
    }

    fn exec_block(&mut self, pair: Pair<'static, Rule>) -> Result<()> {
        for stmt in pair.into_inner() {
            self.exec_statement(stmt)?;
        }
        Ok(())
    }

    fn eval_expr(&mut self, pair: Pair<'static, Rule>) -> Result<Value> {
        match pair.as_rule() {
            Rule::range => {
                let mut inner = pair.into_inner();
                let start_pair = inner.next().unwrap();
                let end_pair = inner.next().unwrap();
                let start_val = self.eval_expr(start_pair)?;
                let end_val = self.eval_expr(end_pair)?;
                let start = self.eval_as_int(start_val)?;
                let end = self.eval_as_int(end_val)?;
                Ok(Value::Range(start, end))
            }
            Rule::logic_expr => self.eval_logic(pair.into_inner()),
            Rule::compare_expr => self.eval_compare(pair.into_inner()),
            Rule::arith_expr => self.eval_arith(pair.into_inner()),
            Rule::term => self.eval_term(pair.into_inner()),
            Rule::factor => {
                let inner_pair = pair.into_inner().next().unwrap();
                match inner_pair.as_rule() {
                    Rule::expr => self.eval_expr(inner_pair),
                    Rule::literal => self.eval_literal(inner_pair),
                    Rule::ident => {
                        let name = inner_pair.as_str();
                        self.variables.get(name).cloned().ok_or(anyhow!("Unknown var: {}", name))
                    }
                    Rule::fn_call => {
                        let mut call_inner = inner_pair.into_inner();
                        let name = call_inner.next().unwrap().as_str().to_string();
                        let arg_list_opt = call_inner.next();
                        let args: Vec<Value> = if let Some(al) = arg_list_opt {
                            al.into_inner().map(|e| self.eval_expr(e).unwrap()).collect()
                        } else {
                            vec![]
                        };
                        self.call_function(&name, args)
                    }
                    Rule::unary => {
                        let mut unary_inner = inner_pair.into_inner();
                        let op = unary_inner.next().unwrap().as_str();
                        let factor_pair = unary_inner.next().unwrap();
                        let factor = self.eval_expr(factor_pair)?;
                        match op {
                            "-" => match factor {
                                Value::Int(n) => Ok(Value::Int(-n)),
                                Value::Float(f) => Ok(Value::Float(-f)),
                                _ => bail!("Cannot negate"),
                            },
                            "!" => self.eval_as_bool(factor).map(|b| Value::Bool(!b)),
                            _ => bail!("Unknown unary"),
                        }
                    }
                    _ => bail!("Unexpected factor"),
                }
            }
            _ => bail!("Unexpected expr: {:?}", pair),
        }
    }

    fn eval_logic(&mut self, mut pairs: Pairs<'static, Rule>) -> Result<Value> {
        let mut left = self.eval_expr(pairs.next().unwrap())?;
        while let Some(op_pair) = pairs.next() {
            let op = op_pair.as_str();
            let right = self.eval_expr(pairs.next().unwrap())?;
            let left_bool = self.eval_as_bool(left)?;
            let right_bool = self.eval_as_bool(right)?;
            left = Value::Bool(match op {
                "&&" | "and" => left_bool && right_bool,
                "||" | "or" => left_bool || right_bool,
                _ => bail!("Unknown logic op"),
            });
        }
        Ok(left)
    }

    fn eval_compare(&mut self, mut pairs: Pairs<'static, Rule>) -> Result<Value> {
        let mut left = self.eval_expr(pairs.next().unwrap())?;
        while let Some(op_pair) = pairs.next() {
            let op = op_pair.as_str();
            let right = self.eval_expr(pairs.next().unwrap())?;
            left = Value::Bool(match (left, right) {
                (Value::Int(a), Value::Int(b)) => match op {
                    "==" => a == b,
                    "!=" => a != b,
                    ">" => a > b,
                    "<" => a < b,
                    ">=" => a >= b,
                    "<=" => a <= b,
                    _ => bail!("Unknown compare"),
                },
                (Value::Float(a), Value::Float(b)) => match op {
                    "==" => a == b,
                    "!=" => a != b,
                    ">" => a > b,
                    "<" => a < b,
                    ">=" => a >= b,
                    "<=" => a <= b,
                    _ => bail!("Unknown compare"),
                },
                (Value::String(a), Value::String(b)) => match op {
                    "==" => a == b,
                    "!=" => a != b,
                    _ => bail!("String compare limited"),
                },
                _ => bail!("Type mismatch in compare"),
            });
        }
        Ok(left)
    }

    fn eval_arith(&mut self, mut pairs: Pairs<'static, Rule>) -> Result<Value> {
        let mut left = self.eval_expr(pairs.next().unwrap())?;
        while let Some(op_pair) = pairs.next() {
            let op = op_pair.as_str();
            let right = self.eval_expr(pairs.next().unwrap())?;
            left = match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int(match op {
                    "+" => a + b,
                    "-" => a - b,
                    _ => bail!("Unknown arith"),
                }),
                (Value::Float(a), Value::Float(b)) => Value::Float(match op {
                    "+" => a + b,
                    "-" => a - b,
                    _ => bail!("Unknown arith"),
                }),
                _ => bail!("Type mismatch in arith"),
            };
        }
        Ok(left)
    }

    fn eval_term(&mut self, mut pairs: Pairs<'static, Rule>) -> Result<Value> {
        let mut left = self.eval_expr(pairs.next().unwrap())?;
        while let Some(op_pair) = pairs.next() {
            let op = op_pair.as_str();
            let right = self.eval_expr(pairs.next().unwrap())?;
            left = match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int(match op {
                    "*" => a * b,
                    "/" => a / b,
                    "%" => a % b,
                    _ => bail!("Unknown term"),
                }),
                (Value::Float(a), Value::Float(b)) => Value::Float(match op {
                    "*" => a * b,
                    "/" => a / b,
                    "%" => a % b,
                    _ => bail!("Unknown term"),
                }),
                _ => bail!("Type mismatch in term"),
            };
        }
        Ok(left)
    }

    fn eval_literal(&self, pair: Pair<'static, Rule>) -> Result<Value> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::string => {
                let s = inner.as_str().trim_matches(|c| c == '"' || c == '\'').to_string();
                let unescaped = self.unescape(&s);
                Ok(Value::String(unescaped))
            }
            Rule::int => Ok(Value::Int(inner.as_str().parse()?)),
            Rule::float => Ok(Value::Float(inner.as_str().parse()?)),
            Rule::bool => Ok(Value::Bool(inner.as_str().to_lowercase() == "true")),
            _ => bail!("Unknown literal"),
        }
    }

    fn unescape(&self, s: &str) -> String {
        let mut result = String::new();
        let mut chars = s.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '\\' {
                if let Some(next) = chars.next() {
                    result.push(next);
                }
            } else {
                result.push(c);
            }
        }
        result
    }

    fn eval_as_bool(&self, value: Value) -> Result<bool> {
        match value {
            Value::Bool(b) => Ok(b),
            Value::Int(n) => Ok(n != 0),
            Value::Float(f) => Ok(f != 0.0),
            Value::String(s) => Ok(!s.is_empty()),
            _ => bail!("Cannot convert to bool"),
        }
    }

    fn eval_as_int(&self, value: Value) -> Result<i64> {
        match value {
            Value::Int(n) => Ok(n),
            Value::Float(f) => Ok(f as i64),
            _ => bail!("Cannot convert to int"),
        }
    }
}
