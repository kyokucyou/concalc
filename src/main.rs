// Copyright (C) 2022 kyokucyou

use crate::{Either::*, Identifier::*};
use serde::{Deserialize, Serialize};
use std::{
    clone::Clone,
    collections::HashMap,
    error::Error,
    f64::consts::{E, PI},
    fmt::{self, Display, Formatter},
    fs::File,
    io::{self, stdin, stdout, BufRead, BufReader, Write},
    iter::from_fn,
    str::{Chars, FromStr},
};

enum Either<L, R> {
    Left(L),
    Right(R),
}

#[derive(Debug)]
struct ParseError {
    msg: String,
}

#[derive(Debug, Serialize, Deserialize)]
enum Token {
    Call(String, Vec<Node>),
    Identifier(String),
    Number(f64),
    LParen,
    RParen,
    Comma,
    Equals,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Caret,
    Percent,
}

#[derive(Debug, Serialize, Deserialize)]
struct Node {
    tok: Token,
    children: Vec<Box<Node>>,
}

#[derive(Serialize, Deserialize)]
enum Identifier {
    Variable(f64),
    Function(Node, Vec<String>),
    #[serde(skip)]
    Builtin(Box<dyn Fn(Vec<f64>) -> f64>, usize),
    #[serde(skip)]
    BuiltinLazy(
        Box<
            dyn Fn(
                &Vec<Node>,
                &Vec<&Environment>,
            ) -> Result<f64, Box<dyn Error>>,
        >,
        usize,
    ),
}

type ParseResult = Result<f64, Box<dyn Error>>;
type NodeResult = Result<Node, Box<dyn Error>>;

type Environment = HashMap<String, Identifier>;

struct Parser<'a> {
    chars: Chars<'a>,
    tok_buf: Vec<Token>,
    var_map: &'a mut Environment,
    ch_buf: Option<char>,
}

impl Token {
    fn precedence(&self) -> u32 {
        match *self {
            Token::Plus | Token::Minus => 1,
            Token::Asterisk | Token::Slash => 2,
            Token::Caret => 3,
            _ => 0,
        }
    }

    fn is_operator(&self) -> bool {
        match *self {
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::Caret
            | Token::Percent => true,
            _ => false,
        }
    }
}

impl Node {
    fn new(tok: Token, left: Box<Node>, right: Box<Node>) -> Self {
        let mut v = Vec::new();
        v.push(left);
        v.push(right);
        Self { tok, children: v }
    }

    fn new_maybe(
        tok: Token,
        left: Option<Box<Node>>,
        right: Option<Box<Node>>,
    ) -> Self {
        let mut v = Vec::new();
        if let Some(n) = left {
            v.push(n);
        }
        if let Some(n) = right {
            v.push(n);
        }
        Self { tok, children: v }
    }

    fn from_tok(tok: Token) -> Self {
        Self::new_maybe(tok, None, None)
    }

    fn evaluate(&self, scopes: &Vec<&Environment>) -> Result<f64, String> {
        match &self.tok {
            Token::Call(id, args) => {
                let f = scopes
                    .iter()
                    .filter_map(|m| m.get(id))
                    .last()
                    .ok_or(format!("unknown function: '{}'", id))?;
                match f {
                    Function(f, arg_names) => {
                        let param_scope = arg_names
                            .iter()
                            .zip(args.iter().map(|a| {
                                a.evaluate(&scopes)
                                    .and_then(|v| Ok(Variable(v)))
                            }))
                            .map(|(a, b)| match b {
                                Ok(v) => Ok((a.clone(), v)),
                                _ => Err(format!(
                                    "error evaluating parameter '{}'",
                                    a
                                )),
                            })
                            .collect::<Result<_, _>>()?;
                        let mut scopes = scopes.clone();
                        scopes.push(&param_scope);
                        f.evaluate(&scopes)
                    }
                    Builtin(f, n_args) => {
                        if *n_args != 0 && args.len() < *n_args {
                            return Err(format!(
                                "not enough arguments to function {}()",
                                id
                            ));
                        }
                        let args = args
                            .into_iter()
                            .map(|a| a.evaluate(scopes))
                            .collect::<Result<_, _>>()?;
                        Ok(f(args))
                    }
                    BuiltinLazy(f, n_args) => {
                        if *n_args != 0 && args.len() < *n_args {
                            return Err(format!(
                                "not enough arguments to function {}()",
                                id
                            ));
                        }
                        let res = f(args, scopes);
                        match res {
                            Ok(r) => Ok(r),
                            Err(e) => Err(format!(
                                "error while evaluating lazy function: {}",
                                e
                            )),
                        }
                    }
                    _ => Err(format!("not a function: '{}'", id)),
                }
            }
            Token::Identifier(id) => Ok(*scopes
                .iter()
                .filter_map(|m| match m.get(id) {
                    Some(Variable(x)) => Some(x),
                    _ => None,
                })
                .last()
                .ok_or(format!("unknown variable: '{}'", id))?),
            Token::Number(n) => Ok(*n),
            Token::Minus if self.children.len() == 1 => {
                let lhs = self.children[0].evaluate(scopes)?;
                Ok(-lhs)
            }
            _ => {
                if self.children.len() != 2 {
                    return Err(
                        "invalid number of operands to binary operator"
                            .to_string(),
                    );
                }
                let lhs = self.children[0].evaluate(scopes)?;
                let rhs = self.children[1].evaluate(scopes)?;
                Ok(match self.tok {
                    Token::Plus => lhs + rhs,
                    Token::Minus => lhs - rhs,
                    Token::Asterisk => lhs * rhs,
                    Token::Slash => lhs / rhs,
                    Token::Percent => lhs % rhs,
                    Token::Caret => lhs.powf(rhs),
                    _ => return Err("invalid operator".to_string()),
                })
            }
        }
    }
}

impl ParseError {
    fn new(msg: &'static str) -> Self {
        Self {
            msg: msg.to_string(),
        }
    }

    fn from_string(msg: String) -> Self {
        Self { msg }
    }
}

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl<'a> Parser<'a> {
    fn new(inp: &'a str, environment: &'a mut Environment) -> Self {
        Self {
            chars: inp.chars(),
            tok_buf: Vec::new(),
            var_map: environment,
            ch_buf: None,
        }
    }

    fn is_eof(&mut self) -> bool {
        match self.tok_buf.pop() {
            Some(_) => false,
            _ => !matches!(self.lex(), Ok(Some(_))),
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        match self.ch_buf {
            some @ Some(_) => some,
            _ => {
                let next = self.chars.next();
                if let Some(_) = next {
                    self.ch_buf = next;
                    next
                } else {
                    None
                }
            }
        }
    }

    fn read_char(&mut self) -> Option<char> {
        match self.ch_buf {
            Some(ch) => {
                self.ch_buf = None;
                Some(ch)
            }
            _ => self.chars.next(),
        }
    }

    fn read_while<T>(&mut self, f: T) -> String
    where
        T: Fn(char) -> bool,
    {
        let mut last = None;
        let s = from_fn(|| {
            last = self.peek_char();
            match self.read_char() {
                Some(ch) if f(ch) => Some(ch),
                _ => None,
            }
        })
        .collect();
        match last {
            Some(_) => self.ch_buf = last,
            _ => {}
        }
        s
    }

    fn lex(&mut self) -> Result<Option<Token>, Box<dyn Error>> {
        if let Some(t) = self.tok_buf.pop() {
            return Ok(Some(t));
        }
        let ch = loop {
            match self.peek_char() {
                Some(c) => {
                    if !c.is_ascii_whitespace() {
                        break c;
                    }
                    self.read_char();
                }
                _ => return Ok(None),
            }
        };
        let tok = if ch.is_ascii_alphabetic() {
            let id = self.read_while(|c| c.is_ascii_alphanumeric());
            Token::Identifier(id)
        } else if ch.is_ascii_digit() || ch == '.' {
            let s = self.read_while(|c| c.is_ascii_digit() || c == '.');
            let n = f64::from_str(&s)?;
            Token::Number(n)
        } else {
            self.read_char();
            match ch {
                '(' => Token::LParen,
                ')' => Token::RParen,
                ',' => Token::Comma,
                '=' => Token::Equals,
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Asterisk,
                '/' => Token::Slash,
                '^' => Token::Caret,
                '%' => Token::Percent,
                _ => return Err(Box::new(ParseError::new("bad char"))),
            }
        };
        Ok(Some(tok))
    }

    fn lex_some(&mut self) -> Result<Token, Box<dyn Error>> {
        if let Some(tok) = self.lex()? {
            Ok(tok)
        } else {
            Err(Box::new(ParseError::new("unexpected EOF")))
        }
    }

    fn lex_record<'b>(
        &mut self,
        buf: &'b mut Vec<Token>,
    ) -> Result<&'b Token, Box<dyn Error>> {
        let tok = self.lex_some()?;
        buf.push(tok);
        return Ok(buf.last().unwrap());
    }

    fn evaluate(&self, n: &Node) -> ParseResult {
        let scopes = vec![&*self.var_map];
        Ok(n.evaluate(&scopes)
            .map_err(|msg| Box::new(ParseError::from_string(msg)))?)
    }

    fn parse_function_ast(&mut self, name: &String) -> NodeResult {
        let mut params = Vec::new();
        loop {
            let tok = self.lex_some()?;
            match tok {
                Token::RParen => break,
                _ => self.tok_buf.push(tok),
            }
            let x = self.parse_expr_ast()?;
            params.push(x);
            let tok = self.lex_some()?;
            match tok {
                Token::RParen => break,
                Token::Comma => {}
                _ => return Err(Box::new(ParseError::new("missing comma"))),
            }
        }
        Ok(Node::from_tok(Token::Call(name.clone(), params)))
    }

    fn parse_primary_ast(&mut self) -> NodeResult {
        let tok = self.lex_some()?;
        match tok {
            Token::Identifier(ref id) => {
                if let Some(t) = self.lex()? {
                    if let Token::LParen = t {
                        return self.parse_function_ast(id);
                    }
                    self.tok_buf.push(t);
                }
                Ok(Node::from_tok(tok))
            }
            Token::Number(_) => Ok(Node::from_tok(tok)),
            Token::LParen => {
                let x = self.parse_expr_ast()?;
                match self.lex_some()? {
                    Token::RParen => {}
                    _ => return Err(Box::new(ParseError::new("missing ')"))),
                }
                Ok(x)
            }
            Token::Minus => {
                let x = self.parse_primary_ast()?;
                Ok(Node::new_maybe(tok, Some(Box::new(x)), None))
            }
            _ => Err(Box::new(ParseError::new("expected primary"))),
        }
    }

    fn parse_expr_ast(&mut self) -> NodeResult {
        self.parse_expression_ast(0)
    }

    fn parse_expression_ast(&mut self, precedence: u32) -> NodeResult {
        let mut lhs = self.parse_primary_ast()?;
        loop {
            match self.lex()? {
                Some(tok) => {
                    let prec = tok.precedence();
                    if !tok.is_operator() || prec < precedence {
                        self.tok_buf.push(tok);
                        break;
                    }
                    let rhs = self.parse_expression_ast(prec + 1)?;
                    lhs = Node::new(tok, Box::new(lhs), Box::new(rhs));
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn try_parse_fn_decl(
        &mut self,
    ) -> Result<Either<Vec<Token>, Identifier>, Box<dyn Error>> {
        let mut args = Vec::new();
        let mut buf = Vec::new();
        loop {
            match self.lex_record(&mut buf)? {
                Token::Identifier(id) => args.push(id.to_string()),
                Token::RParen => break,
                _ => return Ok(Left(buf)),
            }
            match self.lex_record(&mut buf)? {
                Token::Comma => {}
                Token::RParen => break,
                _ => return Ok(Left(buf)),
            }
        }
        match self.lex()? {
            Some(Token::Equals) => {}
            Some(tok) => {
                buf.push(tok);
                return Ok(Left(buf));
            }
            _ => return Ok(Left(buf)),
        }
        let n = self.parse_expr_ast()?;
        Ok(Right(Function(n, args)))
    }

    fn parse_ast(&mut self) -> Result<Option<f64>, Box<dyn Error>> {
        Ok(match self.lex()? {
            Some(tok) => {
                match tok {
                    Token::Identifier(ref id) => match self.lex()? {
                        Some(Token::Equals) => {
                            let n = self.parse_expr_ast()?;
                            let x = self.evaluate(&n)?;
                            self.var_map.insert(id.to_string(), Variable(x));
                            return Ok(Some(x));
                        }
                        Some(Token::LParen) => {
                            match self.try_parse_fn_decl()? {
                                Left(buf) => self.tok_buf.append(
                                    &mut buf.into_iter().rev().collect(),
                                ),
                                Right(func) => {
                                    self.var_map.insert(id.to_string(), func);
                                    return Ok(None);
                                }
                            }
                            self.tok_buf.push(Token::LParen);
                        }
                        Some(la) => {
                            self.tok_buf.push(la);
                        }
                        _ => {}
                    },
                    _ => {}
                }
                self.tok_buf.push(tok);
                let n = self.parse_expr_ast()?;
                Some(self.evaluate(&n)?)
            }
            _ => None,
        })
    }
}

macro_rules! def_fn {
    ($env:ident, $name:literal, $n_args:literal, $expr:expr) => {
        $env.insert($name.into(), Builtin(Box::new($expr), $n_args));
    };
}

fn create_environment() -> Result<Environment, Box<dyn Error>> {
    let mut m = HashMap::new();
    m.insert("e".into(), Variable(E));
    m.insert("pi".into(), Variable(PI));
    def_fn!(m, "ln", 1, |v| v[0].ln());
    def_fn!(m, "log", 2, |v| v[0].log(v[1]));
    def_fn!(m, "rad", 1, |v| v[0] * PI / 180.0);
    def_fn!(m, "deg", 1, |v| v[0] * 180.0 / PI);
    def_fn!(m, "sin", 1, |v| v[0].sin());
    def_fn!(m, "cos", 1, |v| v[0].cos());
    def_fn!(m, "tan", 1, |v| v[0].tan());
    def_fn!(m, "min", 0, |v| v
        .iter()
        .fold(f64::INFINITY, |a, &b| { f64::min(a, b) }));
    def_fn!(m, "max", 0, |v| v
        .iter()
        .fold(f64::NEG_INFINITY, |a, &b| { f64::max(a, b) }));
    def_fn!(m, "le", 2, |v| {
        if v[0] <= v[1] {
            1.0
        } else {
            0.0
        }
    });
    def_fn!(m, "not", 1, |v| {
        if v[0] == 0.0 {
            1.0
        } else {
            0.0
        }
    });
    m.insert(
        "if".to_string(),
        BuiltinLazy(
            Box::new(|v, s| {
                Ok(if v[0].evaluate(s)? != 0.0 {
                    v[1].evaluate(s)?
                } else {
                    v[2].evaluate(s)?
                })
            }),
            3,
        ),
    );
    Ok(m)
}

fn clean_environment<'a>(
    env: &'a Environment,
) -> HashMap<&String, &Identifier> {
    env.iter()
        .filter(|(k, _)| *k != "ans")
        .filter(|(_, v)| match v {
            Builtin(..) | BuiltinLazy(..) => false,
            _ => true,
        })
        .collect()
}

fn read_file(
    filename: String,
    environment: &mut Environment,
) -> Result<(), Box<dyn Error>> {
    let file = File::open(filename)?;
    let reader = BufReader::new(file);
    for (n, line) in reader.lines().enumerate() {
        let line = line?;
        let mut p = Parser::new(&line, environment);
        match p.parse_ast() {
            Err(e) => {
                println!("Error in line {}", n + 1);
                return Err(e.into());
            }
            _ => {}
        }
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = stdin().lock();
    let mut stdout = stdout();
    let mut lines = stdin.lines();
    let mut environment = create_environment()?;
    let mut prompt = move |p| {
        print!("{}", p);
        stdout.flush()?;
        let name = lines.next();
        match name {
            Some(s) => Ok::<_, io::Error>(Some(s?)),
            _ => Ok(None),
        }
    };
    println!("Welcome to the console calculator! Enter \".quit\" to quit.");
    loop {
        let line = match prompt("> ")? {
            Some(l) => l,
            _ => break,
        };
        match line.as_str() {
            ".env" => {
                for (k, v) in environment.iter() {
                    match v {
                        Variable(v) => println!("\t{:-10} = {}", k, v),
                        Function(..) => println!("\t{:-10} = (function)", k),
                        Builtin(..) => {
                            println!("\t{:-10} = (built-in function)", k)
                        }
                        BuiltinLazy(..) => {
                            println!("\t{:-10} = (lazy built-in function)", k)
                        }
                    }
                }
                continue;
            }
            ".quit" => break,
            ".restore" => {
                environment = create_environment()?;
                println!("Environment restored.");
                continue;
            }
            ".source" => {
                if let Some(filename) = prompt("Filename? ")? {
                    match read_file(filename, &mut environment) {
                        Err(e) => println!("Error while reading file: {}", e),
                        _ => println!("File sourced successfully."),
                    }
                    continue;
                }
                break;
            }
            ".save" => {
                if let Some(filename) = prompt("Filename? ")? {
                    let env = clean_environment(&environment);
                    let json = serde_json::to_string_pretty(&env)?;
                    let mut file = File::create(filename)?;
                    writeln!(file, "{}", json)?;
                    continue;
                }
                break;
            }
            ".load" => {
                if let Some(filename) = prompt("Filename? ")? {
                    let file = File::open(filename)?;
                    let file = BufReader::new(file);
                    let env: Environment = serde_json::from_reader(file)?;
                    environment.extend(env);
                    continue;
                }
                break;
            }
            _ => {}
        }
        let mut parser = Parser::new(&line, &mut environment);
        match parser.parse_ast() {
            Ok(Some(x)) => {
                println!("Result: {}", x);
                parser.var_map.insert("ans".to_string(), Variable(x));
                if !parser.is_eof() {
                    println!("Warning: superfluous input is ignored.");
                }
            }
            Ok(_) => {}
            Err(e) => println!("Error: {}", e),
        }
    }
    println!("Goodbye!");
    Ok(())
}
