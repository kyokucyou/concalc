// Copyright (C) 2022 kyokucyou

use std::{
    collections::HashMap,
    error::Error,
    f64::consts::{E, PI},
    fmt::{self, Display, Formatter},
    io::{stdin, stdout, BufRead, Write},
    iter::from_fn,
    str::{Chars, FromStr},
};

#[derive(Debug)]
struct ParseError {
    msg: &'static str,
}

#[derive(Debug)]
enum Token {
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
}

type ParseResult = Result<f64, Box<dyn Error>>;

struct Parser<'a> {
    chars: Chars<'a>,
    tok_buf: Vec<Token>,
    var_map: &'a mut HashMap<String, f64>,
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
            | Token::Caret => true,
            _ => false,
        }
    }
}

impl ParseError {
    fn new(msg: &'static str) -> Self {
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
    fn new(inp: &'a str, environment: &'a mut HashMap<String, f64>) -> Self {
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

    fn parse_function(&mut self, name: &String) -> ParseResult {
        let mut params = Vec::new();
        loop {
            let tok = self.lex_some()?;
            match tok {
                Token::RParen => break,
                _ => self.tok_buf.push(tok),
            }
            let x = self.parse_expr()?;
            params.push(x);
            let tok = self.lex_some()?;
            match tok {
                Token::RParen => break,
                Token::Comma => {}
                _ => return Err(Box::new(ParseError::new("missing comma"))),
            }
        }
        let cnt = params.len();
        if cnt == 0 {
            return Err(Box::new(ParseError::new("no parameters provided")));
        }
        Ok(match name.as_str() {
            "log" if cnt == 1 => params[0].ln(),
            "log" if cnt == 2 => params[0].log(params[1]),
            "rad" => params[0] * PI / 180.0,
            "deg" => params[0] * 180.0 / PI,
            "sin" => params[0].sin(),
            "cos" => params[0].cos(),
            "tan" => params[0].tan(),
            _ => {
                return Err(Box::new(ParseError::new(
                    "unknown function or bad parameter count",
                )))
            }
        })
    }

    fn parse_primary(&mut self) -> ParseResult {
        let tok = self.lex_some()?;
        match tok {
            Token::Identifier(id) => {
                if let Some(t) = self.lex()? {
                    if let Token::LParen = t {
                        return self.parse_function(&id);
                    }
                    self.tok_buf.push(t);
                }
                if let Some(&x) = self.var_map.get(&id) {
                    Ok(x)
                } else {
                    Err(Box::new(ParseError::new("unknown variable")))
                }
            }
            Token::Number(x) => Ok(x),
            Token::LParen => {
                let x = self.parse_expr()?;
                match self.lex_some()? {
                    Token::RParen => {}
                    _ => return Err(Box::new(ParseError::new("missing ')'"))),
                }
                Ok(x)
            }
            Token::Minus => {
                let x = self.parse_primary()?;
                Ok(-x)
            }
            _ => Err(Box::new(ParseError::new("expected primary"))),
        }
    }

    fn parse_expr(&mut self) -> ParseResult {
        self.parse_expression(0)
    }

    fn parse_expression(&mut self, precedence: u32) -> ParseResult {
        let mut lhs = self.parse_primary()?;
        loop {
            match self.lex()? {
                Some(tok) => {
                    let prec = tok.precedence();
                    if !tok.is_operator() || prec < precedence {
                        self.tok_buf.push(tok);
                        break;
                    }
                    let rhs = self.parse_expression(prec + 1)?;
                    lhs = match tok {
                        Token::Plus => lhs + rhs,
                        Token::Minus => lhs - rhs,
                        Token::Asterisk => lhs * rhs,
                        Token::Slash => lhs / rhs,
                        Token::Caret => lhs.powf(rhs),
                        _ => panic!("internal parsing error"),
                    };
                }
                _ => break,
            }
        }
        Ok(lhs)
    }

    fn parse(&mut self) -> Result<Option<f64>, Box<dyn Error>> {
        Ok(match self.lex()? {
            Some(tok) => {
                match tok {
                    Token::Identifier(ref id) => match self.lex()? {
                        Some(Token::Equals) => {
                            let x = self.parse_expr()?;
                            self.var_map.insert(id.to_string(), x);
                            return Ok(Some(x));
                        }
                        Some(la) => {
                            self.tok_buf.push(la);
                        }
                        _ => {}
                    },
                    _ => {}
                }
                self.tok_buf.push(tok);
                Some(self.parse_expr()?)
            }
            _ => None,
        })
    }
}

fn create_environment() -> HashMap<String, f64> {
    let mut m = HashMap::new();
    m.insert("e".into(), E);
    m.insert("pi".into(), PI);
    m
}

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = stdin();
    let stdin = stdin.lock();
    let mut stdout = stdout();
    let mut lines = stdin.lines();
    let mut environment = create_environment();
    println!("Welcome to the console calculator! Enter \".quit\" to quit.");
    loop {
        print!("> ");
        stdout.flush()?;
        let line = match lines.next() {
            Some(l) => l?,
            _ => break,
        };
        match line.as_str() {
            ".env" => {
                for (k, v) in environment.iter() {
                    println!("\t{:-10} = {}", k, v);
                }
                continue;
            }
            ".quit" => break,
            _ => {}
        }
        let mut parser = Parser::new(&line, &mut environment);
        match parser.parse() {
            Ok(Some(x)) => {
                println!("Result: {}", x);
                if !parser.is_eof() {
                    println!("Warning: Superflous input is ignored.");
                }
            }
            Ok(_) => {}
            Err(e) => println!("Error: {}", e),
        }
    }
    println!("Goodbye!");
    Ok(())
}
