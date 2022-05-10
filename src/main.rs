use std::{
    collections::HashMap,
    error::Error,
    fmt::{self, Display, Formatter},
    io::{stdin, stdout, BufRead, Write},
    iter::Peekable,
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
    chars: Peekable<Chars<'a>>,
    tok_buf: Vec<Token>,
    var_map: HashMap<String, f64>,
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
            Token::Plus | Token::Minus | Token::Asterisk
                | Token::Slash | Token::Caret => true,
            _ => false,
        }
    }
}

impl ParseError {
    fn new(msg: &'static str) -> Self {
        Self {
            msg,
        }
    }
}

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl<'a> Parser<'a> {
    fn new(inp: &'a str) -> Self {
        Self {
            chars: inp.chars().peekable(),
            tok_buf: Vec::new(),
            var_map: HashMap::new(),
            ch_buf: None,
        }
    }

    fn lex(&mut self) -> Result<Option<Token>, Box<dyn Error>> {
        if let Some(t) = self.tok_buf.pop() {
            return Ok(Some(t));
        }
        let mut from_buf = false;
        Ok(Some(loop {
            let ch = match self.ch_buf {
                Some(c) => {
                    self.ch_buf = None;
                    if c.is_whitespace() {
                        continue;
                    }
                    from_buf = true;
                    c
                }
                _ => {
                    match self.chars.peek() {
                        Some(&c) => {
                            if c.is_whitespace() {
                                self.chars.next();
                                continue;
                            }
                            c
                        }
                        _ => return Ok(None),
                    }
                }
            };
            break if ch.is_ascii_alphabetic() {
                let id = self.chars.by_ref().take_while(|&c| {
                    self.ch_buf = Some(c);
                    c.is_ascii_alphanumeric()
                }).collect();
                Token::Identifier(id)
            } else if ch.is_ascii_digit() || ch == '.' {
                let s: String = self.chars.by_ref().take_while(|&c| {
                    self.ch_buf = Some(c);
                    c.is_ascii_digit() || c == '.'
                }).collect();
                let n = f64::from_str(&s)?;
                Token::Number(n)
            } else {
                if !from_buf {
                    self.chars.next();
                }
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
        }))
    }

    fn lex_some(&mut self) -> Result<Token, Box<dyn Error>> {
        if let Some(tok) = self.lex()? {
            Ok(tok)
        } else {
            Err(Box::new(ParseError::new("unexpected EOF")))
        }
    }

    fn parse_primary(&mut self) -> ParseResult {
        let tok = self.lex_some()?;
        match tok {
            Token::Identifier(id) => {
                if let Some(&x) = self.var_map.get(&id) {
                    Ok(x)
                } else {
                    Err(Box::new(ParseError::new("unknown variable")))
                }
            }
            Token::Number(x) => {
                Ok(x)
            }
            Token::LParen => {
                let x = self.parse_expr(0)?;
                match self.lex_some()? {
                    Token::RParen => {},
                    _ => return Err(Box::new(ParseError::new("no ')'")))
                }
                Ok(x)
            }
            Token::Minus => {
                let x = self.parse_expr(0)?;
                Ok(-x)
            }
            _ => Err(Box::new(ParseError::new("expected primary"))),
        }
    }

    fn parse_expr(&mut self, precedence: u32) -> ParseResult {
        let mut lhs = self.parse_primary()?;
        loop {
            match self.lex()? {
                Some(tok) => {
                    let prec = tok.precedence();
                    if !tok.is_operator() || prec < precedence {
                        self.tok_buf.push(tok);
                        break;
                    }
                    let rhs = self.parse_expr(prec + 1)?;
                    lhs = match tok {
                        Token::Plus => lhs + rhs,
                        Token::Minus => lhs - rhs,
                        Token::Asterisk => lhs * rhs,
                        Token::Slash => lhs / rhs,
                        _ => panic!("internal parsing error")
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
                    Token::Identifier(ref id) => {
                        match self.lex()? {
                            Some(Token::Equals) => {
                                let x = self.parse_expr(0)?;
                                self.var_map.insert(id.to_string(), x);
                                return Ok(Some(x));
                            }
                            Some(la) => {
                                self.tok_buf.push(la);
                            }
                            _ => {},
                        }
                    }
                    _ => {},
                }
                self.tok_buf.push(tok);
                Some(self.parse_expr(0)?)
            }
            _ => None,
        })
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = stdin();
    let stdin = stdin.lock();
    let mut stdout = stdout();
    let mut lines = stdin.lines();
    loop {
        print!("> ");
        stdout.flush()?;
        let line = match lines.next() {
            Some(l) => l?,
            _ => break,
        };
        if line == "quit" {
            break;
        }
        let mut parser = Parser::new(&line);
        while let Ok(Some(tok)) = parser.lex() {
            println!("{:?}", tok)
        }
        // if let Some(x) = parser.parse()? {
        //     println!("Result: {}", x);
        // }
    }
    Ok(())
}
