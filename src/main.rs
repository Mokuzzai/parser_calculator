
#[derive(Clone)]
struct Parser<'a> {
	span: &'a str,
	cursor: usize,
}

const _: () = {
	use core::fmt::*;

	impl<'a> Debug for Parser<'a> {
		fn fmt(&self, f: &mut Formatter) -> Result {
			f.debug_struct("Parser")
				.field("matched", &self.matched())
				.field("rest", &self.rest())
				.finish()
		}
	}
};

#[derive(Debug)]
enum Error {
	Unexpected(char),
	Eoi,
}

impl<'a> Parser<'a> {
	fn new(span: &'a str) -> Self {
		Self { span, cursor: 0 }
	}
	fn matched(&self) -> &'a str {
		&self.span[..self.cursor]
	}
	fn rest(&self) -> &'a str {
		&self.span[self.cursor..]
	}
	fn peek(&self) -> Option<char> {
		self.rest().chars().next()
	}
	fn seek_with(&mut self, f: impl FnOnce(char) -> bool) -> Result<char, Error> {
		let next = self.peek().ok_or(Error::Eoi)?;

		if f(next) {
			self.cursor += 1;

			Ok(next)
		} else {
			Err(Error::Unexpected(next))
		}
	}
	fn flat_with(&mut self, f: impl FnOnce(char) -> bool) -> Result<char, Error> {
		self.whitespace();
		let ret = self.seek_with(f)?;
		self.whitespace();

		Ok(ret)
	}
	fn float_matches(&mut self, p: char) -> Result<char, Error> {
		self.flat_with(|c| c == p)
	}
	fn whitespace(&mut self) {
		while let Ok(_) = self.seek_with(char::is_whitespace) {}
	}
	// discards all matched tokens
	fn discard(&mut self) {
		*self = Self::new(self.rest())
	}
}

/// # Syntax
/// i32 = '-'? ~ ('0'..'9')+
/// add = expr ~ '+' ~ expr_or_binop
/// sub = expr ~ '-' ~ expr_or_binop
/// parens = '(' ~ expr_or_binop ~ ')'
/// expr_or_binop = add | sub | expr
/// expr = { i32 | parens }

impl<'a> Astok {
	fn i32(src: &'a str) -> Result<(Self, &'a str), Error> {
		let mut parser = Parser::new(src);

		parser.whitespace();
		parser.discard();

		// optional '-'
		// if src is empty we return with an error
		// if let Err(Error::Eoi) = parser.seek_matches('-') {
		//    return Err(Error::Eoi);
		// }

		// mandatory first digit
		let _ = parser.seek_with(char::is_numeric)?;

		// repeating digits
		while let Ok(_) = parser.seek_with(char::is_numeric) {}

		let num = parser
			.matched()
			.parse()
			.expect("input should be a valid `i32`");

		Ok((Self::Node(Token::I32(num)), parser.rest()))
	}
	fn plus(src: &'a str) -> Result<(Self, &'a str), Error> {
		let mut parser = Parser::new(src);

		parser.float_matches('+')?;

		Ok((Self::Node(Token::Plus), parser.rest()))
	}
	fn minus(src: &'a str) -> Result<(Self, &'a str), Error> {
		let mut parser = Parser::new(src);

		parser.float_matches('-')?;

		Ok((Self::Node(Token::Minus), parser.rest()))
	}
	fn asterisk(src: &'a str) -> Result<(Self, &'a str), Error> {
		let mut parser = Parser::new(src);

		parser.float_matches('*')?;

		Ok((Self::Node(Token::Asterisk), parser.rest()))
	}
	fn slash(src: &'a str) -> Result<(Self, &'a str), Error> {
		let mut parser = Parser::new(src);

		parser.float_matches('/')?;

		Ok((Self::Node(Token::Slash), parser.rest()))
	}
	fn parens(src: &'a str) -> Result<(Self, &'a str), Error> {
		let mut parser = Parser::new(src);

		parser.float_matches('(')?;

		let (tokens, rest) = Self::tokens(parser.rest())?;

		let mut parser = Parser::new(rest);

		parser.float_matches(')')?;

		Ok((tokens, parser.rest()))
	}
	fn token(src: &'a str) -> Result<(Self, &'a str), Error> {
		Self::i32(src)
			.or_else(|_| Self::plus(src))
			.or_else(|_| Self::minus(src))
			.or_else(|_| Self::asterisk(src))
			.or_else(|_| Self::slash(src))
			.or_else(|_| Self::parens(src))
	}
	fn tokens(src: &'a str) -> Result<(Self, &'a str), Error> {
		let (a, mut rest) = Self::token(src)?;

		let mut tokens = vec![a];

		while let Ok((next, cont)) = Self::token(rest) {
			tokens.push(next);

			rest = cont;
		}

		Ok((Self::Tree(Astree::new(tokens)), rest))
	}
}

#[derive(Debug, Clone)]
enum Token {
	I32(i32),
	Plus,
	Minus,
	Asterisk,
	Slash
}

impl Token {
	fn infix_binding_power(&self) -> ([u8; 2], fn(Box<Pair>) -> Tree) {
		match *self {
			Self::Plus => ([1, 2], Tree::Add),
			Self::Minus => ([3, 4], Tree::Sub),
			Self::Asterisk => ([5, 6], Tree::Mul),
			Self::Slash => ([5, 6], Tree::Div),
			ref t => panic!("invalid op: {:?}", t),
		}
	}
}

#[derive(Debug, Clone)]
struct Astree {
	tokens: Vec<Astok>,
}

impl Astree {
	fn new(mut tokens: Vec<Astok>) -> Self {
		tokens.reverse();
		Self { tokens }
	}
	fn seek(&mut self) -> Option<Astok> {
		self.tokens.pop()
	}
	fn peek(&mut self) -> Option<&Astok> {
		self.tokens.last()
	}
	fn tree(&mut self, min_bp: u8) -> Tree {
		let mut lhs = match self.seek() {
			Some(Astok::Node(Token::Minus)) => {
				let rhs = self.tree(5);

				Tree::Neg(Box::new(rhs))
			},
			Some(ref mut astok) => astok.tree(),
			ref t => panic!("bad token: {:?}", t),
		};

		loop {
			match self.peek() {
				None => break,
				Some(Astok::Node(token)) => {
					let ([lhs_bp, rhs_bp], f)= token.infix_binding_power();

					if lhs_bp < min_bp {
						break;
					}

					let _ = self.seek();

					let rhs = self.tree(rhs_bp);

					lhs = f(Box::new(Pair { lhs, rhs }));
				},
				ref t => panic!("bad token: {:?}", t),
			}

		}

		lhs
	}
}

#[derive(Debug, Clone)]
enum Astok {
	Node(Token),
	Tree(Astree),
}

impl Astok {
	fn tree(&mut self) -> Tree {
		match *self {
			Self::Tree(ref mut astree) => astree.tree(0),
			Self::Node(Token::I32(n)) => Tree::I32(n),
			ref t => panic!("bad token: {:?}", t),
		}
	}
}

#[derive(Debug, Clone)]
struct Pair {
	lhs: Tree,
	rhs: Tree,
}

#[derive(Debug, Clone)]
enum Tree {
	I32(i32),
	Neg(Box<Self>),
	Add(Box<Pair>),
	Sub(Box<Pair>),
	Mul(Box<Pair>),
	Div(Box<Pair>),
}


const _: () = {
	use core::fmt::*;

	impl<'a> Display for Tree {
		fn fmt(&self, f: &mut Formatter) -> Result {
			match self {
				Self::I32(n) => write!(f, "{}", n),
				Self::Neg(n) => write!(f, "-({})", n),
				Self::Add(pair) => write!(f, "({} + {})", pair.lhs, pair.rhs),
				Self::Sub(pair) => write!(f, "({} - {})", pair.lhs, pair.rhs),
				Self::Mul(pair) => write!(f, "({} * {})", pair.lhs, pair.rhs),
				Self::Div(pair) => write!(f, "({} / {})", pair.lhs, pair.rhs),
			}
		}
	}
};

impl Tree {
	fn eval(self) -> i32 {
		match self {
			Self::I32(n) => n,
			Self::Neg(unit) => -unit.eval(),
			Self::Add(pair) => pair.lhs.eval() + pair.rhs.eval(),
			Self::Sub(pair) => pair.lhs.eval() - pair.rhs.eval(),
			Self::Mul(pair) => pair.lhs.eval() * pair.rhs.eval(),
			Self::Div(pair) => pair.lhs.eval() / pair.rhs.eval(),
		}
	}
}

#[test]
fn tests() {
	macro_rules! test {
		($e:expr) => {
			let mut ast = Astok::tokens(stringify!($e)).unwrap();
			let tree = ast.0.tree();
			let result = tree.clone().eval();
			let expected = $e;

			if result != expected {
				eprintln!("ast: {:#?}", ast);
				eprintln!("tree: {:}", tree);
				panic!("{} != {}", result, expected);
			}
		}
	}

	test! { 2-1 + 2 };
	test! { -1   +  10  -   ( -10  +  10   ) };
	test! { 3 + 2 * 2 }
	test! { 1+2*5/3+6/4*2 }
}

fn main() -> Result<(), Error> {
	let input: String = std::env::args().skip(1).collect();

	let mut ast = Astok::tokens(&input)?;
	let tree = ast.0.tree();
	let result = tree.clone().eval();

	println!("{}", result);

	Ok(())
}
