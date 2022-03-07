use parsley::{Alphabet, Parser, BnfParser, Grammar, bnf};
use logos::Logos;

#[derive(Alphabet, Logos)]
enum Sym {
    #[terminal("n")]
    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    N(i32),
    #[terminal("+")] 
    #[token("+")]
    Plus,
    #[terminal("-")] 
    #[token("-")]
    Minus,
    #[error] 
    #[regex(r" \t\n", logos::skip)]
    Error,
}

const GRAMMAR: &str = r"
    S : T

    T : T + T
      | T - T
      | N

    N : n
";


fn main() {
    let ast = BnfParser::new(GRAMMAR).parse().unwrap();
    let grammar: Grammar<Sym> = bnf::semantic_analysis(ast).unwrap();
    println!("{}", grammar);
}
