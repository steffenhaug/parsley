use logos::Logos;
use parsley::Alphabet;

#[allow(dead_code)]
#[derive(Alphabet, Logos, Debug)]
#[grammar("grammar.parsley")]
enum Tok {
    #[token("+")] // `token` is Logos lexer-related
    #[terminal("+")] // `terminal` is Parsley parser-related
    Plus,
    #[token("-")]
    #[terminal("-")]
    Minus,
    // Integer tokens.
    #[regex("[0-9]+", |x| x.slice().parse())]
    #[terminal("n")]
    Integer(i64),
    // Error type also handles skipping whitespace.
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

fn main() {
    let input = "1 + 123 - 73";
    let toks: Vec<Tok> = Tok::lexer(input).into_iter().collect();
    let parser = parsley_gen::LR_TABLE.parser(toks);
    println!("{}", parsley_gen::LR_TABLE);
    let _ = parser.trace().parse();
}
