use parsley::{bnf, Alphabet, BnfParser, Grammar, Parser};

#[allow(dead_code)]
#[derive(Alphabet, PartialEq, Eq, Hash)]
enum Sym {
    #[terminal("w")]
    Where,
    #[terminal("m")]
    Mask,
    #[terminal("b")]
    Block,
    #[terminal("e")]
    Elsewhere,
    #[terminal("z")]
    End,
}

const GRAMMAR: &str = r"
S : w X z

X : m b X'

X' : e X''
   |

P : p

X'' : X
    | b
";

fn main() {
    let ast = BnfParser::new(GRAMMAR).parse().unwrap();
    let grammar: Grammar<Sym> = bnf::semantic_analysis(ast).unwrap();
    println!("{}", grammar);
    println!("{:?}", grammar.first_sets());
}
