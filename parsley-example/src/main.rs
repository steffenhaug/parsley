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

X : M B X'

X' : e X''
   |

X'' : X
    | B

B : b

M : m
";

fn main() {
    let ast = BnfParser::new(GRAMMAR).parse().unwrap();
    let grammar: Grammar<Sym> = bnf::semantic_analysis(ast, "S").unwrap();
    let fst = grammar.first_sets();
    println!("{}", grammar);
    println!("{:?}", grammar.follow_sets(&fst));
}
