use parsley::{bnf, Alphabet, Grammar, Parser};

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
    let grammar: Grammar<Sym> = bnf::parse(GRAMMAR, "S").unwrap();
    let fst = grammar.first_sets();
    let fol = grammar.follow_sets(&fst);
    println!("{}", grammar);
    bnf::pretty_print_map(&fst, "FIRST");
    bnf::pretty_print_map(&fol, "FOLLOW");
}
