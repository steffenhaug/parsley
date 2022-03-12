use parsley::{bnf, Alphabet, Grammar};

#[allow(dead_code)]
#[derive(Alphabet, PartialEq, Eq, Hash)]
enum Sym {
    #[terminal("+")]
    Plus,
    #[terminal("*")]
    Minus,
    #[terminal("n")]
    Integer,
    #[terminal("(")]
    ParOpen,
    #[terminal(")")]
    ParClose,
}

parsley::grammar! {
    type Sym;

    match E : E + T
            | T

    match T : T * F
            | F

    match F : ( E )
            | n
}

const GRAMMAR: &str = "
E : E + T
  | T

T : T * F
  | F

F : ( E )
  | n
";

fn main() {
    let grammar: Grammar<Sym> = bnf::parse(GRAMMAR, "E").unwrap();
    println!("GRAMMAR");
    println!("{}", grammar);

    println!("Canonical LR(0) items:");
    for set_of_lri in grammar.canonical_lr0_items() {
        bnf::pretty_print_set(&grammar.kernel(&set_of_lri));
        println!();
    }
}
