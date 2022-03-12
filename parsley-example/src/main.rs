use parsley::{bnf, Alphabet, Grammar};

#[allow(dead_code)]
#[derive(Alphabet, PartialEq, Eq, Hash)]
enum Sym {
    #[terminal("+")]
    Plus,
    #[terminal("-")]
    Minus,
    #[terminal("n")]
    Integer,
}

const GRAMMAR: &str = r"
S : T

T : T + N
  | T - N
  | N

N : n
";

fn main() {
    let grammar: Grammar<Sym> = bnf::parse(GRAMMAR, "S").unwrap();
    println!("GRAMMAR");
    println!("{}", grammar);

    println!("FORST/FOLLOW sets:");
    let fst = grammar.first_sets();
    let fol = grammar.follow_sets(&fst);
    bnf::pretty_print_map(&fst, "FIRST");
    bnf::pretty_print_map(&fol, "FOLLOW");

    println!("LR items:");
    for lri in grammar.all_lr_items() {
        let closure = grammar.closure(&lri);
        print!("CLOSURE({}) = ", lri);
        bnf::pretty_print_set(&closure);
        println!();
    }
}
