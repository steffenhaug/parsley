use parsley::alphabet::Alphabet;
use parsley_derive::{grammar, Alphabet};

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

grammar! {
    type Sym

    start E : E "+" T
            | T

    match T : T "*" F
            | F

    match F : "(" E ")"
            | n
}

fn main() {
    println!("Look at the proc macro output!");
    dbg!(Sym::Plus.id());
}
