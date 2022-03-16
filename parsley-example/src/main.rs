use parsley::alphabet::Alphabet;
use parsley_derive::Alphabet;

#[allow(dead_code)]
#[derive(Alphabet)]
#[grammar("src/grammar.parsley")]
enum Sym {
    #[terminal("+")] Plus,
    #[terminal("-")] Minus,
    #[terminal("n")] Integer,
}

fn main() {
    println!("Look at the proc macro output!");
    dbg!(Sym::Plus.id());
}
