use parsley::alphabet::Alphabet;
use parsley_derive::Alphabet;

#[allow(dead_code)]
#[derive(Alphabet)]
#[grammar("parsley-example/src/grammar.parsley")]
enum Sym {
    #[terminal("+")] Plus,
    #[terminal("-")] Minus,
    #[terminal("n")] Integer,
}

fn main() {
    println!("{}", lr_table_scope::LR_TABLE);
}
