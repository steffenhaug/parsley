use parsley::Alphabet;

#[allow(dead_code)]
#[derive(Alphabet)]
#[grammar("parsley-example/src/grammar.parsley")]
enum Sym {
    #[terminal("+")] Plus,
    #[terminal("-")] Minus,
    #[terminal("n")] Integer,
}

fn main() {
    println!("{}", parsley_gen::LR_TABLE);
}
