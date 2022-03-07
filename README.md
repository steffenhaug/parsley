# Parsley
LR parser generator.
The goal is not performance, but rather to follow the dragon book as
closely as possible as a pedagogical example of how LR tables are
constructed.

Basically:
Given an enum type implementing `Alphabet`, whose variants represent
symbols in the underlying alphabet, a BNF grammar file (or string), 
we want to generate an LR parsing table.
All the `Alphabet` trait needs to do is ensure we can find a correspondence
between the symbols of the language and terminals in our grammar, and this
can be implemented with a derive-macro (we are just making some big `match`es).

With some helper attributes `#[terminal(_____)]` on our enum variants,
we can auto-derive this correspondence.
So for example, using in combination with the brilliant Logos lexer generator,
you can specify your alphabets symbols with an `enum` like so:
```Rust
#[derive(Alphabet, Logos)]
enum Sym {
    #[terminal("n")]
    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    N(i32),
    #[terminal("+")] 
    #[token("+")]
    Plus,
    #[terminal("-")] 
    #[token("-")]
    Minus,
    #[error] 
    #[regex(r" \t\n", logos::skip)]
    Error,
}
```
indicating that your language has integers, plus symbols and minus symbols.
Together with a grammar like this:
```Rust
const GRAMMAR: &str = r"
S : T

T : T + T
  | T - T
  | N

N : n
";
```
And note that the symbols are not required to be valid rust identifiers:
They can not be completely arbitrary, but a lot of symbols are allowed
(regex is `[a-zA-Z!$\+\-\^&<*/'=?@>_~]+`).
