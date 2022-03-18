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
So for example, you can specify your alphabets symbols with an `enum` like so:
```Rust
#[derive(Alphabet)]
#[grammar("grammar.parsley")]
enum Sym {
    #[terminal("+")] Plus,
    #[terminal("-")] Minus,
    #[terminal("n")] Integer,
}
```
indicating that your language has integers, plus symbols and minus symbols.
Together with a grammar-file like this:
```
S : T

T : T + T
  | T - T
  | N

N : n
```
We can know that `+`, `-` and `n` are terminals, and we know that items
on the LHS of productions `S`, `T`, and `N` are non-terminals. So if the
left-over symbols in the grammar coincides with the terminals tagged on
the enum, we are good to go.
All this can be checked in the derive-macro.

Your grammar-symbol enum may of course also derive `Logos` from the brilliant
`logos`-crate to also automatically get a good lexer.

And note that the symbols are not required to be valid rust identifiers:
They can not be completely arbitrary, but a lot of symbols are allowed.

The LR table is compiled to (very ugly) Rust-code that includes the table
in its own module. Inspecting the compiled code can be done with `cargo expand`,
but is gonna be pretty annoying to read. The table can instaed be printed at
runtime, as it implements `fmt:Display`.
The output will look something like
```
Grammar rules:
(0) S -> T                    |β| = 1
(1) T -> T + N                |β| = 3
(2) T -> T - N                |β| = 3
(3) T -> N                    |β| = 1
(4) N -> n                    |β| = 1
State  Action               Goto            Kernel
       +    -    n    $     T    N    S     
   *0  ·    ·    S4   ·     G1   G2   G3    {S' -> · S}
    1  S5   S6   ·    R0    ·    ·    ·     {T -> T · - N, T -> T · + N, S -> T ·}
    2  R3   R3   ·    R3    ·    ·    ·     {T -> N ·}
    3  ·    ·    ·    ACC   ·    ·    ·     {S' -> S ·}
    4  R4   R4   ·    R4    ·    ·    ·     {N -> n ·}
    5  ·    ·    S4   ·     ·    G7   ·     {T -> T + · N}
    6  ·    ·    S4   ·     ·    G8   ·     {T -> T - · N}
    7  R1   R1   ·    R1    ·    ·    ·     {T -> T + N ·}
    8  R2   R2   ·    R2    ·    ·    ·     {T -> T - N ·}
```
Note that because of the use of un-ordered set types in the compilation, the
table can be a bit unpredictable.
