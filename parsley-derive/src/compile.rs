use parsley_util::bnf::bnf_parser::{semantic_analysis, BnfParser};
use parsley_util::bnf::{
    Grammar,
    Symbol::{self, *},
};
use parsley_util::lr;
use parsley_util::parser::Parser;
use proc_macro2::TokenStream;
use quote::quote;

use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
    pub static ref TERMINALS: Mutex<HashMap<Symbol, usize>> = Mutex::new(HashMap::new());
}

pub fn parse_grammar(src: String) -> Grammar {
    let ast = BnfParser::from_src(&src)
        .parse()
        .expect("failed to parse grammar");

    semantic_analysis(ast).expect("semantic error in grammar")
}

pub fn compile_lr_table(src: String) -> TokenStream {
    let grammar = parse_grammar(src);

    // This is the set the Dragon Book calls C.
    let canon_item_sets = grammar.canonical_lr0_items();

    // We need the FOLLOW-sets of the grammar.
    let fst = grammar.first_sets();
    let fol = grammar.follow_sets(&fst);

    // Dimensions of the LR table.
    let m = grammar.terminals.len();
    let n = grammar.nonterminals.len();
    let k = canon_item_sets.len();

    // Acquire the symbol table, and insert dollar as the Mth symbol.
    // Note: A grammar with M terminal symbol thus requires table-
    // dimension M + 1.
    let mut symtab = TERMINALS.lock().unwrap();
    symtab.insert(Dollar, m);

    // Create action- and goto- tables to hold the entries while constructing.
    let mut action: Vec<Vec<_>> = (0..k)
        // Create M + 1 `None`-entries for each state.
        .map(|_| (0..m + 1).map(|_| None).collect())
        .collect();

    let mut goto: Vec<Vec<_>> = (0..k)
        // Create N `None`-entries for each state.
        .map(|_| (0..n).map(|_| None).collect())
        .collect();

    // Until now, we have operated on Set-structures without a defined ordering,
    // but in order to construct the table we need to impose an arbitrary ordering.
    // Because of this, the table is a bit unpredictable, which might be a weakness.
    // Note that the terminals already had an order imposed by deriving `Alphabet`.
    let canon_item_sets: Vec<_> = canon_item_sets.into_iter().collect();
    let nonterminals: Vec<_> = grammar.nonterminals.iter().collect();

    // Compile the LR table! (Finally)
    // Restating the definition in the book with some notes on how things are
    // calculated imperatively:
    // 1. Construct collection of canonical LR item sets is already done.
    //
    // 2. State i is constructed from I_i = ith canonical LR item set.
    // The parsing actions for state i is determined as follows:
    //  a) [A -> α · a β] in I_i and GOTO(I_i, a) = I_j
    //      => action[i][symtab[a]] := shift j
    //     I.e., if the LR item has a terminal after the dot, we calculate
    //     its GOTO-set. This will correspond to some LR item set I_j.
    //     We can find this set with a linear search; it isn't fantastic,
    //     but it lets us follow the notation of the book closely.
    //
    //  b) [A -> α ·] in I_i
    //      => forall a in FOLLOW(A), action[i][symtab[a]] = reduce A -> α
    //     I.e., if there is a reducing item [A -> α ·] in I_i, we calculate
    //     FOLLOW(A), and for all its members update the action table entry.
    //
    //  c) If [S' -> S ·] in I_i => action[i][symtab[$]] := accept
    //     This one is pretty straight forward; just check if the accepting
    //     LR item is in I_i.
    //
    //  3. The goto-transitions for state i are constructed for all
    //     non-terminals A using the rule
    //       GOTO(I_i, nonterminals[k]) = I_j => goto[i][k] := goto j
    //     Note the difference between GOTO the function and goto the
    //     table of actions! Again, this is simple: For every non-
    //     terminal in the language, check if the GOTO is a canonical
    //     LR item set, and if so update the goto table entry.
    //
    //  4. All remaining entries indicate error.
    //     This is simple: We just replace alle None-values in the table.
    //
    //  5. The automaton starts in the state containing [S' -> · S].
    //     Once again, simlpe. Just look for this item.
    //
    //  Because of Rust naming-convention, I_i and I_j will be denoted i_i
    //  and i_j. Pretty horrible names, but we are following the notation.

    for (i, i_i) in canon_item_sets.iter().enumerate() {
        // 2.a) Find all LR-items with a terminal after the dot.
        for item in i_i.iter().filter(|item| item.has_terminal_after_dot()) {
            // Given an item with a terminal after the dot, apply rule 2.a:
            let a = item.after_dot_unchecked();
            let go = grammar.goto(i_i, a);

            if let Some(j) = canon_item_sets
                .iter()
                // Find the next state i_j, where GOTO(i_i, a) == i_j.
                .position(|i_j| go == *i_j)
            {
                // action[i][symtab[a]] := shift j
                let conflict = action[i][symtab[a]].replace(quote!(Action::Shift(#j)));
                assert!(conflict.is_none());
            }
        }

        for item in i_i
            .iter()
            // 2.b) Find all LR-items that can reduce except [S' -> S ·].
            .filter(|it| it.is_reducing())
            .filter(|it| it.production != grammar.augmented_start())
        {
            // Given a reducing LR-item, apply rule 2.b:
            // We have item = [A -> β ·] for some β.
            let a_nt = &item.production.symbol; // A (a non-terminal!)
            let fol_a = &fol[a_nt]; // FOLLOW(A)

            // We encode the reduce action slightly differently from the
            // book, because storing the recipe, which is a Vec of symbols,
            // i. e. heap-allocated, in a `const` table is unnecessary.
            // We only need to know how many symbols to pull from the stack,
            // and which symbol to replace it with.

            // |β| := the order of the recipe is the # of symbols we need
            // to remove from the stack when performing the reduction.
            let ord = item.production.recipe.len();

            // The index of the non-terminal A allows us to reconstruct it
            // when parsing to put on the stack to replace β.
            let sym = nonterminals.iter().position(|nt| *nt == a_nt);

            // For all a in FOLLOW(A), action[i][symtab[a]] = reduce A -> β.
            for a in fol_a {
                let conflict = action[i][symtab[a]].replace(quote!(Action::Reduce(#ord, #sym)));
                assert!(conflict.is_none());
            }
        }

        // 2.c) If [S' -> S ·] in I_i, action[i][symtab[$]] := accept
        if i_i.contains(&grammar.accept_item()) {
            let conflict = action[i][symtab[&Dollar]].replace(quote!(Action::Accept));
            assert!(conflict.is_none());
        }

        // 3. Compute goto-table.
        for (k, nt) in nonterminals.iter().enumerate() {
            let go = grammar.goto(i_i, nt); // GOTO(I_i, nt)

            if let Some(j) = canon_item_sets
                .iter()
                // Find the state I_j where GOTO(I_i, nt) == I_j.
                .position(|i_j| go == *i_j)
            {
                // goto[i][k] := goto j
                let conflict = goto[i][k].replace(quote!(Action::Goto(#j)));
                assert!(conflict.is_none());
            }
        }
    }

    // 4. Convert None to Error.
    for i in 0..k {
        for j in 0..m + 1 {
            if action[i][j].is_none() {
                action[i][j].replace(quote!(Action::Error));
            }
        }

        for j in 0..n {
            if goto[i][j].is_none() {
                goto[i][j].replace(quote!(Action::Error));
            }
        }
    }

    // 5. Find the start state.
    let start_state = canon_item_sets
        .iter()
        .position(|i_i| i_i.contains(&grammar.start_item()))
        .expect("could not find start item");

    // Code generation:
    // In this step, our table is translated into Rust code that will
    // define this table in the final binary. It defines a `const LrTable`,
    // which is generic over its dimensions K x (M + N), allowing us to
    // avoid any heap allocation; whatever size our table is, it will be
    // placable in the programs rodata segment, and there will be zero cost
    // of initialization at actual runtime.

    // Codegen `State`s for each state `i`.
    let states = (0..k).map(|i| {
        // Safe to unwrap, because we just replaced all `None`s.
        let a = action[i].iter().map(|opt| opt.as_ref().unwrap());
        let g = goto[i].iter().map(|opt| opt.as_ref().unwrap());

        // String representation of a set of LR items.
        // The kernel of the canonical LR item set is a good way to describe a state.
        let descr = lr::describe(&grammar.kernel(&canon_item_sets[i]));

        // Generate the code.
        quote! {
            State {
                state: #i,
                actions: [ #(#a),* ],
                gotos: [ #(#g),*],
                description: #descr
            }
        }
    });

    // Compute the reverse lookup of the symbol tables.
    // These are used to place &'static str representations of the symbols
    // in the parsing table, so we can create nonterminals to push to the
    // stack, and print nice error messages should we encounter errors.
    let nonterminal_names = (0..n).map(|i| nonterminals[i].to_string());
    let terminal_names = (0..m + 1).map(|i| {
        let sym = symtab.iter().find(|(_, v)| **v == i).unwrap().0;
        sym.to_string()
    });

    // Generate the code.
    // This emits a whole module to attempt to minimze the pollution of
    // the users namespace. There is still a theoretical chance of conflict,
    // but it is both unlikely and easily avoidable by the user, and probably
    // quite hard to avoid from our end.
    quote! {
        mod parsley_gen {
            use ::parsley::internals::lr::*;
            use ::parsley::internals::bnf::{self, Symbol::*};
            // Put the states into a Kx(M+1+N) LR table.
            pub const LR_TABLE: LrTable<#k, {#m+1}, #n> = LrTable {
                terminals: [ #(#terminal_names),* ],
                nonterminals: [ #(#nonterminal_names),* ],
                states: [ #(#states),* ],
                start_state: #start_state,
            };
        }
    }
}
