extern crate proc_macro;
#[macro_use]
extern crate lazy_static;

// TODO: Modularise the compiler.

use parsley::bnf::bnf_parser::{semantic_analysis, BnfParser};
use parsley::bnf::Symbol::{self, *};
use parsley::lr;
use parsley::parser::Parser;
use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;

use std::collections::HashMap;
use std::sync::Mutex;
use syn::{Attribute, Data, DataEnum, DeriveInput, Fields, Ident, Variant};

lazy_static! {
    // Database of terminals in the language.
    static ref TERMINALS: Mutex<HashMap<Symbol, usize>> = Mutex::new(HashMap::new());
}

fn find_attributes_named<'a, A>(attrs: A, name: &str) -> Vec<&'a Attribute>
where
    A: IntoIterator<Item = &'a Attribute>,
{
    attrs
        .into_iter()
        .filter(|attr| attr.path.is_ident(name))
        .collect()
}

fn extract_variant_info(v: &mut Variant) -> Option<(String, Ident, Fields)> {
    let Variant {
        attrs,
        ident,
        fields,
        ..
    } = v;

    for attr in find_attributes_named(&*attrs, "terminal") {
        // Steal the tokens from the token stream
        let tokens = attr.tokens.clone();

        // First token in the group should be a literal...
        // This is ugly.
        if let Some(TokenTree::Group(g)) = tokens.into_iter().next() {
            if let Some(TokenTree::Literal(l)) = g.stream().into_iter().next() {
                let quoted = l.to_string();
                return Some((
                    quoted[1..quoted.len() - 1].to_owned(),
                    ident.clone(),
                    fields.clone(),
                ));
            }
        }
    }

    // No terminals was found.
    None
}

#[proc_macro_derive(Alphabet, attributes(terminal, grammar))]
pub fn alphabet_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as DeriveInput);

    let DeriveInput {
        ident, data, attrs, ..
    } = ast;

    // Get the name of the grammar file from the first grammar("___") attribute.
    let grammar_attrs = find_attributes_named(&attrs, "grammar");
    let grammar_file: Option<String> = grammar_attrs.first().map(|g| {
        g.parse_args::<syn::LitStr>()
            // If there _was_ some literal, but it's not a string, we panic.
            // If there was _no_ literal, `.map` would skip this with the None variant.
            .expect("grammar file must be a string literal")
            .value()
    });

    // This is now something sensible:
    //     dbg!(&grammar_file);

    // Extract the relevant info about the enum variants from the tree.
    // (terminals in the grammar, identifier and fields)
    let variants: Vec<(String, Ident, Fields)> = if let Data::Enum(DataEnum { variants, .. }) = data
    {
        variants
            .into_iter()
            .filter_map(|mut v| extract_variant_info(&mut v))
            .collect()
    } else {
        panic!("Only enums can derive Alphabet!");
    };

    let mut symtab = TERMINALS.lock().unwrap();

    for (seq, (literal, _, _)) in variants.iter().enumerate() {
        // Unquote the string-literals value and add it to the symbol database.
        symtab.insert(Terminal(literal.to_string().into()), seq);
    }

    let match_arms: proc_macro2::TokenStream = variants
        .iter()
        .map(|(lit, id, fs)| {
            let n = symtab[&Terminal(lit.to_owned().into())];
            match fs {
                Fields::Unit => quote! { #ident :: #id     => #n, },
                Fields::Unnamed(_) => quote! { #ident :: #id (_) => #n, },
                Fields::Named(_) => panic!("Named fields are supported!"),
            }
        })
        .collect();

    // Now we no longer need the symtab.
    drop(symtab);

    let lrtab_module = grammar_file.map_or(quote! {}, |file| {
        let src = std::fs::read_to_string(file).expect("could not read grammar file");
        compile_lr_table(src)
    });

    let expanded = quote! {
        #[automatically_derived]
        impl Alphabet for #ident {
            fn id(&self) -> usize {
                match self {
                    #match_arms
                }
            }
        }

        #lrtab_module
    };

    expanded.into()
}

fn compile_lr_table(src: String) -> proc_macro2::TokenStream {
    let ast = BnfParser::from_src(&src)
        .parse()
        .expect("failed to parse grammar");

    let grammar = semantic_analysis(ast).expect("semantic error in grammar");
    let fst = grammar.first_sets();
    let fol = grammar.follow_sets(&fst);
    let c = grammar.canonical_lr0_items();

    // Now that we are ready to compile the table, we need the symtab.
    let mut symtab = TERMINALS.lock().unwrap();

    let m = grammar.terminals.len();
    let n = grammar.nonterminals.len();
    let k = c.len();

    // We have K states, each with M+1 actions and N gotos.
    // We have a column for $, even though it is not a real terminal.
    // The terminals' IDs range from [0, m-1], so the ID m is free, and
    // corresponds to the right-most column in the action table.
    symtab.insert(Dollar, m);

    // Note that these tables are tables of _quoted Rust code_! After computing
    // the table (of code) the table will be compiled into code compiling into
    // a LR parsing table, so we can have the table completely in the programs
    // data segment of the program so we get good performance.

    // todo: replace with options(tokenstream) and leave none, so we can
    // check for confclicts. cant compare tokenstreams, so doesnt work atm
    let mut action: Vec<Vec<_>> = (0..k)
        .map(|_| (0..m + 1).map(|_| quote!(Action::Error)).collect())
        .collect();

    let mut goto: Vec<Vec<_>> = (0..k)
        .map(|_| (0..n).map(|_| quote!(Action::Error)).collect())
        .collect();

    // So for example, now something like this is valid:
    //     act[0][symtab["n"]] = quote!(Action::Shift(5));

    // Now, one last thing: Until now we operated with our LR item set as
    // a _actual_ set (a HashSet) in which the order is not defined, so
    // to give meaningful numbers to our states, we will convert it into
    // a Vector!
    let c = c.into_iter().collect::<Vec<_>>();
    // similarly, it will be useful to impose a defined ordering on the
    // Non-terminals. Terminals already have an ordering imposed on them
    // given by the derivation of `Alphabet`.
    let nts = grammar.nonterminals.iter().clone().collect::<Vec<_>>();

    // Compile the LR table! (Finally)
    // Restating the definition in the book with some notes on how things are
    // calculated imperatively:
    // 1. Cosntruct `clri` = collection of canonical lr items is already done.
    //
    // 2. State `i` is constructed from `clri[i]`.
    // The parsing actions for state `i` is determined as follows:
    //  a) A -> α · a β in clri[i] and GOTO(clri[i], a) = clri[j]
    //      => action[i][symtab[a]] := shift j
    //     i. e., if the symbol after the dot is a terminal, calculate the
    //     GOTO item-set. If this set is in clri at index j, put shift j
    //     into the action table. This suggests a linear search through
    //     clri for every terminal after a dot in clri[i]. This can probably
    //     be done more efficiently, but in the interest of following the
    //     books notation, we will do a linear search.
    //
    //  b) A -> α · in clri[i]
    //      => forall a in FOLLOW(A), action[i, symtab[a]] = reduce
    //     i. e., if a production is reducing in clri[i], calculate
    //     its follow set, and for all symbols here, put in a reduce
    //     action. A != S'. a may be $.
    //
    //  c) If S' -> S · in clri[i] => action[i][symtab[$]] := accept
    //
    //  3. The goto-transitions for state i are constructed for all
    //     non-terminals A using the rule
    //       GOTO(I_i, nts[k]) = I_j => goto[i][k] := goto j
    //     note the difference between GOTO the function and goto the
    //     table of actions.
    //
    //  4. All remaining entries indicate error.
    //
    //  5. The automaton starts in the state containing S' -> · S.

    for (i, i_i) in c.iter().enumerate() {
        // 2.a) Find all LR-items with a terminal after the dot.
        for lri in i_i.iter().filter(|item| item.has_terminal_after_dot()) {
            let a = lri.after_dot_unchecked(); // a
            let goto = grammar.goto(i_i, a); // GOTO(I_i, a)

            // Find the next state i_j, where GOTO(i_i, a) == i_j.
            if let Some(j) = c.iter().position(|i_j| goto == *i_j) {
                action[i][symtab[a]] = quote!(Action::Shift(#j));
            }
        }

        // 2.b) Find all LR-items that can reduce.
        for lri in i_i
            .iter()
            .filter(|it| it.is_reducing())
            .filter(|it| it.production != grammar.augmented_start())
        {
            // lri = A -> β ·
            let a = &lri.production.symbol; // A
            let fol_a = &fol[a]; // FOLLOW(A)
            let ord_pr = lri.production.recipe.len(); // |β| (# syms to pop after reducing)
            let sym = nts.iter().position(|name| *name == a);

            // For all x in FOLLOW(A), act[i, x] = Reduce A -> β.
            // Note that the reduction is encoded not by reduction #, but by
            // # of syms to pop off stack, and the sym to replace it.
            for x in fol_a {
                action[i][symtab[x]] = quote!(Action::Reduce(#ord_pr, #sym));
            }
        }

        // 2.c) If S' -> S · in I_i, action[i][symtab[$]] := accept
        if i_i.contains(&grammar.accept_item()) {
            action[i][symtab[&Dollar]] = quote!(Action::Accept);
        }

        // 3.
        for (k, nt) in nts.iter().enumerate() {
            let go = grammar.goto(i_i, nt); // GOTO(I_i, nts[j])

            // Find the state I_j, where GOTO(I_i, nts[j]) == I_j.
            // Note: We don't care about the # of this LR-item, merely
            // that it exists.
            if let Some(j) = c.iter().position(|i_j| go == *i_j) {
                goto[i][k] = quote!(Action::Goto(#j));
            }
        }

        // 4. Convert None to Error (TODO after conflict checking)

    }

    let start_state = c.iter().position(|i_i| {
        i_i.contains(&grammar.start_item())
    }).expect("could not find start item");




    // Write `State`s for each state `i`.
    let states = (0..k).map(|i| {
        let (a, g) = (&action[i], &goto[i]);
        // String representation of a set of LR items
        let descr = lr::describe(&grammar.kernel(&c[i]));
        quote! {
            State { state: #i, actions: [ #(#a),* ], gotos: [ #(#g),*], description: #descr }
        }
    });

    // Compute the reverse lookup of the symbol table to describe terminals.
    let t_descrs = (0..m + 1).map(|i| {
        let sym = symtab.iter().find(|(_, v)| **v == i).unwrap().0;
        sym.to_string()
    });

    let nt_descrs = (0..n).map(|i| {
        nts[i].to_string()
    });

    quote! {
        mod lr_table_scope {
            use parsley::lr::*;
            use parsley::bnf::{self, Symbol::*};
            // Put the states into a Kx(M+1+N) LR table.
            pub const LR_TABLE: LrTable<#k, {#m+1}, #n> = LrTable {
                t_sym_descr: [ #(#t_descrs),* ],
                nt_sym_descr: [ #(#nt_descrs),* ],
                states: [ #(#states),* ],
                start_state: #start_state,
            };
        }
    }
}
