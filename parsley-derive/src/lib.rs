extern crate proc_macro;
#[macro_use]
extern crate lazy_static;

// TODO: Modularise the compiler.

use parsley::bnf::bnf_parser::{semantic_analysis, BnfParser};
use parsley::bnf::{self, pretty_print_set, Symbol};
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

    // Note whether we have already implemented an alphabet type, because multiple is currently
    // unsupported.
    static ref HAVE_WE_PARSED_AN_ALPHABET: Mutex<bool> = Mutex::new(false);
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
    // Check if we already defined an alphabet.
    let mut alphabet_defined = HAVE_WE_PARSED_AN_ALPHABET.lock().unwrap();
    if *alphabet_defined {
        panic!("only supporting one alphabet type for now")
    }

    // Supporting multiple different alphabets could give conflicts
    // in the terminal symbol table, so this would need some extra
    // book-keeping. Definitely not too tricky, but not a priority.

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
        symtab.insert(Symbol::Terminal(literal.to_string().into()), seq);
    }

    let match_arms: proc_macro2::TokenStream = variants
        .iter()
        .map(|(lit, id, fs)| {
            let n = symtab[&Symbol::Terminal(lit.to_owned().into())];
            match fs {
                Fields::Unit => quote! { #ident :: #id     => #n, },
                Fields::Unnamed(_) => quote! { #ident :: #id (_) => #n, },
                Fields::Named(_) => panic!("Named fields are supported!"),
            }
        })
        .collect();

    // Now we no longer need the symtab.
    drop(symtab);

    // Indicate that an alphabet have been defined, since the symbol table
    // can not currently be re-used.
    *alphabet_defined = true;

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
    symtab.insert(Symbol::Dollar, m);

    // Note that these tables are tables of _quoted Rust code_! After computing
    // the table (of code) the table will be compiled into code compiling into
    // a LR parsing table, so we can have the table completely in the programs
    // data segment of the program so we get good performance.

    let mut act: Vec<Vec<_>> = (0..k)
        .map(|_| (0..m + 1).map(|_| quote!(Action::Error)).collect())
        .collect();

    let mut go: Vec<Vec<_>> = (0..k)
        .map(|_| (0..n).map(|_| quote!(Action::Error)).collect())
        .collect();

    // So for example, now something like this is valid:
    //     act[0][symtab["n"]] = quote!(Action::Shift(5));

    // Now, one last thing: Until now we operated with our LR item set as
    // a _actual_ set (a HashSet) in which the order is not defined, so
    // to give meaningful numbers to our states, we will convert it into
    // a Vector!
    let c = c.into_iter().collect::<Vec<_>>();
    // Note that this is the first time ordering of the LR items have been
    // enforced, i.e. there is no correspondence between the grammar structure
    // and the ordering of the items. If desired, this could be fixed by
    // using a Set-structure with deterministic ordering. (HashSet doesn't in general)

    // Compile the LR table! (finally)
    // 1. Cosntruct `clri` = collection of canonical lr items is already done.
    //
    // 2. State `i` is constructed from `clri[i]`.
    // The parsing actions for state `i` is determined as follows:
    //  a) A -> α · a β in clri[i] and GOTO(clri[i], a) = clri[j]
    //      => action[i, smytab[a]] := shift j
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
    //  c) If S' -> S · in clri[i] => action[i, symtab[$]] := accept

    for (i, i_i) in c.iter().enumerate() {
        // a) Find all LR-items with a terminal after the dot.
        for lri in i_i.iter().filter(|item| item.has_terminal_after_dot()) {
            // Get the symbol after the dot, and calculate GOTO(i_i, a)
            let a = lri.after_dot_unchecked();
            let goto = grammar.goto(i_i, a);

            // Find the next state i_j, where GOTO(i_i, a) == i_j.
            if let Some(j) = c.iter().position(|i_j| goto == *i_j) {
                print!("GOT TERMINAL AFTER DOT: {} GOTO: ", &lri);
                pretty_print_set(&goto);
                println!(" {} -> {}", i, j);
                act[i][symtab[a]] = quote!(Action::Shift(#j));
            }
        }
    }

    // Write `State`s for each state `i`.
    let states = (0..k).map(|i| {
        let (a, g) = (&act[i], &go[i]);
        // String representation of a set of LR items
        let descr = lr::describe(&grammar.kernel(&c[i]));
        quote! {
            State { state: #i, actions: [ #(#a),* ], gotos: [ #(#g),*], description: #descr }
        }
    });

    quote! {
        mod lr_table_scope {
            use parsley::lr::*;
            // Put the states into a Kx(M+1+N) LR table.
            pub const LR_TABLE: LrTable<#k, {#m+1}, #n> = LrTable {
                states: [ #(#states),* ]
            };
        }
    }
}
