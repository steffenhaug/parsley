extern crate proc_macro;
#[macro_use]
extern crate lazy_static;

// TODO: make new modules and only use proc_macro2-things.
// the proc-macro vs proc-macro2 name conflicts are actually
// very irritating, so i tihnk thats the best move.

use parsley::bnf::bnf_parser::{semantic_analysis, BnfParser, BnfToken, BnfToken::*};
use parsley::bnf::pretty_print_set;
use parsley::lr::Action;
use parsley::parser::Parser;
use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use std::mem;
use syn::{Attribute, Data, DataEnum, DeriveInput, Fields, Ident, Variant};

use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
    // Database of terminals in the language.
    static ref TERMINALS: Mutex<HashMap<String, usize>> = Mutex::new(HashMap::new());
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

fn string_in_parens<'a, T>(toks: T) -> Option<String>
where
    T: IntoIterator<Item = &'a TokenTree>,
{
    if let Some(TokenTree::Group(g)) = toks.into_iter().next() {
        if let Some(TokenTree::Literal(l)) = g.stream().into_iter().next() {
            // Turn the literal into a String. Since this is a string literal, the
            // token will be something like "\"foobar\"". So we have to so some
            // ugly hacking. I don't think this covers the general case but oh well.
            let quoted = l.to_string();
            return Some(quoted[1..quoted.len() - 1].into());
        }
    }

    None
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
        symtab.insert(literal.to_string(), seq);
    }

    let match_arms: proc_macro2::TokenStream = variants
        .iter()
        .map(|(lit, id, fs)| {
            let n = symtab[lit];
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

    let lrtab_module = grammar_file.map_or( quote! {}, |file| {
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
    let lr_items = grammar.canonical_lr0_items();

    parsley::bnf::pretty_print_map(&fol, "FOLLOW SETS:");

    println!("Canonical LR(0) items:");
    for it in &lr_items {
        pretty_print_set(&grammar.kernel(it));
        println!();
    }

    // Now that we are ready to compile the table, we need the symtab.
    let symtab = TERMINALS.lock().unwrap();

    let m = grammar.terminals.len();
    let n = grammar.nonterminals.len();
    let k = lr_items.len();

    // We have K states, each with M actions and N gotos.
    // Note, we don't have a column for $, because we can check for
    // eof by seeing if the input stream has no more items. might change that.
    // The easiest thing to do is to maintain two separate tables for now.
    //
    // Note that these tables are tables of _quoted rust code_! After computing
    // the table (of code) the table will be compiled into code compiling into
    // a LR parsing table, so we can have the table completely in the programs
    // data segment of the program so we get good performance.
    let mut act: Vec<Vec<_>> = (0..k)
        .map(|_| (0..m).map(|_| quote!(Action::Error)).collect())
        .collect();

    let mut go: Vec<Vec<_>> = (0..k)
        .map(|_| (0..n).map(|_| quote!(Action::Error)).collect())
        .collect();

    // So for example, now something like this is valid.
    act[0][symtab["n"]] = quote!(Action::Shift(5));

    // Write `State`s for each state `i`.
    let states = (0..k).map(|i|{
        let (a, g) = (&act[i], &go[i]);
        quote! {
            State { actions: [ #(#a),* ], gotos: [ #(#g),*] }
        }
    });

    quote! {
        mod lr_table_scope {
            use parsley::lr::*;
            // Put the states into a Kx(M+N) LR table.
            pub const LR_TABLE: LrTable<#k, #m, #n> = LrTable {
                states: [ #(#states),* ]
            };
        }
    }
}
