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
use proc_macro2::{Literal, TokenTree};
use quote::quote;
use std::mem;
use syn::{Data, DataEnum, DeriveInput, Fields, Ident, Variant};

use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
    // Database of terminals in the language.
    static ref TERMINALS: Mutex<HashMap<String, usize>> = Mutex::new(HashMap::new());
    static ref HAVE_WE_PARSED_AN_ALPHABET: Mutex<bool> = Mutex::new(false);
}

fn extract_variant_info(v: &mut Variant) -> Option<(String, Ident, Fields)> {
    let Variant {
        attrs,
        ident,
        fields,
        ..
    } = v;

    for attr in attrs {
        let name = if let Some(name) = attr.path.get_ident() {
            name.to_string()
        } else {
            continue;
        };

        if name == "terminal" {
            // Steal the tokens from the token stream
            let tokens = mem::replace(&mut attr.tokens, proc_macro2::TokenStream::new());

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
    }

    // No terminals was found.
    None
}

#[proc_macro_derive(Alphabet, attributes(terminal))]
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

    let DeriveInput { ident, data, .. } = ast;

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

    let expanded = quote! {
        impl Alphabet for #ident {
            fn id(&self) -> usize {
                match self {
                    #match_arms
                }
            }
        }
    };

    *alphabet_defined = true;

    expanded.into()
}

/// Turn a stream of Rust-tokens into tokens that we can parse with
/// our BNF grammar parser.
fn to_bnf_token(tok: proc_macro::TokenTree) -> BnfToken {
    use proc_macro::TokenTree::{Ident, Literal, Punct};

    let tok_str = match tok {
        Ident(ident, ..) => ident.to_string(),
        Punct(ch, ..) => ch.to_string(),
        Literal(symbol, ..) => {
            // Remove quotes
            let symbol = symbol.to_string();
            String::from(&symbol[1..symbol.len() - 1])
        }
        _ => return Error,
    };

    match &tok_str[..] {
        "match" => Match,
        ":" => Colon,
        "|" => Pipe,
        sym => Symbol(sym.to_string()),
    }
}

#[proc_macro]
pub fn grammar(input: TokenStream) -> TokenStream {
    let mut tokens: Vec<BnfToken> = input
        .into_iter()
        .map(to_bnf_token)
        .chain([Eof].into_iter())
        .collect();

    // Find a type declaration.
    let tydecl_at = tokens
        .iter()
        .position(|tok| *tok == Symbol("type".to_owned()));

    let tydecl_at = if let Some(p) = tydecl_at {
        p
    } else {
        panic!("no alphabet type declaraction")
    };

    // Remove type declaration from the token stream.
    // This has to happen at the proc-macro level before we
    // give over the token stream to the parser. We get it
    // as a string, which is very roundabout since it was
    // originialy a rust identifier, and we need to turn it
    // into a rust-identifier again, but it is easier to
    // find it in our own custom token stream, so this will
    // do for now.

    let ty = {
        let tydecl = tokens.drain(tydecl_at..tydecl_at + 2);
        let ty = if let Some(Symbol(ty_sym)) = tydecl.skip(1).next() {
            ty_sym
        } else {
            panic!("alphbet type must be valid rust struct/enum name")
        };
        proc_macro2::Ident::new(&ty, proc_macro2::Span::call_site())
    };

    let start_at = tokens
        .iter()
        .position(|tok| *tok == Symbol("start".to_owned()))
        .expect("no start symbol");

    // Now that we know which is the start, replace with a normal match.
    tokens[start_at] = Match;

    let start: String = if let Symbol(start_sym) = &tokens[start_at + 1] {
        start_sym.to_owned()
    } else {
        panic!("start must be a valid nonterminal");
    };

    let ast = BnfParser::from_toks(tokens)
        .parse()
        .expect("failed to parse grammar");

    let grammar = semantic_analysis(ast, &start).expect("semantic error in grammar");
    let fst = grammar.first_sets();
    let fol = grammar.follow_sets(&fst);
    let lr_items = grammar.canonical_lr0_items();

    println!("Canonical LR(0) items:");
    for it in &lr_items {
        pretty_print_set(&grammar.kernel(it));
        println!();
    }

    let m = grammar.terminals.len();
    let n = grammar.nonterminals.len();
    let k = lr_items.len();

    let mut action: Vec<Vec<_>> = Vec::new();
    let mut goto: Vec<Vec<_>> = Vec::new();

    // Fill the table with error by default.
    // Note how this works: The table has quoted rust code that
    // represents the error action.
    for _ in 0..k {
        action.push((0..m).map(|_| quote! { Action::Error }).collect());
        goto.push((0..n).map(|_| quote! { Action::Error }).collect());
    }

    // Now that we are ready to compile the table, we need the symtab.
    let symtab = TERMINALS.lock().unwrap();

    dbg!(&symtab);

    let g = quote! {};

    g.into()
}
