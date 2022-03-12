extern crate proc_macro;

use parsley::bnf::bnf_parser::{semantic_analysis, BnfParser, BnfToken, BnfToken::*};
use parsley::bnf::pretty_print_set;
use parsley::parser::Parser;
use proc_macro::TokenStream;
use proc_macro2::{Literal, TokenTree};
use quote::quote;
use std::mem;
use syn::{Data, DataEnum, DeriveInput, Fields, Ident, Variant};

fn extract_variant_info(v: &mut Variant) -> Option<(Literal, Ident, Fields)> {
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
                    return Some((l, ident.clone(), fields.clone()));
                }
            }
        }
    }

    // No terminals was found.
    None
}

#[proc_macro_derive(Alphabet, attributes(terminal))]
pub fn alphabet_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as DeriveInput);

    let DeriveInput { ident, data, .. } = ast;

    // Extract the relevant info about the enum variants from the tree.
    // (terminals in the grammar, identifier and fields)
    let variants: Vec<(Literal, Ident, Fields)> =
        if let Data::Enum(DataEnum { variants, .. }) = data {
            variants
                .into_iter()
                .filter_map(|mut v| extract_variant_info(&mut v))
                .collect()
        } else {
            panic!("Only enums can derive Alphabet!");
        };

    // The match arms for string -> id like "abc" => Some(3)
    let match_arms_from_str: proc_macro2::TokenStream = variants
        .iter()
        .enumerate()
        .map(|(n, (l, _, _))| {
            // Let's keep it simple and only do one for now.
            // We should really zip over ls with n and flatten.
            quote! { #l => Some(#n), }
        })
        .collect();

    // Match arms for self -> id
    let match_arms: proc_macro2::TokenStream = variants
        .iter()
        .enumerate()
        .map(|(n, (_, id, fs))| match fs {
            Fields::Unit => quote! { #ident :: #id     => Some(#n), },
            Fields::Unnamed(_) => quote! { #ident :: #id (_) => Some(#n), },
            Fields::Named(_) => panic!("Named fields are supported!"),
        })
        .collect();

    let n_variants = variants.len();

    let expanded = quote! {
        impl Alphabet for #ident {
            fn lr_id_from_str(terminal: &str) -> Option<usize> {
                match terminal {
                    #match_arms_from_str
                    t  => None
                }
            }

            fn lr_id(&self) -> Option<usize> {
                match self {
                    #match_arms
                    &_ => None
                }
            }

            fn n_lr_ids(&self) -> usize {
                #n_variants
            }
        }
    };

    expanded.into()
}

/// Turn a stream of Rust-tokens into tokens that we can parse with
/// our BNF grammar parser.
fn to_bnf_token(tok: proc_macro::TokenTree) -> BnfToken {
    use proc_macro::TokenTree::{Ident, Literal, Punct};
    let tok_str = match tok {
        Ident(ident, ..) => ident.to_string(),
        Punct(ch, ..) => ch.to_string(),
        Literal(symbol, ..) => symbol.to_string(),
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

    let ty: String = {
        let tydecl = tokens.drain(tydecl_at..tydecl_at + 2);
        let ty = if let Some(Symbol(ty_sym)) = tydecl.skip(1).next() {
            ty_sym
        } else {
            panic!("alphbet type must be valid rust struct/enum name")
        };
        ty
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

    let g = quote! {};

    g.into()
}
