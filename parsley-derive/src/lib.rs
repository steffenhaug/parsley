extern crate proc_macro;

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
