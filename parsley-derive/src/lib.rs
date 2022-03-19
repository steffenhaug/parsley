#[macro_use]
extern crate lazy_static;
extern crate parsley_util;
extern crate proc_macro;
mod compile;

use parsley_util::bnf::Symbol::*;
use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use std::path::PathBuf;
use syn::{Attribute, Data, DataEnum, DeriveInput, Fields, Ident, Variant};

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

    // Fill out the symtab.

    let mut symtab = compile::TERMINALS.lock().unwrap();

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

    let eof_id = symtab.len();

    // Now we no longer need the symtab.
    drop(symtab);

    let lrtab_module = grammar_file.map_or(quote! {}, |file| {
        // Look for grammar file relative to the crate root.
        // This is necessary because the rust language server and cargo
        // is not consistent in whether the compilation is crate-relative
        // or workspace-relative.
        let manifest = std::env::var("CARGO_MANIFEST_DIR")
            .expect("missing $CARGO_MANIFEST_DIR, something is wrong with your cargo project");
        let src_dir: PathBuf = [&manifest, "src", &file].iter().collect();

        // Open the file.
        let src = std::fs::read_to_string(src_dir).expect("grammar file not found");

        // Compile the grammar.
        crate::compile::compile_lr_table(src, &ident)
    });

    let expanded = quote! {
        #[automatically_derived]
        impl Alphabet for #ident {
            fn id(&self) -> usize {
                match self {
                    #match_arms
                    &_ => panic!("not a terminal")
                }
            }

            fn eof() -> usize {
                #eof_id
            }
        }

        #lrtab_module
    };

    expanded.into()
}
