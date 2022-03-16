/// This module contains a parser for BNF grammars.
///
/// The parser itself is using recursive descent, and can thus utilize
/// EBNF-things like Kleene stars easily by using loops, but it doesn't parse grammars that uses
/// these operators.
///
/// The parser showcases how to use recursive descent to construct a "dumb tree" (just nodes with
/// text, basically) and how you can do another (semantic) pass to determine the semantics and do
/// semantic validation.
///
/// Additionally, it demonstrates how to easily make such a parser in Rust, using a struct to hold
/// the intermediate representation while building it, encoding the grammar rules with methods on
/// this struct with a mutable reference to self, and then finally how the top level rule consumes
/// the parser struct to get ownership of the intermediate representation after the parsing
/// succeeds, to hand it off to the caller safely without cloning.
///
/// Also, note the use of `Arc<str>` in place of `String`. The grammar symbols are used in a wide
/// range of control structures, so it is very nice to be able to share them cheaply. `Arc<str>`
/// saves a layer of indirection over `Arc<String>`.
use crate::bnf::{Grammar, Production, Symbol, Symbol::*};
use crate::parser::Parser;
use logos::Logos;
use std::collections::HashSet;
use std::mem;
use std::sync::Arc;
use BnfToken::*;

/// A token in a BNF grammar specification.
/// Note that the legal names of grammar symbols is quite liberal.
#[derive(Logos, Debug, Clone, PartialEq)]
pub enum BnfToken {
    #[token(":")]
    Colon,
    #[token("|")]
    Pipe,
    #[regex(r"[a-zA-Z!$\+*\-\^&</'=@>_~\(\)]+", |lex| lex.slice().to_owned() )]
    Symbol(String),
    #[regex("\n\n")] // double newline separates rules
    Separator,
    #[error]
    #[regex(r"[ \t\n]", logos::skip)]
    Error,
    Eof,
}

/// A rule is simply the name of a symbol, and a list
/// of the things that produce this symbol.
#[derive(Debug)]
pub struct Rule {
    symbol: Arc<str>,
    recipe: Vec<Arc<str>>,
}

///semantic_analysis,  The "dumb tree" is just a vector of rules!
type Rules = Vec<Rule>;

/// Struct to own the AST while it's under construction.
#[derive(Debug)]
pub struct BnfParser {
    tokens: Vec<BnfToken>,
    position: usize,
    ast: Vec<Rule>,
}

/// TODO use a real error type.
/// Needs variants for parsing- and semantic errors.
pub type BnfParseError = ();

impl Parser<Rules> for BnfParser {
    type Error = BnfParseError;

    fn parse(self) -> Result<Rules, Self::Error> {
        // The trait implementation is super simple: Just invoke the top-level rule.
        self.grammar()
    }
}

impl BnfParser {
    pub fn from_src(src: &str) -> BnfParser {
        let toks = BnfToken::lexer(src)
            .into_iter()
            .chain([Eof]) // Add on Eof
            .collect();

        Self::from_toks(toks)
    }

    pub fn from_toks(tokens: Vec<BnfToken>) -> BnfParser {
        BnfParser {
            tokens,
            position: 0,
            ast: Vec::new(),
        }
    }

    fn look(&self) -> BnfToken {
        // Most tokens are cheap to clone, and even in the case
        // where it has a String we need to clone the symbol later if
        // we dont do it here.
        if self.position < self.tokens.len() {
            self.tokens[self.position].clone()
        } else {
            Eof
        }
    }

    fn match_sym(&mut self) -> Result<String, ()> {
        if let Symbol(s) = self.look() {
            self.position += 1;
            Ok(s)
        } else {
            Err(())
        }
    }

    fn match_tok(&mut self, tok: BnfToken) -> Result<(), ()> {
        // This is the most generic way to check if two enums are of the
        // same variant, since doing this with a match would require N
        // lines for N variants, and testing for equality wouldnt be
        // exactly correct for variant with strings.
        if mem::discriminant(&self.look()) == mem::discriminant(&tok) {
            self.position += 1;
            Ok(())
        } else {
            Err(())
        }
    }

    // Production:
    //   grammar -> rule*
    // Note how easy is is to do Kleene stars when we are not
    // locked to the world of DFAs.
    fn grammar(mut self) -> Result<Vec<Rule>, ()> {
        // As long as there are symbols in the token stream...
        while let Symbol(_) = self.look() {
            // Parse a rule and add it to the list of productions.
            let rule = self.rule()?;
            self.ast.extend(rule.into_iter());
        }

        // When there are no more symbols in the lookahead, we should
        // be at the end of input.
        if self.look() == Eof {
            Ok(self.ast)
        } else {
            Err(())
        }
    }

    // Production:
    //   rule -> symbol ':' recipe ('|' recipe)*
    // A rule has one or more recipes, separated by pipe symbols.
    // Again, we use a loop to implement the Kleene star.
    fn rule(&mut self) -> Result<Vec<Rule>, ()> {
        let mut productions = Vec::new();
        let sym = self.match_sym()?;
        self.match_tok(Colon)?;
        productions.push(self.recipe(&sym)?);

        while self.look() == Pipe {
            self.match_tok(Pipe)?;
            productions.push(self.recipe(&sym)?);
        }

        if self.look() == Separator {
            self.match_tok(Separator)?;
        }

        Ok(productions)
    }

    // Production:
    //   recipe -> symbol*
    // A recipe with no symbols are permitted and should be interpreted
    // as epsillon.
    fn recipe(&mut self, sym: &str) -> Result<Rule, ()> {
        let mut symbols = Vec::new();
        while let Symbol(_) = self.look() {
            symbols.push(self.match_sym()?.into());
        }

        let rule = Rule {
            symbol: sym.to_string().into(),
            recipe: symbols,
        };

        Ok(rule)
    }
}

/// Semantic analysis turns the list of rules into a proper
/// grammar where important semantic information is determined.
pub fn semantic_analysis(rules: Rules) -> Result<Grammar, BnfParseError> {
    let mut terminals = HashSet::<Arc<str>>::new();
    let mut nonterminals = HashSet::<Arc<str>>::new();
    let mut productions = Vec::new();
    let mut start_sym = None;

    // First step of semantic analysis: Find the non-temrinals.
    for (i, Rule { symbol, .. }) in rules.iter().enumerate() {
        nonterminals.insert(symbol.clone());

        // LHS Symbol of the first rule is assumed to be the start symbol.
        if i == 0 {
            start_sym.replace(Nonterminal(symbol.clone()));
        }
    }

    // Second step of semantic analysis:
    // For all the rules, identify terminals and non-terminals on the right-hand side.
    for Rule { symbol, recipe } in rules {
        let mut recipe: Vec<Symbol> = recipe
            .into_iter()
            .map(|sym| {
                if nonterminals.contains(&sym) {
                    Ok(Nonterminal(sym))
                } else {
                    terminals.insert(sym.clone());
                    Ok(Terminal(sym))
                }
            })
            .collect::<Result<_, String>>()
            .expect("(fix me)");

        if recipe.is_empty() {
            recipe.push(Epsilon);
        }

        let p = Production {
            symbol: Nonterminal(symbol),
            recipe,
        };

        productions.push(p);
    }

    // Make the symbol sets contain actual symbols.
    let nonterminals = nonterminals.into_iter().map(Nonterminal).collect();
    let terminals = terminals.into_iter().map(Terminal).collect();

    let g = Grammar {
        nonterminals,
        terminals,
        productions,
        start_sym,
    };

    Ok(g)
}
