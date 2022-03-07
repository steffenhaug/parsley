/// Parser for BNF grammars.
/// The parser itself is using recursive descent, and can thus
/// utilize EBNF-things like Kleene stars easily by using loops,
/// but it doesn't parse grammars that uses these operators.
///
/// The parser showcases how to use recursive descent to construct
/// a "dumb tree" (just nodes with text, basically) and how you can
/// do another (semantic) pass to identify which symbols are non-
/// terminal, and so on.

use std::mem;
use std::collections::{HashMap, HashSet};
use logos::Logos;
use crate::parser::Parser;
use crate::alphabet::Alphabet;
use std::fmt;
use self::BnfToken::*; // Simplifies the recursive descent.

/// A token in a BNF grammar specification.
#[derive(Logos, Debug, Clone, PartialEq)]
enum BnfToken {
    #[token(":")]
    Colon,
    #[token("|")]
    Pipe,
    #[regex(r"[a-zA-Z!$\+\-\^&<*/'=?@>_~]+", |lex| lex.slice().to_owned() )]
    Symbol(String),
    #[regex("\n\n")] // double newline separates rules
    Separator,
    #[error]
    #[regex(r"[ \t\n]", logos::skip)]
    Error,
    Eof
}

// BNF grammar AST:

/// Defines a grammar over the alphabet `A`.
#[derive(Debug)]
pub struct Grammar<A: Alphabet> {
    nonterminals: HashSet<String>,
    terminals: HashMap<String, usize>,
    productions: Vec<Production>,
    // Marker so we can use static functions on A.
    _marker: std::marker::PhantomData<A>
}

#[derive(Debug)]
pub struct Production {
    symbol: String,
    recipe: Vec<Symbol>
}

#[derive(Debug)]
pub enum Symbol {
    Terminal(String),
    Nonterminal(String)
}

// The "dumb tree" is just a vector of rules:

#[derive(Debug)]
pub struct Rule {
    symbol: String,
    recipe: Vec<String>
}

#[derive(Debug)]
pub struct BnfParser {
    tokens: Vec<BnfToken>,
    position: usize,
    ast: Vec<Rule>
}

impl Parser<Vec<Rule>> for BnfParser {
    type Error = ();
    fn parse(self) -> Result<Vec<Rule>, Self::Error> {
        self.grammar()
    }
}

/// Semantic analysis turns the list of rules into a proper
/// grammar where non-terminals are determined. This will in
/// the future probably also include computation of FIRST/FOLLOW.
pub fn semantic_analysis<A>(rules: Vec<Rule>) -> Result<Grammar<A>, ()>
where A: Alphabet
{
    let mut terminals = HashMap::<String, usize>::new();
    let mut nonterminals = HashSet::<String>::new();
    let mut productions = Vec::new();

    // First step of semantic analysis: Find the non-temrinals.
    for Rule { symbol, .. } in &rules {
        nonterminals.insert(symbol.to_string());
    }

    // Second step of semantic analysis:
    // For all the rules, identify terminals and non-terminals on the
    // right-hand side, and check that all the terminals have an enum
    // variant in the underlying alphabet.
    for Rule { symbol, recipe } in rules {
        let recipe = recipe
            .into_iter()
            .map(|sym| {
                if nonterminals.contains(&sym) {
                    Ok(Symbol::Nonterminal(sym))
                } else {
                    let id = A::lr_id_from_str(&sym).ok_or(&sym)?;
                    terminals.insert(sym.to_string(), id);
                    Ok(Symbol::Terminal(sym))
                }
            })
            .collect::<Result<_, String>>();

        let p = Production { symbol, recipe: recipe.unwrap() };
        productions.push(p);
    }

    let g = Grammar { 
        nonterminals, 
        terminals, 
        productions,
        _marker: std::marker::PhantomData
    };

    Ok(g)
}

impl BnfParser {
    pub fn new(src: impl AsRef<str>) -> BnfParser {
        // Lexing the entire token stream up front should not be problematic.
        let tokens = BnfToken::lexer(src.as_ref()).into_iter().collect();
        BnfParser {
            tokens, 
            position: 0, 
            ast: Vec::new()
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
            symbols.push(self.match_sym()?);
        }

        let rule = Rule {
            symbol: sym.to_string(), recipe: symbols
        };

        Ok(rule)
    }
}

// Display implementations.

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Symbol::Terminal(s) => write!(f, "{}", s),
            Symbol::Nonterminal(s) => write!(f, "{}", s),
        }
    }
}

impl fmt::Display for Production {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ->", self.symbol)?;
        for sym in &self.recipe {
            write!(f, " {}", sym)?;
        }
        Ok(())
    }
}

impl<A: Alphabet> fmt::Display for Grammar<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let _marker: std::marker::PhantomData::<A>;

        // Write productions.
        for rule in &self.productions {
            write!(f, "{}\n", rule)?;
        }

        // Write non-terminals.
        write!(f, "Non-terminals:")?;
        for nt in &self.nonterminals {
            write!(f, " {}", nt)?;
        }
        writeln!(f)?;

        // Write terminals.
        write!(f, "Terminals:")?;
        for t in self.terminals.keys() {
            write!(f, " {}", t)?;
        }

        Ok(())
    }

}
