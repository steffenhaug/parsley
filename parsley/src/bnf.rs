/// Parser for BNF grammars. The parser itself is using recursive descent, and can thus utilize
/// EBNF-things like Kleene stars easily by using loops, but it doesn't parse grammars that uses
/// these operators.
///
/// The parser showcases how to use recursive descent to construct a "dumb tree" (just nodes with
/// text, basically) and how you can do another (semantic) pass to identify which symbols are non-
/// terminal, and so on.
use self::BnfToken::*;
use self::Symbol::*;
use crate::alphabet::Alphabet;
use crate::parser::Parser;
use logos::Logos;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::mem; // Simplifies the recursive descent.

/// A token in a BNF grammar specification.
#[derive(Logos, Debug, Clone, PartialEq)]
enum BnfToken {
    #[token(":")]
    Colon,
    #[token("|")]
    Pipe,
    #[regex(r"[a-zA-Z!$\+\-\^&</'=@>_~]+", |lex| lex.slice().to_owned() )]
    Symbol(String),
    #[regex("\n\n")] // double newline separates rules
    Separator,
    #[error]
    #[regex(r"[ \t\n]", logos::skip)]
    Error,
    Eof,
}

// BNF grammar AST:

type Set = HashSet<Symbol>;
type Map = HashMap<Symbol, Set>;

/// Defines a grammar over the alphabet `A`.
#[derive(Debug)]
pub struct Grammar<A: Alphabet> {
    nonterminals: Set,
    terminals: Set,
    start: Symbol,
    productions: Vec<Production>,
    // Marker so we can access static methods on `A` for semantic validation.
    _a: std::marker::PhantomData<A>,
}

#[derive(Debug)]
pub struct Production {
    symbol: Symbol,
    recipe: Vec<Symbol>,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Symbol {
    Terminal(String),
    Nonterminal(String),
    Epsilon,
    Dollar,
}

impl Symbol {
    /// Construct the set {t} from grammar symbol t.
    fn singleton(self) -> Set {
        Set::from([self])
    }
}

impl<A: Alphabet> Grammar<A> {
    /// Find all rules in the grammar that produces `sym`.
    pub fn productions_for<'a>(&'a self, sym: &'a Symbol) -> impl Iterator<Item = &'a Production> {
        self.productions.iter().filter(|p| p.symbol == *sym)
    }

    /// Check if a symbol is nullable, i. e. if any of the
    /// productions for `sym` allow deriving epsilon.
    pub fn nullable(&self, sym: &Symbol) -> bool {
        self.productions_for(sym).any(|p| p.recipe == [Epsilon])
    }

    /// Computes the FIRST-set of a sequence of grammar symbols, given a
    /// database of first sets for the individual grammar symbols.
    fn first_of_seq<'a, S>(&self, xs: S, first: &'a Map) -> Set
    where
        S: IntoIterator<Item = &'a Symbol>,
    {
        let mut fst = Set::new();

        for x in xs {
            // Add to FIRST(X1 X2 ... Xn) all the non-ε symbols of FIRST(X)
            // I. e., extend fst by FIRST(X) \ {ε}
            fst.extend(&first[x] - &Epsilon.singleton());

            if !self.nullable(x) {
                break;
            }
        }

        fst
    }

    pub fn first_sets(&self) -> Map {
        // Set up the starting sets.
        let mut sets = Map::new();

        // Consider ε to be terminal to remove special cases.
        sets.insert(Epsilon, Epsilon.singleton());

        // Rule #1: Terminals t start with {t}.
        for t in &self.terminals {
            sets.insert(t.clone(), t.clone().singleton());
        }

        // Rule #3: Nullable non-terminals start with {ε}.
        for nt in &self.nonterminals {
            let initial_set = if self.nullable(nt) {
                Epsilon.singleton()
            } else {
                Set::new()
            };

            sets.insert(nt.clone(), initial_set);
        }

        // Now we iterate with Rule #2 ref. Dragon Book.
        // The book seems to imply that we should iterate with every rule, but
        // clearly whether a symbol is terminal or nullable is invariant over
        // the loop.
        loop {
            let old_sets = sets.clone();

            for Production { symbol, recipe } in &self.productions {
                // Get the first set of the left hand side of this production.
                // All grammar symbols should already be inserted, so this failing
                // indicates something is seriously wrong, and a panic is warranted.
                let fst = sets.get_mut(symbol).expect("corrupt first set database");

                // Rule #2:
                fst.extend(self.first_of_seq(recipe, &old_sets));
            }

            if sets == old_sets {
                break;
            }
        }

        sets
    }

    /// Calculates a map of new follow sets based on the old follows and a production.
    fn update_follows(
        &self,
        p: &Production,
        fol_sets: &Map,
        fst_sets: &Map,
        new_fol_sets: &mut Map,
    ) {
        // Following the notation of the dragon book, A -> α B β, where
        // α, β arbitrary sequences of symbols. We are only interested in
        // β. A, B denoted as a, b to follow rust variable convention.

        let Production { symbol: a, recipe: alpha_b_beta } = p;

        for (i, b) in alpha_b_beta.iter().enumerate() {
            // We are only interested in what follows non-terminals.
            if !self.nonterminals.contains(b) {
                continue;
            }

            // The symbol sequence beta starts after the position of B.
            let beta = &alpha_b_beta[i + 1..];

            // Again, all non-terminals are already added to this map, so
            // in the event that this fails, something is very seriously wrong.
            let fol = new_fol_sets
                .get_mut(b)
                .expect("corrupt follow set database");

            // Finally ready to apply the rules:
            let fst_beta = self.first_of_seq(beta, fst_sets);

            if !(fst_beta.is_empty() || fst_beta.contains(&Epsilon)) {
                // Rule #2: B followed by β where ε not in FIRST(β).
                fol.extend(&fst_beta - &Epsilon.singleton());
            } else {
                // Rule #3: B followed by β where ε in FIRST(β), or A -> αB.
                fol.extend(&fol_sets[a] - &Epsilon.singleton());
            }
        }
    }

    pub fn follow_sets(&self, fst_sets: &Map) -> Map {
        let mut sets = Map::new();

        // Rule #1: Start with $ as the follow of the start symbol.
        for nt in &self.nonterminals {
            let initial_set = if *nt == self.start {
                Dollar.singleton()
            } else {
                Set::new()
            };

            sets.insert(nt.clone(), initial_set);
        }

        loop {
            let old_sets = sets.clone();

            for p in &self.productions {
                self.update_follows(p, &old_sets, fst_sets, &mut sets);
            }

            if sets == old_sets {
                break;
            }
        }

        sets
    }
}

// The "dumb tree" is just a vector of rules:

#[derive(Debug)]
pub struct Rule {
    symbol: String,
    recipe: Vec<String>,
}

#[derive(Debug)]
pub struct BnfParser {
    tokens: Vec<BnfToken>,
    position: usize,
    ast: Vec<Rule>,
}

impl Parser<Vec<Rule>> for BnfParser {
    type Error = ();
    fn parse(self) -> Result<Vec<Rule>, Self::Error> {
        self.grammar()
    }
}

/// Semantic analysis turns the list of rules into a proper
/// grammar where important semantic information is determined.
pub fn semantic_analysis<A>(rules: Vec<Rule>, start_name: &str) -> Result<Grammar<A>, ()>
where
    A: Alphabet,
{
    let mut terminals = Set::new();
    let mut nonterminals = Set::new();
    let mut productions = Vec::new();

    // First step of semantic analysis: Find the non-temrinals.
    for Rule { symbol, .. } in &rules {
        nonterminals.insert(Nonterminal(symbol.to_string()));
    }

    // Check that the start symbol is actually a valid nonterminal.
    let start = Nonterminal(start_name.to_string());
    if !nonterminals.contains(&start) {
        return Err(());
    }

    // Second step of semantic analysis:
    // For all the rules, identify terminals and non-terminals on the
    // right-hand side, and check that all the terminals have an enum
    // variant in the underlying alphabet.
    for Rule { symbol, recipe } in rules {
        let mut recipe: Vec<Symbol> = recipe
            .into_iter()
            .map(|sym| {
                if nonterminals.contains(&Nonterminal(sym.to_string())) {
                    Ok(Nonterminal(sym))
                } else {
                    terminals.insert(Terminal(sym.to_string()));
                    Ok(Terminal(sym))
                }
            })
            .collect::<Result<_, String>>()
            .unwrap();

        if recipe.is_empty() {
            recipe.push(Epsilon);
        }

        let p = Production {
            symbol: Nonterminal(symbol),
            recipe,
        };

        productions.push(p);
    }

    let g = Grammar {
        nonterminals,
        terminals,
        productions,
        start,
        _a: std::marker::PhantomData,
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
            symbols.push(self.match_sym()?);
        }

        let rule = Rule {
            symbol: sym.to_string(),
            recipe: symbols,
        };

        Ok(rule)
    }
}

// Display implementations.

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Terminal(s) => write!(f, "{}", s),
            Nonterminal(s) => write!(f, "{}", s),
            Epsilon => write!(f, "ε"),
            Dollar => write!(f, "$"),
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
        // Write productions.
        for rule in &self.productions {
            writeln!(f, "{}", rule)?;
        }

        writeln!(f, "Start symbol: {}", self.start)?;

        // Write non-terminals.
        write!(f, "Non-terminals:")?;
        for nt in &self.nonterminals {
            write!(f, " {}", nt)?;
        }
        writeln!(f)?;

        // Write terminals.
        write!(f, "Terminals:")?;
        for t in &self.terminals {
            write!(f, " {}", t)?;
        }

        Ok(())
    }
}

// We have to implement this ourselves to circumvent the PhantomData, so
// we don't force all alphabet symbols to implement clone.
impl Clone for Symbol {
    fn clone(&self) -> Self {
        match self {
            Nonterminal(s) => Nonterminal(s.clone()),
            Terminal(s) => Terminal(s.clone()),
            Epsilon => Epsilon,
            Dollar => Dollar,
        }
    }
}
