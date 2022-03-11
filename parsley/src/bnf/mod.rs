mod bnf_parser;

use self::Symbol::*;
use crate::alphabet::Alphabet;
use crate::lr::LrItem;

use std::sync::Arc;
use std::collections::{HashMap, HashSet};
use std::fmt;

// Re-export
pub use bnf_parser::parse_bnf as parse;
// BNF grammar AST:

type Set = HashSet<Symbol>;
type Map = HashMap<Symbol, Set>;

/// Defines a grammar over the alphabet `A`.
#[derive(Debug)]
pub struct Grammar<A: Alphabet> {
    nonterminals: Set,
    terminals: Set,
    start_sym: Symbol,
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
    Terminal(Arc<str>),
    Nonterminal(Arc<str>),
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

        let Production {
            symbol: a,
            recipe: alpha_b_beta,
        } = p;

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
            let initial_set = if *nt == self.start_sym {
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

        writeln!(f, "Start symbol: {}", self.start_sym)?;

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

// cba newtyping to impl display for now

pub fn pretty_print_map(map: &Map, description: &str) {
    println!("{}", description);
    for (k, v) in map {
        print!("{}: ", k);
        pretty_print_set(v);
        println!();
    }
}

pub fn pretty_print_set(set: &Set) {
    print!("{{");
    for (i, x) in set.iter().enumerate() {
        if i != 0 {
            print!(", ");
        }
        print!("{}", x);
    }
    print!("}}");
}

