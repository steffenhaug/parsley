/// The bnf-module contains a structure for and operations on
/// grammars in bnf form, such as computing FIRST- and FOLLOW-
/// sets, creating LR-items, calculating closures of LR-items,
/// and so on.
pub mod bnf_parser;

use self::Symbol::*;
use crate::lr::{Item, ItemSet};

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::sync::Arc;

type Set = HashSet<Symbol>;
type Map = HashMap<Symbol, Set>;

/// Defines a grammar over the alphabet `A`.
#[derive(Debug)]
pub struct Grammar {
    pub nonterminals: Set,
    pub terminals: Set,
    pub start_sym: Option<Symbol>,
    productions: Vec<Production>,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Production {
    pub symbol: Symbol,
    pub recipe: Vec<Symbol>,
}

impl Production {
    fn lr_item(self) -> Item {
        Item {
            production: self,
            dot: 0,
        }
    }
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

impl Grammar {
    /// Find all rules in the grammar that produces `sym`.
    pub fn productions_for<'a>(&'a self, sym: &'a Symbol) -> impl Iterator<Item = &'a Production> {
        self.productions.iter().filter(|p| p.symbol == *sym)
    }

    /// Check if a symbol is nullable, i. e. if any of the
    /// productions for `sym` allow deriving epsilon.
    pub fn nullable(&self, sym: &Symbol) -> bool {
        self.productions_for(sym).any(|p| p.recipe == [Epsilon])
    }

    pub fn symbols<'a>(&'a self) -> impl Iterator<Item = &'a Symbol> {
        self.nonterminals.iter().chain(self.terminals.iter())
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

    /// Computes the first sets for all grammar symbols, including terminals
    /// and also including epsilon.
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
        // The book almost seems to imply that we should iterate with every rule,
        // but clearly whether a symbol is terminal or nullable is invariant over
        // the loop.

        'fix: loop {
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
                break 'fix;
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
            // If there are non-terminals, per definition the start symbol exists,
            // since we assumed the start symbol to be the first non-terminal.
            let initial_set = if *nt == self.start_sym.clone().expect("infallible") {
                Dollar.singleton()
            } else {
                Set::new()
            };

            sets.insert(nt.clone(), initial_set);
        }

        'fix: loop {
            let old_sets = sets.clone();

            for p in &self.productions {
                self.update_follows(p, &old_sets, fst_sets, &mut sets);
            }

            if sets == old_sets {
                break 'fix;
            }
        }

        sets
    }

    /// Get _all_ LR-items (dots in every position) for the given productions.
    pub fn lr_items<'a, P>(&self, prods: P) -> Vec<Item>
    where
        P: IntoIterator<Item = &'a Production>,
    {
        let mut items = Vec::new();

        for pr in prods {
            // For a production A -> X Y Z, add the LR items
            // A -> . X Y Z, A -> X . Y Z and so on. Note that
            // we also include the dot == len case (dot at end)
            for dot in 0..1 + pr.recipe.len() {
                items.push(Item::of(dot, pr));
            }
        }

        items
    }

    // Get _all_ LR-items for every production in the grammar.
    pub fn all_lr_items(&self) -> Vec<Item> {
        self.lr_items(&self.productions)
    }

    pub fn closure(&self, mut items: ItemSet) -> ItemSet {
        // Add the lr item itself to t he closure.

        'fix: loop {
            let old_items = items.clone();

            for it in &old_items {
                // Find the symbol after the dot (if there is any)
                if let Some(sym) = it.production.recipe.get(it.dot) {
                    // Find all productions for this symbol, and construct
                    // the LR-item with dot in the firstmost position.
                    let frontier: ItemSet = self
                        .productions_for(sym)
                        .map(|pr| Item::of(0, pr))
                        .collect();

                    // Add these LR-items to the set.
                    items.extend(frontier);
                }
            }

            if items == old_items {
                break 'fix;
            }
        }

        items
    }

    pub fn goto(&self, i: &ItemSet, x: &Symbol) -> ItemSet {
        // GOTO(I, X) where I is a set of items, and X is a grammar symbol
        // is defined as the closure of the sets { A -> α X · β }, such that
        // { A -> α · X β } is in I. So what does this mean?
        //
        // Well, clearly, { A -> α · X β } is in I for a given X boils down
        // to filtering the set I for items where the dot is in front of X.
        // On these sets, we move the dot one step ahead: { A -> α X · β }.
        // This set, we take the closure of.

        // Find · X items.
        let dot_x: ItemSet = i
            .iter()
            .cloned()
            .filter(|it| it.production.recipe.get(it.dot) == Some(x))
            .collect();

        // Move the dot forward.
        let x_dot: ItemSet = dot_x
            .into_iter()
            .map(|it| Item {
                production: it.production,
                dot: it.dot + 1,
            })
            .collect();

        // And take the closure.
        self.closure(x_dot)
    }

    pub fn augmented_start(&self) -> Production {
        let start = self
            .start_sym
            .clone()
            .expect("no start sym (fix this panic)");

        Production {
            symbol: Nonterminal(format!("{}'", start).into()),
            recipe: vec![start.clone()],
        }
    }

    /// The augmented start production with a dot at the end.
    /// When this item would normally indicate the automaton to reduce,
    /// the input should be accepted.
    pub fn accept_item(&self) -> Item {
        let production = self.augmented_start();
        let dot = production.recipe.len();
        Item { production, dot }
    }

    pub fn kernel(&self, i: &ItemSet) -> ItemSet {
        let s = self.augmented_start().lr_item(); // S' -> ·S
        let k = i
            .iter()
            .cloned()
            .filter(|it| it.dot != 0 || *it == s)
            .collect();

        k
    }

    pub fn canonical_lr0_items(&self) -> Vec<ItemSet> {
        let s = self.augmented_start().lr_item(); // S' -> ·S
        let mut c = vec![self.closure(s.singleton())];

        'fix: loop {
            let old_c = c.clone();

            for i in &old_c {
                for x in self.symbols() {
                    let goto = self.goto(i, x);

                    if goto.is_empty() {
                        continue;
                    }

                    if !c.contains(&goto) {
                        c.push(goto);
                    }
                }
            }

            if c == old_c {
                break 'fix;
            }
        }

        c
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

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write productions.
        for rule in &self.productions {
            writeln!(f, "{}", rule)?;
        }

        writeln!(f, "Start symbol: {:?}", self.start_sym)?;

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

pub fn pretty_print_set<P>(set: &HashSet<P>)
where
    P: fmt::Display,
{
    print!("{{");
    for (i, x) in set.iter().enumerate() {
        if i != 0 {
            print!(", ");
        }
        print!("{}", x);
    }
    print!("}}");
}
