use crate::bnf::{
    Production,
    Symbol::{self, *},
};
use ansi_term::Color;
use std::collections::HashSet;
use std::fmt;

/// An LR-item is a production with a dot in it to
/// indicate the state of the stack. The position of the
/// dot is indicated by an index into the productions
/// list of grammar symbols.
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct Item {
    pub production: Production,
    pub dot: usize,
}

/// An item-set is symple a set of LR-items.
pub type ItemSet = HashSet<Item>;

pub fn describe(its: &ItemSet) -> String {
    let mut description = String::new();
    let n = its.len();

    description.push_str("{");
    for (i, item) in its.into_iter().enumerate() {
        description.push_str(&item.to_string());

        if i != n - 1 {
            description.push_str(", ");
        }
    }
    description.push_str("}");

    description
}

impl Item {
    pub fn singleton(self) -> ItemSet {
        ItemSet::from([self])
    }

    pub fn has_terminal_after_dot(&self) -> bool {
        // If the dot is at the end, clearly there is no terminal after the dot.
        if self.is_reducing() {
            return false;
        }
        let after_dot = &self.production.recipe[self.dot];
        matches!(after_dot, Terminal(_))
    }

    pub fn after_dot_unchecked(&self) -> &Symbol {
        &self.production.recipe[self.dot]
    }

    pub fn is_reducing(&self) -> bool {
        self.dot >= self.production.recipe.len()
    }

    pub fn of(dot: usize, pr: &Production) -> Self {
        let symbol = pr.symbol.clone();

        if pr.recipe == [Epsilon] {
            // A -> ε only gives the item A -> ·
            // Note: Here dot == len, so it indicates reducing, as it should.
            //
            // FIXME: I tihnk we could avoid this special case if we dont encode
            // nullable productions with [ε] but instead empty string [].
            return Item {
                production: Production {
                    symbol,
                    recipe: vec![],
                },
                dot: 0,
            };
        }

        Item {
            production: pr.clone(),
            dot,
        }
    }
}

#[derive(Debug)]
pub enum Action {
    Goto(usize),   // number is next state
    Reduce(usize), // number is reduction rule index
    Shift(usize),  // number is next state
    Accept,
    Error,
}

#[derive(Debug)]
pub struct Reduction {
    pub ord: usize,
    pub sym: &'static str,
    pub description: &'static str,
}

#[derive(Debug)]
pub struct State<const M: usize, const N: usize> {
    pub actions: [Action; M],
    pub gotos: [Action; N],
    pub description: &'static str,
}

pub struct LrTable<const K: usize, const M: usize, const N: usize, const P: usize> {
    // Symbol tables.
    pub terminals: [&'static str; M],
    pub nonterminals: [&'static str; N],
    // LR parsing table.
    pub reductions: [Reduction; P],
    pub states: [State<M, N>; K],
    pub start_state: usize,
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ->", self.production.symbol)?;

        for (i, sym) in self.production.recipe.iter().enumerate() {
            if self.dot == i {
                write!(f, "{}", Color::Yellow.paint(" ·"))?;
            }
            write!(f, " {}", sym)?;
        }

        if self.dot == self.production.recipe.len() {
            write!(f, "{}", Color::Yellow.paint(" ·"))?;
        }

        Ok(())
    }
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Action::Accept => write!(f, "ACC"),
            // Inserting some dots in the table instead of leaving cells empty
            // makes it easier to read, but keeps it relatively clean.
            Action::Error => write!(f, "·"),
            Action::Reduce(p) => write!(f, "R{}", p),
            Action::Shift(j) => write!(f, "S{}", j),
            Action::Goto(a) => write!(f, "G{}", a),
        }
    }
}

impl<const K: usize, const M: usize, const N: usize, const P: usize> fmt::Display
    for LrTable<K, M, N, P>
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Write out the productions:
        writeln!(f, "Grammar rules:")?;
        for (i, re) in self.reductions.iter().enumerate() {
            writeln!(f, "({}) {:<25} |β| = {}", i, re.description, re.ord)?;
        }

        // Print the first header row:
        write!(f, "{:<5}  ", "State")?;
        write!(f, "{:<10}", "Action")?;
        for _ in 0..M - 2 {
            write!(f, "{:<5}", "")?;
        }
        write!(f, " {:<5}", "Goto")?;
        for _ in 0..N - 1 {
            write!(f, "{:<5}", "")?;
        }
        writeln!(f, " Kernel")?;

        // Print the second header row (with symbol identification).
        write!(f, "{:<5}  ", "")?;
        for i in 0..M {
            write!(f, "{:<5}", self.terminals[i])?;
        }
        write!(f, " ")?;
        for i in 0..N {
            write!(f, "{:<5}", self.nonterminals[i])?;
        }
        writeln!(f, " ")?;

        // Print the table itself.
        for (i, state) in self.states.iter().enumerate() {
            // Mark the start state.
            if i == self.start_state {
                write!(f, "{:>5}  ", format!("*{}", i))?;
            } else {
                write!(f, "{:>5}  ", i)?;
            }

            for ac in &state.actions {
                if let Action::Error = ac {
                    // Color the entry red:
                    let entry = Color::Red.paint(format!("{:<5}", ac.to_string()));
                    write!(f, "{}", entry)?;
                } else {
                    write!(f, "{:<5}", ac.to_string())?;
                }
            }
            write!(f, " ")?;
            for g in &state.gotos {
                if let Action::Error = g {
                    // Color the entry red:
                    let entry = Color::Red.paint(format!("{:<5}", g.to_string()));
                    write!(f, "{}", entry)?;
                } else {
                    write!(f, "{:<5}", g.to_string())?;
                }
            }
            write!(f, " {}", state.description)?;

            if i != self.states.len() - 1 {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}
