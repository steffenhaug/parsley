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

        if i != n-1 {
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
        if self.dot >= self.production.recipe.len() {
            return false;
        }
        let after_dot = &self.production.recipe[self.dot];
        matches!(after_dot, Terminal(_))
    }

    pub fn after_dot_unchecked(&self) -> &Symbol {
        &self.production.recipe[self.dot]
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
    Goto(usize),
    Reduce(usize),
    Shift(usize),
    Accept,
    Error,
}

#[allow(dead_code)] // This will be compiled by a proc macro.
#[derive(Debug)]
pub struct State<const M: usize, const N: usize> {
    pub state: usize,
    pub actions: [Action; M],
    pub gotos: [Action; N],
    pub description: &'static str,
}

#[allow(dead_code)] // This will be compiled by a proc macro.
pub struct LrTable<const K: usize, const M: usize, const N: usize> {
    pub states: [State<M, N>; K],
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
            Action::Error => write!(f, ""),
            Action::Reduce(_) => write!(f, "R"),
            Action::Shift(j) => write!(f, "S{}", j),
            Action::Goto(a) => write!(f, "G{}", a),
        }
    }
}

impl<const K: usize, const M: usize, const N: usize> fmt::Display for LrTable<K, M, N> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:<4} | ", "Stat")?;
        write!(f, "{:<7}", "Action")?;
        for _ in 0..M - 1 {
            write!(f, "{:<7}", "")?;
        }
        write!(f, "| {:<7}", "Goto")?;
        for _ in 0..N-1 {
            write!(f, "{:<7}", "")?;
        }
        writeln!(f, "| Note")?;

        for state in &self.states {
            write!(f, "{:>4} | ", state.state)?;
            for ac in &state.actions {
                write!(f, "{:>7}", ac.to_string())?;
            }
            write!(f, "| ")?;
            for g in &state.gotos {
                write!(f, "{:>7}", g.to_string())?;
            }
            writeln!(f, "| {}", state.description)?;
        }

        Ok(())
    }
}
