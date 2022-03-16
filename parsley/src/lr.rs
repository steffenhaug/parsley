use crate::bnf::{Production, Symbol::*};
use ansi_term::Color;
use std::fmt;
use std::collections::HashSet;

/// An LR-item is a production with a dot in it to
/// indicate the state of the stack. The position of the
/// dot is indicated by an index into the productions
/// list of grammar symbols.
#[derive(Hash, PartialEq, Eq, Clone)]
pub struct Item {
    pub(crate) production: Production,
    pub(crate) dot: usize,
}

/// An item-set is symple a set of LR-items.
pub type ItemSet = HashSet<Item>;

impl Item {
    pub fn singleton(self) -> ItemSet {
        ItemSet::from([self])
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
                production: Production { symbol, recipe: vec!() },
                dot: 0
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
    pub actions: [Action; M],
    pub gotos: [Action; N]
}

#[allow(dead_code)] // This will be compiled by a proc macro.
pub struct LrTable<const K: usize, const M: usize, const N: usize> {
    pub states: [State<M, N>; K]
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
