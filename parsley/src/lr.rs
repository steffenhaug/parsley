use crate::bnf::{Production, Symbol::*};
use ansi_term::Color;
use std::fmt;
use std::collections::HashSet;

/// An LR-item is a production with a dot in it to
/// indicate the state of the stack. The position of the
/// dot is indicated by an index into the productions
/// list of grammar symbols.
#[derive(Hash, PartialEq, Eq, Clone)]
pub struct LrItem {
    pub(crate) production: Production,
    pub(crate) dot: usize,
}

impl LrItem {
    pub fn of(dot: usize, pr: &Production) -> Self {
        let symbol = pr.symbol.clone();

        if pr.recipe == [Epsilon] {
            // A -> ε only gives the item A -> ·
            // Note: Here dot == len, so it indicates reducing, as it should.
            //
            // FIXME: I tihnk we could avoid this special case if we dont encode
            // nullable productions with [ε] but instead empty string [].
            return LrItem {
                production: Production { symbol, recipe: vec!() },
                dot: 0
            };
        }

        LrItem {
            production: pr.clone(),
            dot,
        }
    }
}

/// An item-set is symple a set of LR-items.
pub type ItemSet = HashSet<LrItem>;

pub enum LrAction {
    Goto(usize),
    Reduce(usize),
    Shift(usize),
    Accept,
}



impl fmt::Display for LrItem {
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
