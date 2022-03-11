use crate::bnf::{Symbol, Grammar, Production};


/// An LR-item is a production with a dot in it to
/// indicate the state of the stack. The position of the 
/// dot is indicated by an index into the productions
/// list of grammar symbols.
pub(crate) struct LrItem {
    production: Production,
    dot: usize,
}
