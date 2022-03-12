/// Trait to describe that a type is suitable for use as an
/// underlying alphabet for a grammar.

pub trait Alphabet {
    // Derive macro impls these by calculating a table of
    // LR ids identifying a grammar symbol in the LR table.
    fn lr_id_from_str(terminal: &str) -> Option<usize>;
    fn lr_id(&self) -> Option<usize>;
    fn n_lr_ids(&self) -> usize;

    // Defaults cna be implemented u
    fn lr_ids<S>(&self) -> std::ops::Range<usize> {
        0..self.n_lr_ids()
    }
}
