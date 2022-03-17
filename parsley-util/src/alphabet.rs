/// Trait to describe that a type is suitable for use as an
/// underlying alphabet for a grammar.
pub trait Alphabet {
    /// Gives a number identifying the symbol uniquely.
    fn id(&self) -> usize;
}
