/// Trait to describe that a type is suitable for use as an
/// underlying alphabet for a grammar.
pub trait Alphabet {
    fn id(&self) -> usize;
}
