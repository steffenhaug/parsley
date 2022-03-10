/// Trait to describe that a type is suitable for use as an
/// underlying alphabet for a grammar.

pub trait Alphabet {
    fn lr_id_from_str(terminal: &str) -> Option<usize>;
    fn lr_id(&self) -> Option<usize>;
}
