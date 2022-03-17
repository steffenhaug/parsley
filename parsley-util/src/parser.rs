/// Trait to encapsulate parsing operation. A parser consumes
/// itself in the process, so it can be able to store the
/// partial AST internally while constructing it, and hand off
/// ownership in the end.
///
/// Think of a parser as a thin wrapper around he token stream.
/// In other words, if the parser requires something expensive
/// to calculate, like a LR table, consider not giving the parser
/// ownership over this table, but share it between parsers.
pub trait Parser<T> {
    type Error;
    fn parse(self) -> Result<T, Self::Error>;
}
