// Re-expost the Alphabet trait.
pub use parsley_util::alphabet::Alphabet;
pub use parsley_util::parser::Parser;

// Re-export the Alphabet derive macro.
pub use parsley_derive::Alphabet;

// Export the things that the compiled LR-table refers to, but
// in such a way that it can not be confused for part of the
// "public API".
pub mod internals {
    pub use parsley_util::lr;
    pub use parsley_util::bnf;
}
