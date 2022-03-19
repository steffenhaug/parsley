use crate::alphabet::Alphabet;
use crate::lr::{Action::*, LrTable, State};
use crate::parser::Parser;
use ansi_term::Color;

#[derive(Debug)]
pub enum Tree<A: Alphabet> {
    Leaf(&'static str, A),
    Internal(&'static str, Vec<Tree<A>>),
}

/// LR Parsing struct.
/// Contains a reference to an LR table, and the necessary stacks to
/// parse an input stream using the LR parsing algorithm. Generally,
/// the reference to the LR table will be &'static, but that is not
/// _technically_ absolutely required.
#[allow(dead_code)]
#[derive(Debug)]
pub struct LrParser<'a, A: Alphabet, const K: usize, const M: usize, const N: usize, const P: usize>
{
    pub(crate) table: &'a LrTable<A, K, M, N, P>, // The underlying LR-table
    pub(crate) states: Vec<usize>,                // Stack of states
    pub(crate) symbols: Vec<Tree<A>>,             // Stack of the tree in progress
    pub(crate) input: Vec<A>,                     // The input tokens
    pub(crate) trace: bool,                       // Toggle printing stack trace
}

// note: if we impose the restriction that the ast-stack may only have 32 items,
// we can create a parser with zero allocation
//  stack = [0; 256], stack_top = 0
//  symbols: [Tree<A>; 32] = Default::default(), symbolst_top = 0
// i think this is very easily doable. no ast-node can have arbitrary # of children.
// "lists" are actually "unbalanced trees" / linked lists, and its very unlikely that
// a single rule will have upwards of 20 symbols in its rule.
// the input token stream can be taken by reference.

impl<'a, A: Alphabet, const K: usize, const M: usize, const N: usize, const P: usize>
    LrParser<'a, A, K, M, N, P>
{
    pub fn trace(mut self) -> Self {
        self.trace = true;
        self
    }

    fn top(&self) -> &State<M, N> {
        let index = self.states.last().unwrap();
        &self.table.states[*index]
    }

    /// Pops off symbols and states from the parsers stacks corresponding
    /// to a production A -> β, given n = |β|.
    fn pop(&mut self, n: usize) -> Vec<Tree<A>> {
        // Recall: Reducing involves popping not just the symbols
        // β, but also the states associated with them, so we expose
        // an older state t in which an A was expected, i. e. the
        // state has a goto-entry for A.
        //
        // At all times, the following invariant is maintained:
        // There are exactly 1 more states on the stack than symbols.
        // Proof:
        //  Initially, there is 1 state, the start state, and zero symbols.
        //  When shifting, we add 1 symbol and 1 state, thus maintaining the invariant
        //  When reducing, we remove K symbols and K states, thus maintaining the invariant
        // In other words, it is always true to say that the state stack
        // should be drained to have 1 more element than the symbol stack.
        let new_top = self.symbols.len() - n;
        self.states.drain(1 + new_top..);
        self.symbols.drain(new_top..).collect()
    }

    /// Reduces A -> β given n = |β| and i = i(A).
    fn reduce(&mut self, n: usize, i: usize) {
        // Pop n = |β| symbols off the stack.
        let rhs = self.pop(n);
        // Push A.
        let sym = self.table.nonterminals[i];
        self.symbols.push(Tree::Internal(sym, rhs));
    }

    fn shift(&mut self, state: usize, sym: usize, token: A) {
        self.states.push(state);
        let sym = Tree::Leaf(self.table.terminals[sym], token);
        self.symbols.push(sym);
    }
}

impl<'a, A: Alphabet, const K: usize, const M: usize, const N: usize, const P: usize>
    Parser<Tree<A>> for LrParser<'a, A, K, M, N, P>
{
    type Error = ();

    fn parse(mut self) -> Result<Tree<A>, Self::Error> {
        // Get an owned iterator without preventing borrowing self.
        let mut input = std::mem::replace(&mut self.input, vec![]).into_iter();

        // Put the automaton in the start state.
        self.states.push(self.table.start_state);

        if self.trace {
            println!("LR-Parser stack trace:");
        }

        let mut lookahead = input.next();

        for i in 0.. {
            let s = self.top();

            // Get the ID of the symbol in the lookahead.
            let a = lookahead.as_ref().map_or(A::eof(), |a| a.id());

            // Select an action.
            match s.actions[a] {
                Shift(t) => {
                    if self.trace {
                        let info = format!("S{} {}", t, self.table.terminals[a]);
                        print_trace_step(&self, i, a, info);
                    }

                    // Shift the lookahead symbol to the stack.
                    // Note: Since a shift action was chosen, lookahead maps to
                    // a real token ID, so it is safe to unwrap.
                    self.shift(t, a, lookahead.unwrap());

                    // Move to the next token.
                    lookahead = input.next();
                }
                Reduce(r) => {
                    // Get rule Rₙ = A -> β from the parsers table of reductions.
                    let rn = &self.table.reductions[r];

                    if self.trace {
                        let info = format!("R{} {}", r, rn.description);
                        print_trace_step(&self, i, a, info);
                    }

                    // Perform the reduction.
                    self.reduce(rn.ord, rn.sym);

                    // This exposes a previous state t on the stack, in which
                    // an A was expected to follow.
                    let t = self.top();

                    // The goto-entry of state t, GOTO(t, A), determines the next state.
                    if let Some(i) = t.gotos[rn.sym] {
                        self.states.push(i);
                    } else {
                        // If this happens, the LR table itself is wrong, indicating
                        // a mistake in the BNF-compiler.
                        panic!("corrupt table: reduce without goto")
                    }
                }
                Accept => {
                    if self.trace {
                        print_trace_step(&self, i, a, "Accept");
                    }

                    // Success - parsing is done.
                    break;
                }
                Error => {
                    // Error - do something better in the future.
                    return Err(());
                }
            }
        }

        // Parsing is done - the AST node corresponding to the root rule
        // will be on the top of the smybol stack. Unwrapping is safe;
        // if the parse was successful, the stack is certainly not empty.
        Ok(self.symbols.pop().expect("true by definition"))
    }
}

fn print_trace_step<A: Alphabet, const K: usize, const M: usize, const N: usize, const P: usize>(
    parser: &LrParser<A, K, M, N, P>,
    step: usize,
    look: usize,
    act: impl AsRef<str>,
) {
    let sym_names = parser
        .symbols
        .iter()
        .map(|t| match t {
            Tree::Leaf(s, _) => *s,
            Tree::Internal(s, _) => *s,
        })
        .collect::<Vec<&str>>();
    print!("{:<5}", step);
    // Print the state stack.
    print!("States: ");
    for (i, state) in parser.states.iter().enumerate() {
        if i != parser.states.len() - 1 {
            print!(" {:<3}", state);
        } else {
            print!(" {:<3}", Color::Yellow.paint(state.to_string()));
        }
    }
    println!();

    // Print the symbol stack:
    print!("     Symbols:");
    for name in sym_names {
        print!(" {:<3}", name);
    }
    println!();

    //
    println!(
        "     Look: {} => Action: {}",
        Color::Yellow.paint(parser.table.terminals[look]),
        Color::Green.paint(act.as_ref())
    );
}
