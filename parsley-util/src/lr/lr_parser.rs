use crate::alphabet::Alphabet;
use crate::lr::{Action::*, LrTable, Reduction, State};
use ansi_term::Color;

#[derive(Debug)]
pub enum Tree<A: Alphabet> {
    Leaf(&'static str, A),
    Internal(&'static str, Vec<Tree<A>>),
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct LrParser<A: Alphabet, const K: usize, const M: usize, const N: usize, const P: usize>
where
    A: 'static,
{
    pub(crate) table: &'static LrTable<A, K, M, N, P>, // The underlying LR-table
    pub(crate) states: Vec<usize>,                     // Stack of states
    pub(crate) symbols: Vec<Tree<A>>,                  // Stack of the tree in progress
    pub(crate) input: Vec<A>,                          // The input tokens
    pub(crate) position: usize,                        // Position in the token stream
    pub(crate) trace: bool,
}

// note: if we impose the restriction that the ast-stack may only have 32 items,
// we can create a parser with zero allocation
//  stack = [0; 256], stack_top = 0
//  symbols: [Tree<A>; 32] = Default::default(), symbolst_top = 0
// i think this is very easily doable. no ast-node can have arbitrary # of children.
// "lists" are actually "unbalanced trees" / linked lists, and its very unlikely that
// a single rule will have upwards of 20 symbols in its rule.
// the input token stream can be taken by reference.

impl<A: Alphabet + 'static, const K: usize, const M: usize, const N: usize, const P: usize>
    LrParser<A, K, M, N, P>
{
    pub fn trace(mut self) -> Self {
        self.trace = true;
        self
    }

    fn top(&self) -> &State<M, N> {
        let index = self.states.last().unwrap();
        &self.table.states[*index]
    }

    fn pop_syms(&mut self, n: usize) -> Vec<Tree<A>> {
        // Remove n symbols from the symbol stack.
        let new_top = self.symbols.len() - n;
        self.states.drain(new_top + 1..);
        self.symbols.drain(new_top..).collect()
    }

    /// Reduces A -> β given n = |β| and i = i(A).
    fn reduce(&mut self, n: usize, i: usize) {
        // Pop n = |β| symbols off the stack.
        let rhs = self.pop_syms(n);
        // Push A.
        let sym = self.table.nonterminals[i];
        self.symbols.push(Tree::Internal(sym, rhs));
    }

    pub fn parse(mut self) -> Tree<A> {
        // Get an owned iterator without preventing borrowing self.
        let mut input = std::mem::replace(&mut self.input, vec![]).into_iter();

        // Put the automaton in the start state.
        self.states.push(self.table.start_state);

        // Parsing loop.
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
                        print_trace_step(
                            &self,
                            i,
                            a,
                            format!("S{} {}", t, self.table.terminals[a]),
                        );
                    }
                    // Push state t onto the stack.
                    self.states.push(t);

                    // Create a new AST node for the input token.
                    self.symbols
                        .push(Tree::Leaf(self.table.terminals[a], lookahead.unwrap()));

                    // Move to the next token.
                    lookahead = input.next();
                }
                Reduce(r) => {
                    // Get reduction r = A -> β from the table.
                    let Reduction {
                        ord,
                        sym,
                        description,
                    } = &self.table.reductions[r];

                    if self.trace {
                        print_trace_step(&self, i, a, format!("R{} {}", r, description));
                    }

                    // Pop β and push A.
                    // Recall: Reducing involves popping not just the symbols
                    // β, but also the states associated with them, so we expose
                    // an older state t in which an A was expected, i. e. the
                    // state has a goto-entry for A.
                    self.reduce(*ord, *sym);
                    let t = self.top();

                    // The goto-entry determines the next state.
                    if let Some(i) = t.gotos[*sym] {
                        self.states.push(i);
                    } else {
                        panic!("corrupt table, reduce without goto")
                    }
                }
                Accept => {
                    if self.trace {
                        print_trace_step(&self, i, a, "Accept");
                    }
                    break;
                }
                Error => {
                    panic!("todo: error recovery")
                }
            }
        }
        return self.symbols.pop().unwrap();
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
