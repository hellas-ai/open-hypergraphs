use crate::lax::hypergraph::*;
use crate::lax::open_hypergraph::*;

use std::cell::RefCell;
use std::rc::Rc;

// Given a set of operation labels (i.e., edge labels),
// distinguish one as the type of "Vars".
// A `Var` will typically be a `1 → N` map representing copying.
// However, it can more generally be any `M → N` operation.
pub trait HasVar {
    /// get the label for the "Var" operation
    fn var() -> Self;
}

#[derive(Clone, Debug)]
pub struct Var<O, A> {
    pub state: Rc<RefCell<OpenHypergraph<O, A>>>,
    pub edge_id: EdgeId,
    pub label: O,
}

/// Vars can be created when the set of edge labels has a 'Copy' operation.
impl<O: Clone, A: HasVar> Var<O, A> {
    pub fn new(state: Rc<RefCell<OpenHypergraph<O, A>>>, default_node_label: O) -> Self {
        let (edge_id, _) = state.borrow_mut().new_operation(A::var(), vec![], vec![]);
        Var {
            state,
            edge_id,
            label: default_node_label,
        }
    }

    pub fn new_source(&self) -> NodeId {
        self.state
            .borrow_mut()
            .add_edge_source(self.edge_id, self.label.clone())
    }

    pub fn new_target(&self) -> NodeId {
        self.state
            .borrow_mut()
            .add_edge_target(self.edge_id, self.label.clone())
    }
}
