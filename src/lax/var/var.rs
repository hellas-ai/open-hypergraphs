use crate::lax::hypergraph::*;
use crate::lax::open_hypergraph::*;

use std::sync::{Arc, Mutex};

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
    pub state: Arc<Mutex<OpenHypergraph<O, A>>>,
    pub edge_id: EdgeId,
    pub label: O,
}

/// Vars can be created when the set of edge labels has a 'Copy' operation.
impl<O: Clone, A: HasVar> Var<O, A> {
    pub fn new(state: Arc<Mutex<OpenHypergraph<O, A>>>, default_node_label: O) -> Self {
        let (edge_id, _) = state.lock().unwrap().new_operation(A::var(), vec![], vec![]);
        Var {
            state,
            edge_id,
            label: default_node_label,
        }
    }

    pub fn new_source(&self) -> NodeId {
        self.state
            .lock()
            .unwrap()
            .add_edge_source(self.edge_id, self.label.clone())
    }

    pub fn new_target(&self) -> NodeId {
        self.state
            .lock()
            .unwrap()
            .add_edge_target(self.edge_id, self.label.clone())
    }
}

pub type BuildResult<O, A> = Result<OpenHypergraph<O, A>, Arc<Mutex<OpenHypergraph<O, A>>>>;

/// Construct an [`OpenHypergraph`] from a function taking an empty OpenHypergraph,
/// and returning two lists of [`Var`]s corresponding to *sources* and *targets*.
pub fn build<F, O: Clone, A: HasVar>(f: F) -> BuildResult<O, A>
where
    F: Fn(&Arc<Mutex<OpenHypergraph<O, A>>>) -> (Vec<Var<O, A>>, Vec<Var<O, A>>),
{
    let state = Arc::new(Mutex::new(OpenHypergraph::<O, A>::empty()));
    {
        let (s, t) = f(&state);
        state.lock().unwrap().sources = s.iter().map(|x| x.new_source()).collect();
        state.lock().unwrap().targets = t.iter().map(|x| x.new_target()).collect();
    }
    Arc::try_unwrap(state).map(|f| f.into_inner().unwrap())
}
