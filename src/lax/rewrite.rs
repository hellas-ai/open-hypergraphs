use crate::lax::{Arrow, Hypergraph, LaxHypergraphArrow};

/// A rewrite rule as a span `L <- K -> R` of lax morphisms.
pub struct Rule<O, A> {
    pub lhs: Hypergraph<O, A>,
    pub interface: Hypergraph<O, A>,
    pub rhs: Hypergraph<O, A>,
    pub left: LaxHypergraphArrow<O, A>,
    pub right: LaxHypergraphArrow<O, A>,
}

impl<O: Clone + PartialEq, A: Clone + PartialEq> Rule<O, A> {
    /// Construct a rule and validate its structural properties.
    pub fn new(
        lhs: Hypergraph<O, A>,
        interface: Hypergraph<O, A>,
        rhs: Hypergraph<O, A>,
        left: LaxHypergraphArrow<O, A>,
        right: LaxHypergraphArrow<O, A>,
    ) -> Self {
        Rule {
            lhs,
            interface,
            rhs,
            left,
            right,
        }
        .validate()
    }

    /// Validate the span structure `L <- K -> R`.
    pub fn validate(self) -> Self {
        if self.left.source != self.interface {
            panic!("rule left morphism source does not match interface");
        }
        if self.left.target != self.lhs {
            panic!("rule left morphism target does not match lhs");
        }
        if self.right.source != self.interface {
            panic!("rule right morphism source does not match interface");
        }
        if self.right.target != self.rhs {
            panic!("rule right morphism target does not match rhs");
        }

        self
    }
}

/// A rule-specific match `L -> G`, carrying the rule and the morphism.
pub struct Match<O, A> {
    pub rule: Rule<O, A>,
    pub morphism: LaxHypergraphArrow<O, A>,
}

impl<O: Clone + PartialEq, A: Clone + PartialEq> Match<O, A> {
    /// Construct a match without validation.
    pub fn new(rule: Rule<O, A>, morphism: LaxHypergraphArrow<O, A>) -> Self {
        Match { rule, morphism }.validate()
    }

    /// Validate a match against identification and dangling conditions.
    pub fn validate(self) -> Self {
        let l = &self.rule.left;
        let m = &self.morphism;
        let g = &m.target;

        if l.w.target() != self.rule.lhs.nodes.len() {
            panic!(
                "rule left map target size mismatch: got {}, expected {}",
                l.w.target(),
                self.rule.lhs.nodes.len()
            );
        }
        if m.w.source() != self.rule.lhs.nodes.len() {
            panic!(
                "match map source size mismatch: got {}, expected {}",
                m.w.source(),
                self.rule.lhs.nodes.len()
            );
        }
        if m.w.target() != g.nodes.len() {
            panic!(
                "match map target size mismatch: got {}, expected {}",
                m.w.target(),
                g.nodes.len()
            );
        }
        if m.x.target() != g.edges.len() {
            panic!(
                "match edge map target size mismatch: got {}, expected {}",
                m.x.target(),
                g.edges.len()
            );
        }
        if !self.identification_condition() {
            panic!("match violates the identification condition");
        }
        if !self.dangling_condition() {
            panic!("match violates the dangling condition");
        }
        self
    }

    fn identification_condition(&self) -> bool {
        // Identification fails if x,y ∈ L \ l(K) and m(x) = m(y).
        let l = &self.rule.left;
        let m = &self.morphism;

        let mut in_image = vec![false; self.rule.lhs.nodes.len()];
        for i in 0..l.w.source() {
            let idx = l.w.table[i];
            in_image[idx] = true;
        }

        let mut seen = vec![None; m.w.target()];
        for i in 0..self.rule.lhs.nodes.len() {
            if in_image[i] {
                continue;
            }
            let img = m.w.table[i];
            if let Some(existing) = seen[img] {
                if existing != i {
                    return false;
                }
            } else {
                seen[img] = Some(i);
            }
        }

        true
    }

    fn dangling_condition(&self) -> bool {
        // Dangling fails if a vertex in L \ l(K) is incident to any hyperedge in G
        // outside the image of m(L).
        let l = &self.rule.left;
        let m = &self.morphism;
        let g = &m.target;

        let mut in_l_image = vec![false; self.rule.lhs.nodes.len()];
        for i in 0..l.w.source() {
            let idx = l.w.table[i];
            in_l_image[idx] = true;
        }

        let mut forbidden_nodes = vec![false; g.nodes.len()];
        for i in 0..self.rule.lhs.nodes.len() {
            if in_l_image[i] {
                continue;
            }
            let img = m.w.table[i];
            forbidden_nodes[img] = true;
        }

        let mut edge_in_image = vec![false; g.edges.len()];
        for i in 0..m.x.source() {
            let idx = m.x.table[i];
            edge_in_image[idx] = true;
        }

        for (edge_id, edge) in g.adjacency.iter().enumerate() {
            if edge_in_image[edge_id] {
                continue;
            }
            let touches_forbidden = edge
                .sources
                .iter()
                .chain(edge.targets.iter())
                .any(|n| forbidden_nodes[n.0]);
            if touches_forbidden {
                return false;
            }
        }

        true
    }
}
