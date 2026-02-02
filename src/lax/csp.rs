// A tiny, reusable CSP (Constraint Satisfaction Problem) engine.
//
// This is intentionally minimal: it provides variables with finite domains,
// a few constraint kinds, and a backtracking search that enumerates all solutions.
// The design keeps extension points (new constraints, search heuristics) simple.

pub type VarId = usize;

pub struct Csp<'a> {
    // Candidate values for each variable (indexed by VarId).
    domains: Vec<Vec<usize>>,
    // All constraints in the model; constraints reference variables by VarId.
    constraints: Vec<Constraint<'a>>,
    // Reverse index so we can check only constraints that mention a variable.
    constraints_by_var: Vec<Vec<usize>>,
}

impl<'a> Csp<'a> {
    pub fn new() -> Self {
        Self {
            domains: Vec::new(),
            constraints: Vec::new(),
            constraints_by_var: Vec::new(),
        }
    }

    pub fn add_var(&mut self, domain: Vec<usize>) -> VarId {
        // Register a new variable and its allowed values.
        let id = self.domains.len();
        self.domains.push(domain);
        self.constraints_by_var.push(Vec::new());
        id
    }

    pub fn add_all_different(&mut self, vars: Vec<VarId>) {
        // Enforce pairwise inequality across the given variables.
        self.add_constraint(vars, ConstraintKind::AllDifferent);
    }

    pub fn add_predicate<F>(&mut self, vars: Vec<VarId>, predicate: F)
    where
        F: Fn(&[Option<usize>]) -> bool + 'a,
    {
        // Add an n-ary constraint that inspects the partial assignment.
        self.add_constraint(vars, ConstraintKind::Predicate(Box::new(predicate)));
    }

    pub fn add_constraint(&mut self, vars: Vec<VarId>, kind: ConstraintKind<'a>) {
        // Store the constraint and index it by each referenced variable.
        if matches!(kind, ConstraintKind::AllDifferent) && vars.len() <= 1 {
            return;
        }
        let idx = self.constraints.len();
        self.constraints.push(Constraint { vars, kind });
        for &var in &self.constraints[idx].vars {
            self.constraints_by_var[var].push(idx);
        }
    }

    pub fn solve_all(&self) -> Vec<Vec<usize>> {
        // Depth-first search over assignments; enumerates all solutions.
        let mut solutions = Vec::new();
        let mut assignment = vec![None; self.domains.len()];
        self.backtrack(&mut assignment, &mut solutions);
        solutions
    }

    fn backtrack(&self, assignment: &mut [Option<usize>], solutions: &mut Vec<Vec<usize>>) {
        let Some(var) = self.select_unassigned_var(assignment) else {
            // All variables assigned: record a concrete solution.
            let solution = assignment
                .iter()
                .map(|value| value.expect("all variables assigned"))
                .collect();
            solutions.push(solution);
            return;
        };

        for &value in &self.domains[var] {
            assignment[var] = Some(value);
            if self.consistent(var, assignment) {
                self.backtrack(assignment, solutions);
            }
            assignment[var] = None;
        }
    }

    // Smallest-domain-first heuristic (MRV) to fail fast.
    fn select_unassigned_var(&self, assignment: &[Option<usize>]) -> Option<VarId> {
        let mut best: Option<(usize, VarId)> = None;
        for (var, value) in assignment.iter().enumerate() {
            if value.is_some() {
                continue;
            }
            let domain_len = self.domains[var].len();
            match best {
                None => best = Some((domain_len, var)),
                Some((best_len, _)) if domain_len < best_len => best = Some((domain_len, var)),
                _ => {}
            }
        }
        best.map(|(_, var)| var)
    }

    fn consistent(&self, var: VarId, assignment: &[Option<usize>]) -> bool {
        // Check only constraints that mention the last-assigned variable.
        for &constraint_idx in &self.constraints_by_var[var] {
            if !self.constraints[constraint_idx].is_consistent(assignment) {
                return false;
            }
        }
        true
    }
}

struct Constraint<'a> {
    vars: Vec<VarId>,
    kind: ConstraintKind<'a>,
}

pub enum ConstraintKind<'a> {
    AllDifferent,
    Predicate(Box<dyn Fn(&[Option<usize>]) -> bool + 'a>),
}

impl<'a> Constraint<'a> {
    fn is_consistent(&self, assignment: &[Option<usize>]) -> bool {
        match &self.kind {
            ConstraintKind::AllDifferent => {
                // Check that all assigned values are distinct (partial check is OK).
                for i in 0..self.vars.len() {
                    let Some(left) = assignment[self.vars[i]] else {
                        continue;
                    };
                    for j in (i + 1)..self.vars.len() {
                        let Some(right) = assignment[self.vars[j]] else {
                            continue;
                        };
                        if left == right {
                            return false;
                        }
                    }
                }
                true
            }
            ConstraintKind::Predicate(predicate) => predicate(assignment),
        }
    }
}
