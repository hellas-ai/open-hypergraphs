#[derive(Debug, Clone)]
pub struct UnionFind {
    parent: Vec<usize>,
    size: Vec<usize>,
    history: Vec<HistoryEntry>,
    components: usize,
}

#[derive(Debug, Clone)]
enum HistoryEntry {
    Noop,
    Merge {
        root: usize,
        parent: usize,
        size_parent: usize,
    },
}

impl UnionFind {
    pub fn new(n: usize) -> Self {
        Self {
            parent: (0..n).collect(),
            size: vec![1; n],
            history: Vec::new(),
            components: n,
        }
    }

    pub fn len(&self) -> usize {
        self.parent.len()
    }

    pub fn components(&self) -> usize {
        self.components
    }

    pub fn find(&mut self, x: usize) -> usize {
        let mut node = x;
        while self.parent[node] != node {
            node = self.parent[node];
        }
        node
    }

    pub fn union(&mut self, x: usize, y: usize) {
        let root_x = self.find(x);
        let root_y = self.find(y);
        if root_x == root_y {
            self.history.push(HistoryEntry::Noop);
            return;
        }

        let (root, parent) = if self.size[root_x] >= self.size[root_y] {
            (root_y, root_x)
        } else {
            (root_x, root_y)
        };

        self.history.push(HistoryEntry::Merge {
            root,
            parent,
            size_parent: self.size[parent],
        });

        self.parent[root] = parent;
        self.size[parent] += self.size[root];
        self.components -= 1;
    }

    pub fn snapshot(&self) -> usize {
        self.history.len()
    }

    pub fn rollback(&mut self, snapshot: usize) {
        while self.history.len() > snapshot {
            match self.history.pop().expect("rollback history") {
                HistoryEntry::Noop => {}
                HistoryEntry::Merge {
                    root,
                    parent,
                    size_parent,
                } => {
                    self.parent[root] = root;
                    self.size[parent] = size_parent;
                    self.components += 1;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::UnionFind;

    #[test]
    fn union_find_unions_and_connects() {
        let mut uf = UnionFind::new(4);
        assert_eq!(uf.components(), 4);
        uf.union(0, 1);
        uf.union(2, 3);
        assert_eq!(uf.components(), 2);
        uf.union(1, 2);
        assert_eq!(uf.components(), 1);
    }

    #[test]
    fn union_find_snapshot_and_rollback() {
        let mut uf = UnionFind::new(3);
        uf.union(0, 1);
        let snap = uf.snapshot();
        uf.union(1, 2);
        assert_eq!(uf.components(), 1);
        uf.rollback(snap);
        assert_eq!(uf.components(), 2);
        uf.union(0, 2);
        assert_eq!(uf.components(), 1);
    }
}
