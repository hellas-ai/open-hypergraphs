use crate::union_find::UnionFind;

pub struct PartitionInput<T> {
    pub elements: Vec<T>,
    // class_ids identify the class for each element.
    // We search for partitions whose induced relation, together with the class relation,
    // generates the total relation (i.e. everything becomes connected).
    // We keep per-block class lists so we only union each class once per block,
    // avoiding redundant unions when multiple elements in the same class land together.
    pub class_ids: Vec<usize>,
    pub class_count: usize,
}

pub struct Partition<T> {
    pub blocks: Vec<PartitionBlock<T>>,
}

pub struct PartitionBlock<T> {
    pub elements: Vec<T>,
}

struct BlockState<T> {
    elements: Vec<T>,
    classes: Vec<usize>,
}

pub fn enumerate_partitions<T: Clone>(input: &PartitionInput<T>) -> Vec<Partition<T>> {
    let mut results = Vec::new();
    let mut blocks: Vec<BlockState<T>> = Vec::new();
    let mut uf = UnionFind::new(input.class_count);

    fn all_connected(uf: &UnionFind) -> bool {
        uf.components() == 1
    }

    fn walk<T: Clone>(
        idx: usize,
        input: &PartitionInput<T>,
        blocks: &mut Vec<BlockState<T>>,
        uf: &mut UnionFind,
        results: &mut Vec<Partition<T>>,
    ) {
        if idx == input.elements.len() {
            if all_connected(uf) {
                let blocks = blocks
                    .iter()
                    .map(|b| PartitionBlock {
                        elements: b.elements.clone(),
                    })
                    .collect();
                results.push(Partition { blocks });
            }
            return;
        }

        let element = input.elements[idx].clone();
        let class_id = input.class_ids[idx];

        for i in 0..blocks.len() {
            let snap = uf.snapshot();
            let (elements_len, classes_len) = {
                let block = &mut blocks[i];
                let elements_len = block.elements.len();
                let classes_len = block.classes.len();

                block.elements.push(element.clone());
                if !block.classes.contains(&class_id) {
                    if let Some(&rep) = block.classes.first() {
                        uf.union(rep, class_id);
                    }
                    block.classes.push(class_id);
                }

                (elements_len, classes_len)
            };

            walk(idx + 1, input, blocks, uf, results);

            uf.rollback(snap);
            let block = &mut blocks[i];
            block.elements.truncate(elements_len);
            block.classes.truncate(classes_len);
        }

        blocks.push(BlockState {
            elements: vec![element],
            classes: vec![class_id],
        });
        walk(idx + 1, input, blocks, uf, results);
        blocks.pop();
    }

    walk(0, input, &mut blocks, &mut uf, &mut results);
    results
}

#[cfg(test)]
mod tests {
    use super::{enumerate_partitions, PartitionInput};

    fn normalize(partition: &super::Partition<usize>) -> Vec<Vec<usize>> {
        let mut blocks = partition
            .blocks
            .iter()
            .map(|block| {
                let mut elems = block.elements.clone();
                elems.sort();
                elems
            })
            .collect::<Vec<_>>();
        blocks.sort();
        blocks
    }

    #[test]
    fn partitions_single_class_allows_all_partitions() {
        let input = PartitionInput {
            elements: vec![0, 1],
            class_ids: vec![0, 0],
            class_count: 1,
        };

        let partitions = enumerate_partitions(&input);
        assert_eq!(partitions.len(), 2);
    }

    #[test]
    fn partitions_two_classes_require_connection() {
        let input = PartitionInput {
            elements: vec![0, 1],
            class_ids: vec![0, 1],
            class_count: 2,
        };

        let partitions = enumerate_partitions(&input);
        assert_eq!(partitions.len(), 1);
        assert_eq!(normalize(&partitions[0]), vec![vec![0, 1]]);
    }

    #[test]
    fn partitions_three_elements_two_classes() {
        let input = PartitionInput {
            elements: vec![0, 1, 2],
            class_ids: vec![0, 0, 1],
            class_count: 2,
        };

        let partitions = enumerate_partitions(&input);
        assert_eq!(partitions.len(), 3);

        let mut actual = partitions.iter().map(normalize).collect::<Vec<_>>();
        actual.sort();

        let mut expected = vec![
            vec![vec![0, 1, 2]],
            vec![vec![0, 2], vec![1]],
            vec![vec![1, 2], vec![0]],
        ]
        .into_iter()
        .map(|blocks| {
            let mut blocks = blocks
                .into_iter()
                .map(|mut block| {
                    block.sort();
                    block
                })
                .collect::<Vec<_>>();
            blocks.sort();
            blocks
        })
        .collect::<Vec<_>>();
        expected.sort();

        assert_eq!(actual, expected);
    }
}
