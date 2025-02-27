use open_hypergraphs::array::{vec::*, *};

#[test]
fn test_segmented_sum() {
    let x = VecArray(vec![1, 2, 3, 4, 5, 6]);
    let segments = VecArray(vec![2, 4]);
    let actual = segments.segmented_sum(&x);
    let expected = VecArray(vec![1 + 2, 3 + 4 + 5 + 6]);
    assert_eq!(actual, expected);
}

// Tests moved from layer.rs

#[test]
fn test_scatter_assign_constant() {
    let mut actual = VecArray(vec![0, 1, 2, 3, 4, 5]);
    let i = VecArray(vec![0, 2, 4]);
    actual.scatter_assign_constant(&i, 10);
    let expected = VecArray(vec![10, 1, 10, 3, 10, 5]);
    assert_eq!(actual, expected);
}

#[test]
fn test_zero() {
    let x = VecArray(vec![0, 1, 0, 2, 0, 3]);
    let actual = x.zero();
    let expected = VecArray(vec![0, 2, 4]);
    assert_eq!(actual, expected);
}

#[test]
fn test_bincount() {
    let input = VecArray(vec![0, 3, 1, 3, 0, 3, 3]);
    let actual = input.bincount(4);
    let expected = VecArray(vec![2, 1, 0, 4]);
    assert_eq!(actual, expected);

    // Test with empty array
    let empty = VecArray(vec![]);
    let actual_empty = empty.bincount(3);
    let expected_empty = VecArray(vec![0, 0, 0]);
    assert_eq!(actual_empty, expected_empty);

    // Test with single element
    let single = VecArray(vec![1]);
    let actual_single = single.bincount(2);
    let expected_single = VecArray(vec![0, 1]);
    assert_eq!(actual_single, expected_single);
}
