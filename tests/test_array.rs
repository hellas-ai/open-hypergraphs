use open_hypergraphs::array::{vec::*, *};

#[test]
fn test_segmented_sum() {
    let x = VecArray(vec![1, 2, 3, 4, 5, 6]);
    let segments = VecArray(vec![2, 4]);
    let actual = segments.segmented_sum(&x);
    let expected = VecArray(vec![1 + 2, 3 + 4 + 5 + 6]);
    assert_eq!(actual, expected);
}
