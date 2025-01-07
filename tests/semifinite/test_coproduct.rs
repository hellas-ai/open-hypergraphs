use open_hypergraphs::array::{vec::*, *};
use open_hypergraphs::semifinite::*;

use super::strategy::*;
use proptest::{prop_assert_ne, proptest};

#[test]
fn test_coproduct() {
    let f = SemifiniteFunction::<VecKind, &str>(VecArray(vec!["a", "b"]));
    let g = SemifiniteFunction::<VecKind, &str>(VecArray(vec!["c", "d", "e"]));

    let actual: SemifiniteFunction<VecKind, &str> = f + g;

    let v = vec!["a", "b", "c", "d", "e"];
    let expected = SemifiniteFunction::<VecKind, &str>(VecArray(v));

    assert_eq!(actual, expected);
}

proptest! {
    #[test]
    fn semifinite_nonempty(f in semifinite_strategy::<String>(None,true)) {
        prop_assert_ne!(f.len(),0);
    }
}
