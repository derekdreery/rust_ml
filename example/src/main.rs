use rust_ml::rust_ml;

rust_ml! {
    add1 :: u8 -> u8
    add1 a = a + 1
}

fn main() {
    assert_eq!(add1(3), 4);
}
