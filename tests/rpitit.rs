#![cfg(feature = "auto_enums")]
use std::sync::Arc;

use enum_dispatch::enum_dispatch;

#[enum_dispatch]
trait Trait {
    fn foo(&self) -> impl Iterator<Item=i32>;
}

struct Impl1;
impl Trait for Impl1 {
    fn foo(&self) -> impl Iterator<Item=i32> {
        1..
    }
}

struct Impl2;
impl Trait for Impl2 {
    fn foo(&self) -> impl Iterator<Item=i32> {
        2..10
    }
}

#[enum_dispatch(Trait)]
enum Item {
    Impl1(Impl1),
    Impl2(Impl2)
}


#[test]
fn main() {
    assert_eq!(Item::from(Impl1{}).foo().next().unwrap(), 1);
    assert_eq!(Item::from(Impl2{}).foo().next().unwrap(), 2);
}
