use enum_dispatch::enum_dispatch;

#[enum_dispatch]
trait TestTrait {
    fn foo(&self) -> Option<Self>
    where
        Self: Sized;
}

#[enum_dispatch(TestTrait)]
enum TestEnum {
    A,
    B,
}

struct A;

impl TestTrait for A {
    fn foo(&self) -> Option<A> {
        Some(A)
    }
}

struct B;

impl TestTrait for B {
    fn foo(&self) -> Option<B> {
        None
    }
}

#[test]
fn main() {
    let a = A;
    let e: TestEnum = a.into();
    let _: Option<TestEnum> = e.foo();
}
