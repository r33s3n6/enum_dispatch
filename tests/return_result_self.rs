use enum_dispatch::enum_dispatch;

#[enum_dispatch]
trait TestTrait {
    fn foo(&self) -> Result<Self, String>
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
    fn foo(&self) -> Result<A, String> {
        Ok(A)
    }
}

struct B;

impl TestTrait for B {
    fn foo(&self) -> Result<B, String> {
        Err("B error".to_string())
    }
}

#[test]
fn main() {
    let a = A;
    let e: TestEnum = a.into();
    let _: Result<TestEnum, String> = e.foo();
}
