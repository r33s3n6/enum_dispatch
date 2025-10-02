use enum_dispatch::enum_dispatch;
use std::future::Future;

struct A;
struct B;

impl Calculator for A {
    async fn calculate(&self, x: i32) -> i32 {
        x + 10
    }
}

impl Calculator for B {
    async fn calculate(&self, x: i32) -> i32 {
        x * 2
    }
}

#[enum_dispatch]
enum Calc {
    A,
    B,
}

#[enum_dispatch(Calc)]
trait Calculator {
    fn calculate(&self, x: i32) -> impl Future<Output = i32> + Send;
}

#[test]
fn test_impl_future_with_return_value() {
    let a: Calc = A.into();
    let b: Calc = B.into();
    
    smol::block_on(async {
        assert_eq!(a.calculate(5).await, 15);
        assert_eq!(b.calculate(5).await, 10);
    });
}

