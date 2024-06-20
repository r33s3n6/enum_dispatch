use enum_dispatch::enum_dispatch;

#[enum_dispatch]
trait TestTrait {
    fn describe(&self) -> String;
}

struct E;

#[enum_dispatch(TestTrait)]
enum TestEnum {
    A {
        x: i8,
    },
    #[cfg(target_os = "linux")]
    B {
        y: i16,
    },
    C {
        #[allow(dead_code)]
        y: i32,
    },
    D(i64),
    E,
}

impl TestTrait for i8 {
    fn describe(&self) -> String {
        format!("{}", self)
    }
}

impl TestTrait for i16 {
    fn describe(&self) -> String {
        format!("{}", self)
    }
}

impl TestTrait for i32 {
    fn describe(&self) -> String {
        format!("{}", self)
    }
}

impl TestTrait for i64 {
    fn describe(&self) -> String {
        format!("{}", self)
    }
}

impl TestTrait for E {
    fn describe(&self) -> String {
        "E".to_string()
    }
}

fn main() {
    let a = TestEnum::A { x: 1 };
    a.describe();
}
