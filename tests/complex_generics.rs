use std::hash::Hash;
use std::marker::PhantomData;

use enum_dispatch::enum_dispatch;

/// In this module, the attribute with the argument appears over the enum.
mod attribute_argument_on_enum {
    use super::*;

    #[enum_dispatch]
    trait StoresKV<K: Hash, V> {
        fn store(&mut self, key: K, value: V);
        fn get(&self, key: &K) -> Option<&V>;
        fn update_value<F: Fn(V) -> V>(&mut self, key: &K, update_by: F);
    }

    #[enum_dispatch(StoresKV<K, V>)]
    enum KVStore<K: Hash, V, C: Credentials> {
        FileStorage(FileStorage<K, V>),
        RemoteStorage(RemoteStorage<K, V, C>),
        DevNull,
    }

    impl<K: Hash, V> StoresKV<K, V> for FileStorage<K, V> {
        #[allow(unused_variables)]
        fn store(&mut self, key: K, value: V) {
            unimplemented!();
        }
        #[allow(unused_variables)]
        fn get(&self, key: &K) -> Option<&V> {
            unimplemented!();
        }
        #[allow(unused_variables)]
        fn update_value<F: Fn(V) -> V>(&mut self, key: &K, update_by: F) {
            unimplemented!();
        }
    }

    impl<K: Hash, V, C: Credentials> StoresKV<K, V> for RemoteStorage<K, V, C> {
        #[allow(unused_variables)]
        fn store(&mut self, key: K, value: V) {
            let identity = self.credentials.identity();
            let api_key = self.credentials.api_key();
            unimplemented!();
        }
        #[allow(unused_variables)]
        fn get(&self, key: &K) -> Option<&V> {
            let identity = self.credentials.identity();
            let api_key = self.credentials.api_key();
            unimplemented!();
        }
        #[allow(unused_variables)]
        fn update_value<F: Fn(V) -> V>(&mut self, key: &K, update_by: F) {
            let identity = self.credentials.identity();
            let api_key = self.credentials.api_key();
            unimplemented!();
        }
    }

    impl<K: Hash, V> StoresKV<K, V> for DevNull {
        fn store(&mut self, _key: K, _value: V) {}
        fn get(&self, _key: &K) -> Option<&V> {
            None
        }
        fn update_value<F: Fn(V) -> V>(&mut self, _key: &K, _update_by: F) {}
    }

    #[test]
    fn attribute_argument_on_enum() {
        let mut storages: Vec<KVStore<String, String, SimpleCreds>> = vec![
            DevNull.into(),
            FileStorage::default().into(),
            RemoteStorage::new(SimpleCreds).into(),
        ];

        storages[0].store(String::from("key1"), String::from("value"));
        assert_eq!(storages[0].get(&String::from("key2")), None);
        storages[0].update_value(&String::from("key1"), |value| format!("{} + 1", value));
    }
}

/// Same as above, but the attributes are reversed such that the one with an argument is on the
/// trait rather than the enum.
mod attribute_argument_on_trait {
    use super::*;

    #[enum_dispatch(KVStoreReversed<K, V, _C>)]
    trait StoresKVReversed<K: Hash, V> {
        fn store(&mut self, key: K, value: V);
        fn get(&self, key: &K) -> Option<&V>;
        fn update_value<F: Fn(V) -> V>(&mut self, key: &K, update_by: F);
    }

    #[enum_dispatch]
    enum KVStoreReversed<K: Hash, V, C: Credentials> {
        FileStorage(FileStorage<K, V>),
        RemoteStorage(RemoteStorage<K, V, C>),
        DevNull,
    }

    impl<K: Hash, V> StoresKVReversed<K, V> for FileStorage<K, V> {
        #[allow(unused_variables)]
        fn store(&mut self, key: K, value: V) {
            unimplemented!();
        }
        #[allow(unused_variables)]
        fn get(&self, key: &K) -> Option<&V> {
            unimplemented!();
        }
        #[allow(unused_variables)]
        fn update_value<F: Fn(V) -> V>(&mut self, key: &K, update_by: F) {
            unimplemented!();
        }
    }

    impl<K: Hash, V, C: Credentials> StoresKVReversed<K, V> for RemoteStorage<K, V, C> {
        #[allow(unused_variables)]
        fn store(&mut self, key: K, value: V) {
            let identity = self.credentials.identity();
            let api_key = self.credentials.api_key();
            unimplemented!();
        }
        #[allow(unused_variables)]
        fn get(&self, key: &K) -> Option<&V> {
            let identity = self.credentials.identity();
            let api_key = self.credentials.api_key();
            unimplemented!();
        }
        #[allow(unused_variables)]
        fn update_value<F: Fn(V) -> V>(&mut self, key: &K, update_by: F) {
            let identity = self.credentials.identity();
            let api_key = self.credentials.api_key();
            unimplemented!();
        }
    }

    impl<K: Hash, V> StoresKVReversed<K, V> for DevNull {
        fn store(&mut self, _key: K, _value: V) {}
        fn get(&self, _key: &K) -> Option<&V> {
            None
        }
        fn update_value<F: Fn(V) -> V>(&mut self, _key: &K, _update_by: F) {}
    }

    #[test]
    fn attribute_argument_on_enum() {
        let mut storages: Vec<KVStoreReversed<String, String, SimpleCreds>> = vec![
            DevNull.into(),
            FileStorage::default().into(),
            RemoteStorage::new(SimpleCreds).into(),
        ];

        storages[0].store(String::from("key1"), String::from("value"));
        assert_eq!(storages[0].get(&String::from("key2")), None);
        storages[0].update_value(&String::from("key1"), |value| format!("{} + 1", value));
    }
}

#[derive(Default)]
struct FileStorage<K: Hash, V> {
    key_value_type: PhantomData<(K, V)>,
}

trait Credentials {
    fn identity(&self) -> String;
    fn api_key(&self) -> String;
}

struct RemoteStorage<K: Hash, V, C: Credentials> {
    key_value_type: PhantomData<(K, V)>,
    credentials: C,
}

impl<K: Hash, V, C: Credentials> RemoteStorage<K, V, C> {
    fn new(credentials: C) -> Self {
        Self {
            key_value_type: Default::default(),
            credentials,
        }
    }
}

struct DevNull;

struct SimpleCreds;

impl Credentials for SimpleCreds {
    fn identity(&self) -> String {
        String::from("root")
    }
    fn api_key(&self) -> String {
        String::from("D0nTb3f0ol3D-Thi51Sn0tAR34al4P1kEY")
    }
}

mod different_generic_names {
    use enum_dispatch::enum_dispatch;
    use std::marker::PhantomData;

    trait Pool: 'static {}

    struct LogicalExpr<P: Pool> {
        exprs: Vec<Expr<P>>,
    }

    struct LiteralExpr<P: Pool> {
        value: String,
        phantom: PhantomData<fn(&mut P)>,
    }

    trait HasNodeRef {
        type NodeRef<'a>
        where
            Self: 'a;
    }

    trait AsNodeRef<H: HasNodeRef> {
        fn as_node_ref(&self) -> Option<H::NodeRef<'_>> {
            None
        }
    }

    trait Visitor<H: HasNodeRef> {
        fn visit<'a>(&self, node_ref: H::NodeRef<'a>);
    }

    struct Driver<'a, H: HasNodeRef, V: Visitor<H>> {
        visitor: &'a V,
        phantom: PhantomData<fn(&mut H)>,
    }

    impl<'a, H: HasNodeRef, V: Visitor<H>> Driver<'a, H, V> {
        pub fn walk_node<N: WalkChildren<H> + AsNodeRef<H>>(&mut self, n: &N) -> bool {
            true
        }
    }

    #[enum_dispatch]
    trait WalkChildren<H: HasNodeRef> {
        fn walk_children<V: Visitor<H>>(&self, d: &mut Driver<H, V>) -> bool;
    }

    enum NodeRef<'a, P: Pool> {
        Expr(&'a Expr<P>),
    }

    struct HasNodeRefTag<P: Pool> {
        phantom: PhantomData<fn(&mut P)>,
    }
    impl<P: Pool> HasNodeRef for HasNodeRefTag<P> {
        type NodeRef<'a> = NodeRef<'a, P>;
    }

    impl<P: Pool> WalkChildren<HasNodeRefTag<P>> for LogicalExpr<P> {
        fn walk_children<V: Visitor<HasNodeRefTag<P>>>(
            &self,
            d: &mut Driver<HasNodeRefTag<P>, V>,
        ) -> bool {
            for expr in &self.exprs {
                if !d.walk_node(expr) {
                    return false;
                }
            }
            true
        }
    }

    impl<P: Pool> WalkChildren<HasNodeRefTag<P>> for LiteralExpr<P> {
        fn walk_children<V: Visitor<HasNodeRefTag<P>>>(
            &self,
            _d: &mut Driver<HasNodeRefTag<P>, V>,
        ) -> bool {
            true
        }
    }

    #[enum_dispatch(WalkChildren<HasNodeRefTag<P>>)]
    enum Expr<P: Pool> {
        Logical(LogicalExpr<P>),
        Literal(LiteralExpr<P>),
    }

    impl<P: Pool> AsNodeRef<HasNodeRefTag<P>> for Expr<P> {
        fn as_node_ref(&self) -> Option<NodeRef<'_, P>> {
            Some(NodeRef::Expr(self))
        }
    }

    struct DefaultPool;
    impl Pool for DefaultPool {}

    struct DefaultVisitor;
    impl Visitor<HasNodeRefTag<DefaultPool>> for DefaultVisitor {
        fn visit<'a>(&self, _node_ref: NodeRef<'a, DefaultPool>) {}
    }

    #[test]
    fn test_different_generic_names() {
        let logical_expr: LogicalExpr<DefaultPool> = LogicalExpr {
            exprs: vec![
                LiteralExpr {
                    value: String::from("literal"),
                    phantom: Default::default(),
                }
                .into(),
                LogicalExpr {
                    exprs: vec![LiteralExpr {
                        value: String::from("literal"),
                        phantom: Default::default(),
                    }
                    .into()],
                }
                .into(),
            ],
        };
        let literal_expr: LiteralExpr<DefaultPool> = LiteralExpr {
            value: String::from("literal"),
            phantom: Default::default(),
        };

        let mut driver = Driver {
            visitor: &DefaultVisitor,
            phantom: Default::default(),
        };

        assert!(logical_expr.walk_children(&mut driver));
        assert!(literal_expr.walk_children(&mut driver));
    }
}

mod missing_def {

    /// This test ensures that the compiler error is informative when the trait or enum definition is missing.
    /// ```compile_fail
    /// mod test1 {
    ///     struct A {}
    ///     struct B {}
    ///     #[enum_dispatch::enum_dispatch(Foo)]
    ///     pub enum Any {
    ///         A,
    ///         B,
    ///     }
    /// }
    /// ```

    mod test2 {
        use std::marker::PhantomData;

        struct A<T> {
            _phantom: PhantomData<T>,
        }
        struct B<T> {
            _phantom: PhantomData<T>,
        }

        impl<T> Foo2<T> for A<T> {}
        impl<T> Foo2<T> for B<T> {}

        #[enum_dispatch::enum_dispatch(Foo2<T>)]
        pub enum Any<T> {
            A(A<T>),
            B(B<T>),
        }

        #[enum_dispatch::enum_dispatch]
        trait Foo2<T> {}
    }
}
