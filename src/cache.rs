//! Procedural macros don't offer a good way to store information between macro invocations.  In
//! addition, all syntax-related structures implement `!Send` and `!Sync`, making it impossible to
//! keep them in any sort of static storage. This module uses some workarounds to add that
//! functionality.
//!
//! Fortunately, `TokenStream`s can be converted to and from `String`s, which can be stored
//! statically. Unfortunately, doing so strips any related `Span` information, preventing error
//! messages from being as informative as they could be. For now, it seems this is the best option
//! available.
use proc_macro2::Ident;
use quote::ToTokens;

use once_cell::sync::Lazy;

use std::collections::{HashMap, HashSet};
use std::sync::Mutex;

use crate::enum_dispatch_item;

/// Uniquely identifies a trait or an enum. This is based on its name and number of arguments.
#[derive(PartialEq, Eq, Hash, Clone)]
pub struct UniqueItemId {
    item_name: String,
    num_generics: usize,
}

impl UniqueItemId {
    /// Convenience constructor for UniqueItemId.
    pub fn new(item_name: String, num_generics: usize) -> Self {
        Self {
            item_name,
            num_generics,
        }
    }
}

// Magical storage for trait definitions so that they can be used when parsing other syntax
// structures.
static TRAIT_DEFS: Lazy<Mutex<HashMap<UniqueItemId, String>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));
static ENUM_DEFS: Lazy<Mutex<HashMap<UniqueItemId, String>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

// to support multiple enum_dispatch on enum
static ENUM_CONVERSION_IMPLS_DEFS: Lazy<Mutex<HashSet<UniqueItemId>>> =
    Lazy::new(|| Mutex::new(HashSet::new()));

pub struct LinkRef {
    pub target: UniqueItemId,
    pub generic_args: Vec<String>,
}

struct PendingMap {
    enums_waiting_on_trait: HashMap<UniqueItemId, Vec<LinkRef>>,
    traits_waiting_on_enum: HashMap<UniqueItemId, Vec<LinkRef>>,
}

impl PendingMap {
    pub fn new() -> Self {
        Self {
            enums_waiting_on_trait: HashMap::new(),
            traits_waiting_on_enum: HashMap::new(),
        }
    }

    pub fn split_mut(
        &mut self,
    ) -> (
        &mut HashMap<UniqueItemId, Vec<LinkRef>>,
        &mut HashMap<UniqueItemId, Vec<LinkRef>>,
    ) {
        (
            &mut self.enums_waiting_on_trait,
            &mut self.traits_waiting_on_enum,
        )
    }
}

static PENDING_MAP: Lazy<Mutex<PendingMap>> = Lazy::new(|| Mutex::new(PendingMap::new()));

pub fn get_unique_id(defname: &Ident, generics: &syn::Generics) -> UniqueItemId {
    let num_generic_args = crate::supported_generics::num_supported_generics(generics);
    UniqueItemId::new(defname.to_string(), num_generic_args)
}

/// Store a trait definition for future reference.
pub fn cache_trait(item: syn::ItemTrait) -> UniqueItemId {
    let uid = get_unique_id(&item.ident, &item.generics);
    TRAIT_DEFS
        .lock()
        .unwrap()
        .insert(uid.clone(), item.into_token_stream().to_string());
    uid
}

/// Store an enum definition for future reference.
pub fn cache_enum_dispatch(item: enum_dispatch_item::EnumDispatchItem) -> UniqueItemId {
    let uid = get_unique_id(&item.ident, &item.generics);
    ENUM_DEFS
        .lock()
        .unwrap()
        .insert(uid.clone(), item.into_token_stream().to_string());
    uid
}

pub fn collect_ready_and_defer_missing(
    owner_id: &UniqueItemId,
    incoming_edges: Vec<LinkRef>,
    target_defs: &HashMap<UniqueItemId, String>,
    waiting_on_me: &mut HashMap<UniqueItemId, Vec<LinkRef>>,
    waiters_of_target: &mut HashMap<UniqueItemId, Vec<LinkRef>>,
) -> Vec<LinkRef> {
    // 已经挂在 key_id 下的 pending
    let mut ready = waiting_on_me.remove(owner_id).unwrap_or_else(Vec::new);

    for edge in incoming_edges {
        if target_defs.contains_key(&edge.target) {
            // 目标已存在：直接加入 ready
            ready.push(edge);
        } else {
            // 目标不存在：反向记录“谁在等谁”
            waiters_of_target
                .entry(edge.target)
                .or_default()
                .push(LinkRef {
                    target: owner_id.clone(),
                    generic_args: edge.generic_args,
                });
        }
    }

    ready
}

pub fn ingest_enum_trait_edges(enum_id: &UniqueItemId, trait_edges: Vec<LinkRef>) -> Vec<LinkRef> {
    let mut pending_map = PENDING_MAP.lock().unwrap();
    let (enums_by_trait, traits_by_enum) = pending_map.split_mut();
    let trait_defs = TRAIT_DEFS.lock().unwrap();
    collect_ready_and_defer_missing(
        enum_id,
        trait_edges,
        &*trait_defs,
        traits_by_enum,
        enums_by_trait,
    )
}

pub fn ingest_trait_enum_edges(trait_id: &UniqueItemId, enum_edges: Vec<LinkRef>) -> Vec<LinkRef> {
    let mut pending_map = PENDING_MAP.lock().unwrap();
    let (enums_by_trait, traits_by_enum) = pending_map.split_mut();
    let enum_defs = ENUM_DEFS.lock().unwrap();
    collect_ready_and_defer_missing(
        trait_id,
        enum_edges,
        &*enum_defs,
        enums_by_trait,
        traits_by_enum,
    )
}

pub fn get_trait_def(uid: &UniqueItemId) -> syn::ItemTrait {
    TRAIT_DEFS
        .lock()
        .unwrap()
        .get(uid)
        .map(|entry| syn::parse_str(entry).unwrap())
        .unwrap()
}

pub fn get_enum_def(uid: &UniqueItemId) -> enum_dispatch_item::EnumDispatchItem {
    ENUM_DEFS
        .lock()
        .unwrap()
        .get(uid)
        .map(|entry| syn::parse_str(entry).unwrap())
        .unwrap()
}

pub fn set_if_conversion_not_defined(enum_id: UniqueItemId) -> bool {
    let mut conversion_impls = ENUM_CONVERSION_IMPLS_DEFS.lock().unwrap();
    if conversion_impls.contains(&enum_id) {
        false
    } else {
        conversion_impls.insert(enum_id);
        true
    }
}

// /// Returns a list of all of the trait definitions that were previously linked to the supplied enum
// /// name.
// pub fn fulfilled_by_enum(
//     defname: &::proc_macro2::Ident,
//     num_generic_args: usize,
// ) -> Vec<syn::ItemTrait> {
//     let idents = match DEFERRED_LINKS
//         .lock()
//         .unwrap()
//         .remove_entry(&UniqueItemId::new(defname.to_string(), num_generic_args))
//     {
//         Some((_, links)) => links,
//         None => vec![],
//     };
//     idents
//         .iter()
//         .filter_map(|ident_string| {
//             TRAIT_DEFS
//                 .lock()
//                 .unwrap()
//                 .get(ident_string)
//                 .map(|entry| syn::parse(entry.parse().unwrap()).unwrap())
//         })
//         .collect()
// }

// /// Returns a list of all of the enum definitions that were previously linked to the supplied trait
// /// name.
// pub fn fulfilled_by_trait(
//     defname: &::proc_macro2::Ident,
//     num_generic_args: usize,
// ) -> Vec<enum_dispatch_item::EnumDispatchItem> {
//     let idents = match DEFERRED_LINKS
//         .lock()
//         .unwrap()
//         .remove_entry(&UniqueItemId::new(defname.to_string(), num_generic_args))
//     {
//         Some((_, links)) => links,
//         None => vec![],
//     };
//     idents
//         .iter()
//         .filter_map(|ident_string| {
//             ENUM_DEFS
//                 .lock()
//                 .unwrap()
//                 .get(ident_string)
//                 .map(|entry| syn::parse(entry.parse().unwrap()).unwrap())
//         })
//         .collect()
// }
