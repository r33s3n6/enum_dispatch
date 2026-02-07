//! Provides a utility for generating `enum_dispatch` impl blocks given `EnumDispatchItem` and
//! `syn::ItemTrait` definitions.
use std::collections::HashMap;

// use crate::cache;
use proc_macro2::{Group, Span, TokenStream, TokenTree};
use quote::{quote, quote_spanned, ToTokens};
use syn::spanned::Spanned;
use syn::{
    parse_str,
    visit_mut::{self, VisitMut},
    Ident, PathArguments, Type, TypePath,
};

use crate::enum_dispatch_item::EnumDispatchItem;
use crate::enum_dispatch_variant::EnumDispatchVariant;
use crate::syn_utils::plain_identifier_expr;

/// Name bound to the single enum field in generated match statements. It doesn't really matter
/// what this is, as long as it's consistent across the left and right sides of generated match
/// arms. For simplicity's sake, the field is bound to this name everywhere it's generated.
const FIELDNAME: &str = "inner";

pub fn push_enum_conversion_impls(
    stream: &mut proc_macro2::TokenStream,
    enumname: &syn::Ident,
    enumvariants: &[&EnumDispatchVariant],
    generics: &syn::Generics,
) {
    push_from_impls(stream, enumname, enumvariants, generics);
    push_try_into_impls(stream, enumname, enumvariants, generics);
}

struct ParamSubst {
    // pub map: HashMap<Ident, GenericArgument>,
    pub type_map: HashMap<Ident, Type>,
}

impl ParamSubst {
    fn new(generics: &syn::Generics, trait_params: &[String]) -> Self {
        let mut type_map = HashMap::new();
        generics
            .params
            .iter()
            .zip(trait_params.iter())
            .for_each(|(generic_param, trait_param)| match generic_param {
                syn::GenericParam::Type(type_param) => {
                    let ident = type_param.ident.clone();
                    let t = parse_str::<Type>(trait_param).expect("parse type failed");
                    // let t = GenericArgument::Type(t);
                    type_map.insert(ident, t);
                }
                syn::GenericParam::Const(_const_param) => {
                    // let ident = const_param.ident.clone();
                    // let e = parse_str::<Expr>(trait_param).expect("parse const failed");
                    // let e = GenericArgument::Const(e);

                    // (ident, e)
                    panic!("Const generics in #[enum_dispatch(...)] are not supported")
                }
                syn::GenericParam::Lifetime(_lifetime_def) => {
                    panic!("Lifetime generics in #[enum_dispatch(...)] are not supported")
                }
            });
        Self { type_map }
    }

    fn empty() -> Self {
        Self {
            type_map: HashMap::new(),
        }
    }

    fn is_empty(&self) -> bool {
        self.type_map.is_empty()
    }

    fn map_type_param(&self, ident: &Ident) -> Option<&Type> {
        self.type_map.get(ident)
    }
}

// TODO: 现在没有处理遮蔽
impl VisitMut for ParamSubst {
    // fn visit_generic_argument_mut(&mut self, i: &mut syn::GenericArgument) {
    //     visit_mut::visit_generic_argument_mut(self, i);
    // }

    fn visit_type_mut(&mut self, i: &mut Type) {
        visit_mut::visit_type_mut(self, i);

        let Type::Path(TypePath { qself, path }) = i else {
            return;
        };

        if qself.is_none() {
            // leading_colon means the path starts with ::, so it can't be a single-segment path referring to a type parameter
            if path.leading_colon.is_some() {
                return;
            }
            let seg = &mut path.segments[0];
            // generics cannot be type constructor
            if !matches!(seg.arguments, PathArguments::None) {
                return;
            }

            if let Some(arg) = self.map_type_param(&seg.ident) {
                if path.segments.len() == 1 {
                    *i = arg.clone();
                } else {
                    if let Type::Path(arg_tp) = arg {
                        let mut new_path = arg_tp.path.clone();
                        new_path
                            .segments
                            .extend(path.segments.iter().skip(1).cloned());
                        *i = Type::Path(TypePath {
                            qself: None,
                            path: new_path,
                        });
                    } else {
                        // not supported yet
                        panic!(
                            "Only type parameters can be substituted in trait method signatures"
                        );
                    }
                }
            }
        }
        // no need to handle qself, as its type will be visited recursively
    }
}

pub fn get_enum_info(
    enum_def: &EnumDispatchItem,
) -> (&syn::Ident, Vec<&EnumDispatchVariant>, &syn::Generics) {
    let enumname = &enum_def.ident;
    let enumvariants: Vec<&EnumDispatchVariant> = enum_def.variants.iter().collect();
    let generics = &enum_def.generics;
    (enumname, enumvariants, generics)
}

pub fn push_trait_def_checker_for_enum(
    stream: &mut proc_macro2::TokenStream,
    traitname: &syn::Ident,
    trait_args: &[String],
    enumname: &syn::Ident,
    enumgenerics: &syn::Generics,
) {
    let (generic_impl_constraints, enum_type_generics, where_clause) =
        enumgenerics.split_for_impl();

    let trait_args = if trait_args.is_empty() {
        // empty
        quote! {}
    } else {
        let args: Vec<proc_macro2::TokenStream> = trait_args
            .iter()
            .map(|s| s.parse().expect("trait_type_generics tokenize failed"))
            .collect();
        quote! {   < #( #args ),* > }
    };

    //     const _: () = {
    //     fn assert_impl<T, U>()
    //     where
    //         Foo<T, U>: TraitX<T>,
    //     {}
    //     let _ = assert_impl::<T, U>;
    // };
    // fn assert_impl < #enum_params , #(#trait_args ,)* #enumname #enum_type_generics : #traitname #trait_args > ()
    // #where_clause
    // {}
    // let _ = assert_impl #trait_args_usage;

    // let enum_params = enumgenerics.params;

    let checker = quote! {
        const _: () = {
            trait Check #generic_impl_constraints : #traitname #trait_args #where_clause {}
            impl #generic_impl_constraints Check #enum_type_generics for #enumname #enum_type_generics #where_clause {}
        };
    };
    checker.to_tokens(stream);
}

/// Implements the specified trait for the given enum definition, assuming the trait definition is
/// already present in local storage.
pub fn push_enum_impls(
    stream: &mut proc_macro2::TokenStream,
    trait_def: &syn::ItemTrait,
    trait_args: Vec<String>,
    enumname: &syn::Ident,
    enumvariants: &[&EnumDispatchVariant],
    enumgenerics: &syn::Generics,
) {
    let traitname = &trait_def.ident;
    let traitfns = &trait_def.items;

    let (generic_impl_constraints, enum_type_generics, where_clause) =
        enumgenerics.split_for_impl();
    let (_, trait_type_generics, _) = trait_def.generics.split_for_impl();

    // let generic_span = generic_impl_constraints.span();
    // eprintln!("generic_span: {:?}", generic_span);

    let (trait_type_generics, mut subst) = if trait_args.is_empty() {
        let trait_type_generics = trait_type_generics.as_turbofish();
        (trait_type_generics.to_token_stream(), ParamSubst::empty())
    } else {
        assert_eq!(
            trait_args.len(),
            trait_def.generics.params.len(),
            "The number of generic parameters specified in #[enum_dispatch(...)] must match the number of generic parameters in the trait definition"
        );

        let mapping = ParamSubst::new(&trait_def.generics, &trait_args);

        let params: Vec<proc_macro2::TokenStream> = trait_args
            .into_iter()
            .map(|s| s.parse().expect("trait_type_generics tokenize failed"))
            .collect();

        (quote! {   ::< #( #params ),* > }, mapping)
    };

    // eprintln!("trait_type_generics: {:?}", trait_type_generics);
    // eprintln!("trait_type_generics span: {:?}", trait_type_generics.span());

    let trait_type_generics: Option<syn::AngleBracketedGenericArguments> =
        if trait_type_generics.is_empty() {
            None
        } else {
            let args: syn::AngleBracketedGenericArguments =
                syn::parse2(trait_type_generics).expect("trait_type_generics parse failed");
            // args.colon2_token = Some(Default::default());
            // eprintln!("args_span: {:?}", args.span());
            Some(args)
        };

    // TODO: #trait_type_generics -> real type generics specified by enum attributes

    // BREAKING CHANGE: trait_type_generics -> trait_params
    let trait_impl = quote! {
        impl #generic_impl_constraints #traitname #trait_type_generics for #enumname #enum_type_generics #where_clause {

        }
    };
    // let trait_impl = respan(trait_impl, Span::mixed_site());
    let mut trait_impl: syn::ItemImpl = syn::parse2(trait_impl).expect("trait_impl parse failed");

    trait_impl.unsafety = trait_def.unsafety;

    for trait_fn in traitfns {
        match trait_fn {
            syn::TraitItem::Fn(trait_fn) => {
                let mut trait_fn = trait_fn.to_owned();
                if !subst.is_empty() {
                    subst.visit_trait_item_fn_mut(&mut trait_fn);
                }
                trait_impl.items.push(create_trait_match(
                    trait_fn,
                    &trait_type_generics,
                    &traitname,
                    enumname,
                    enumvariants,
                ));
            }
            _ => {
                panic!("Only trait methods can be implemented in #[enum_dispatch(...)]")
            }
        }
    }

    trait_impl.to_tokens(stream);
}

/// Returns whether or not an attribute from an enum variant should be applied to other usages of
/// that variant's identifier.
fn use_attribute(attr: &&syn::Attribute) -> bool {
    attr.path().is_ident("cfg")
}

#[inline]
fn filtered_attrs<'a>(
    attrs: &'a [syn::Attribute],
) -> impl Iterator<Item = &'a syn::Attribute> + Clone {
    // slice::Iter 是 Clone，函数指针也是 Copy/Clone，所以整个 Filter 也 Clone
    attrs.iter().filter(use_attribute)
}

/// Generates impls of core::convert::From for each enum variant.
fn push_from_impls(
    stream: &mut proc_macro2::TokenStream,
    enumname: &syn::Ident,
    enumvariants: &[&EnumDispatchVariant],
    generics: &syn::Generics,
) {
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    for v in enumvariants {
        let variant_ident = &v.ident;
        let variant_ty = &v.ty;
        let attrs = filtered_attrs(v.attrs.as_ref());

        stream.extend(quote! {
            #(#attrs)*
            impl #impl_generics ::core::convert::From<#variant_ty>
                for #enumname #ty_generics #where_clause
            {
                #[inline]
                fn from(v: #variant_ty) -> #enumname #ty_generics {
                    #enumname::#variant_ident(v)
                }
            }
        });
    }
}

fn push_try_into_impls(
    stream: &mut proc_macro2::TokenStream,
    enumname: &syn::Ident,
    enumvariants: &[&EnumDispatchVariant],
    generics: &syn::Generics,
) {
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    // calculate ident_str for each variant, to avoid doing it multiple times in the loop
    let ident_strs: Vec<String> = enumvariants.iter().map(|v| v.ident.to_string()).collect();

    for (i, v) in enumvariants.iter().enumerate() {
        let variant_ident = &v.ident;
        let variant_ty = &v.ty;
        let attrs = filtered_attrs(v.attrs.as_ref());

        // 目标 variant 名字只算一次
        let to_str = &ident_strs[i];

        // 生成 other arms：单层循环拼 TokenStream，避免多次迭代/clone/collect
        let mut other_arms = proc_macro2::TokenStream::new();
        for (j, other) in enumvariants.iter().enumerate() {
            if i == j {
                continue;
            }

            let other_ident = &other.ident;
            let other_attrs = filtered_attrs(other.attrs.as_ref());
            let from_str = &ident_strs[j];

            other_arms.extend(quote! {
                #(#other_attrs)*
                #enumname::#other_ident(v) => {
                    Err(concat!("Tried to convert variant ", #from_str, " to ", #to_str))
                },
            });
        }

        stream.extend(quote! {
            #(#attrs)*
            impl #impl_generics ::core::convert::TryInto<#variant_ty>
                for #enumname #ty_generics #where_clause
            {
                type Error = &'static str;

                #[inline]
                fn try_into(
                    self
                ) -> ::core::result::Result<
                    #variant_ty,
                    <Self as ::core::convert::TryInto<#variant_ty>>::Error
                > {
                    match self {
                        #enumname::#variant_ident(v) => Ok(v),
                        #other_arms
                    }
                }
            }
        });
    }
}

/// Used to keep track of the 'self' arguments in a trait's function signature.
/// Static -> no 'self' arguments
/// ByReference -> &self, &mut self
/// ByValue -> self, mut self
enum MethodType {
    Static,
    ByReference,
    ByValue,
}

/// Parses the arguments of a trait method's signature, returning all non-self arguments as well as
/// a MethodType enum describing the self argument, if present.
fn extract_fn_args(
    trait_args: syn::punctuated::Punctuated<syn::FnArg, syn::token::Comma>,
) -> (
    MethodType,
    syn::punctuated::Punctuated<syn::Expr, syn::token::Comma>,
) {
    let mut method_type = MethodType::Static;
    let new_args: Vec<syn::Ident> = trait_args
        .iter()
        .filter_map(|arg| match arg {
            syn::FnArg::Receiver(syn::Receiver {
                reference: Some(_), ..
            }) => {
                method_type = MethodType::ByReference;
                None
            }
            syn::FnArg::Receiver(syn::Receiver {
                reference: None, ..
            }) => {
                method_type = MethodType::ByValue;
                None
            }
            syn::FnArg::Typed(syn::PatType { pat, .. }) => {
                if let syn::Pat::Ident(syn::PatIdent { ident, .. }) = &**pat {
                    Some(ident.to_owned())
                } else {
                    // All non-ident fn args are replaced in `identify_signature_arguments`.
                    unreachable!()
                }
            }
        })
        .collect();
    let args = {
        let mut args = syn::punctuated::Punctuated::new();
        new_args.iter().for_each(|arg| {
            args.push(syn::parse_str(arg.to_string().as_str()).unwrap());
        });
        args
    };
    (method_type, args)
}

/// Creates a method call that can be used in the match arms of all non-static method
/// implementations.
fn create_trait_fn_call(
    trait_method: &syn::TraitItemFn,
    trait_generics: &Option<syn::AngleBracketedGenericArguments>,
    trait_name: &syn::Ident,
) -> syn::Expr {
    let trait_args = trait_method.to_owned().sig.inputs;
    let (method_type, mut args) = extract_fn_args(trait_args);

    // Insert FIELDNAME at the beginning of the argument list for UCFS-style method calling
    let explicit_self_arg = syn::Ident::new(FIELDNAME, Span::mixed_site());
    args.insert(0, plain_identifier_expr(explicit_self_arg));

    let mut call = syn::Expr::from(syn::ExprCall {
        attrs: vec![],
        func: {
            if let MethodType::Static = method_type {
                // Trait calls can be created when the inner type is known, like this:
                //
                // syn::parse_quote! { #type::#trait_method_name }
                //
                // However, without a concrete enum to match on, it's impossible to tell
                // which variant to call.
                unimplemented!(
                    "Static methods cannot be enum_dispatched (no self argument to match on)"
                );
            } else {
                let method_name = &trait_method.sig.ident;
                if let Some(trait_generics) = trait_generics {
                    assert!(trait_generics.colon2_token.is_some());
                }

                // let trait_turbofish = trait_generics.as_turbofish();

                // It's not allowed to specify late bound lifetime arguments for a function call.
                // Theoretically, it should be possible to determine from a function signature
                // whether or not it has late bound lifetime arguments. In practice, it's very
                // difficult, requiring recursive visitors over all the types in the signature and
                // inference for elided lifetimes.
                //
                // Instead, it appears to be safe to strip out any lifetime arguments altogether.
                let mut generics_without_lifetimes = trait_method.sig.generics.clone();
                generics_without_lifetimes.params = generics_without_lifetimes
                    .params
                    .into_iter()
                    .filter(|param| !matches!(param, syn::GenericParam::Lifetime(..)))
                    .collect();
                let method_type_generics = generics_without_lifetimes.split_for_impl().1;
                let method_turbofish = method_type_generics.as_turbofish();

                Box::new(
                    syn::parse_quote! { #trait_name #trait_generics::#method_name #method_turbofish },
                )
            }
        },
        paren_token: Default::default(),
        args,
    });

    if trait_method.sig.asyncness.is_some() {
        call = syn::Expr::from(syn::ExprAwait {
            attrs: Default::default(),
            base: Box::new(call),
            dot_token: Default::default(),
            await_token: Default::default(),
        });
    }

    call
}

/// Constructs a match expression that matches on all variants of the specified enum, creating a
/// binding to their single field and calling the provided trait method on each.
fn create_match_expr(
    trait_method: &syn::TraitItemFn,
    trait_generics: &Option<syn::AngleBracketedGenericArguments>,
    trait_name: &syn::Ident,
    enum_name: &syn::Ident,
    enumvariants: &[&EnumDispatchVariant],
) -> syn::Expr {
    let trait_fn_call = create_trait_fn_call(trait_method, trait_generics, trait_name);

    let is_self_return = if let syn::ReturnType::Type(_, returntype) = &trait_method.sig.output {
        match returntype.as_ref() {
            syn::Type::Path(p) => {
                if let Some(i) = p.path.get_ident() {
                    i.to_string() == "Self"
                } else {
                    false
                }
            }
            _ => false,
        }
    } else {
        false
    };

    // Creates a Vec containing a match arm for every enum variant
    let match_arms = enumvariants
        .iter()
        .map(|variant| {
            let mut call = trait_fn_call.to_owned();

            if is_self_return {
                let variant_type = &variant.ty;
                let from_call: syn::ExprCall = syn::parse_quote! {
                    <Self as ::core::convert::From::<#variant_type>>::from(#call)
                };
                call = syn::Expr::from(from_call);
            }

            let variant_name = &variant.ident;
            let attrs = filtered_attrs(variant.attrs.as_ref())
                .cloned()
                .collect::<Vec<_>>();
            syn::Arm {
                attrs,
                pat: {
                    let fieldname = syn::Ident::new(FIELDNAME, Span::mixed_site());
                    syn::parse_quote! {#enum_name::#variant_name(#fieldname)}
                },
                guard: None,
                fat_arrow_token: Default::default(),
                body: Box::new(call),
                comma: Some(Default::default()),
            }
        })
        .collect();

    // Creates the match expression
    syn::Expr::from(syn::ExprMatch {
        attrs: vec![],
        match_token: Default::default(),
        expr: Box::new(plain_identifier_expr(syn::Ident::new(
            "self",
            proc_macro2::Span::call_site(),
        ))),
        brace_token: Default::default(),
        arms: match_arms,
    })
}

/// Builds an implementation of the given trait function for the given enum type.
fn create_trait_match(
    trait_method: syn::TraitItemFn,
    trait_generics: &Option<syn::AngleBracketedGenericArguments>,
    trait_name: &syn::Ident,
    enum_name: &syn::Ident,
    enumvariants: &[&EnumDispatchVariant],
) -> syn::ImplItem {
    let mut trait_method = trait_method.to_owned();
    identify_signature_arguments(&mut trait_method.sig);

    let match_expr = create_match_expr(
        &trait_method,
        trait_generics,
        trait_name,
        enum_name,
        enumvariants,
    );

    let mut impl_attrs = trait_method.attrs.clone();
    // Inline impls - #[inline] is never already specified in a trait method signature
    impl_attrs.push(syn::Attribute {
        pound_token: Default::default(),
        style: syn::AttrStyle::Outer,
        bracket_token: Default::default(),
        meta: syn::Meta::Path(syn::parse_str("inline").unwrap()),
    });

    syn::ImplItem::Fn(syn::ImplItemFn {
        attrs: impl_attrs,
        vis: syn::Visibility::Inherited,
        defaultness: None,
        sig: trait_method.sig,
        block: syn::Block {
            brace_token: Default::default(),
            stmts: vec![syn::Stmt::Expr(match_expr, None)],
        },
    })
}

/// All method arguments that appear in trait method signatures must be passed through to the
/// underlying dispatched method calls, so they must have unique identifiers. That means we need to
/// give names to wildcard arguments (`_`), tuple-style arguments, and a bunch of other argument
/// types you never knew were valid Rust syntax.
///
/// Since there is no way to generate hygienic identifiers, we just use a special underscored
/// string followed by an incrementing counter. We do this for *every* argument, including ones
/// that are already named, in case somebody clever decides to name their arguments similarly.
fn identify_signature_arguments(sig: &mut syn::Signature) {
    let mut arg_counter = 0;

    /// Generates a new argument identifier named `__enum_dispatch_arg_` followed by an
    /// incrementing counter.
    fn new_arg_ident(span: proc_macro2::Span, arg_counter: &mut usize) -> syn::Ident {
        let ident = proc_macro2::Ident::new(&format!("__enum_dispatch_arg_{}", arg_counter), span);
        *arg_counter += 1;
        ident
    }

    sig.inputs.iter_mut().for_each(|arg| match arg {
        syn::FnArg::Typed(ref mut pat_type) => {
            let span = pat_type.span();
            *pat_type.pat = match &*pat_type.pat {
                syn::Pat::Ident(ref pat_ident) => syn::Pat::Ident(syn::PatIdent {
                    ident: new_arg_ident(pat_ident.span(), &mut arg_counter),
                    ..pat_ident.clone()
                }),
                // Some of these aren't valid Rust syntax, but why not support all of them anyways!
                syn::Pat::Lit(syn::PatLit { attrs, .. })
                | syn::Pat::Macro(syn::PatMacro { attrs, .. })
                | syn::Pat::Or(syn::PatOr { attrs, .. })
                | syn::Pat::Path(syn::PatPath { attrs, .. })
                | syn::Pat::Range(syn::PatRange { attrs, .. })
                | syn::Pat::Reference(syn::PatReference { attrs, .. })
                | syn::Pat::Rest(syn::PatRest { attrs, .. })
                | syn::Pat::Slice(syn::PatSlice { attrs, .. })
                | syn::Pat::Struct(syn::PatStruct { attrs, .. })
                | syn::Pat::Tuple(syn::PatTuple { attrs, .. })
                | syn::Pat::TupleStruct(syn::PatTupleStruct { attrs, .. })
                | syn::Pat::Type(syn::PatType { attrs, .. })
                | syn::Pat::Const(syn::PatConst { attrs, .. })
                | syn::Pat::Paren(syn::PatParen { attrs, .. })
                | syn::Pat::Wild(syn::PatWild { attrs, .. }) => syn::Pat::Ident(syn::PatIdent {
                    attrs: attrs.to_owned(),
                    by_ref: None,
                    mutability: None,
                    ident: new_arg_ident(span, &mut arg_counter),
                    subpat: None,
                }),
                // This can occur for `box foo` syntax, which is no longer supported by syn 2.0.
                syn::Pat::Verbatim(_) => syn::Pat::Ident(syn::PatIdent {
                    attrs: Default::default(),
                    by_ref: None,
                    mutability: None,
                    ident: new_arg_ident(span, &mut arg_counter),
                    subpat: None,
                }),
                _ => panic!("Unsupported argument type"),
            }
        }
        // `self` arguments will never need to be renamed.
        syn::FnArg::Receiver(..) => (),
    });
}
