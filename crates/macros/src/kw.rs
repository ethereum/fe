use std::collections::HashSet;

use proc_macro2::TokenStream;
use quote::quote;

use super::{Error, Result};

pub(super) fn define_keywords(attrs: proc_macro::TokenStream) -> Result<proc_macro::TokenStream> {
    let args = syn::parse::<Args>(attrs)?;

    let definer = KeywordDefiner { args };
    Ok(definer.build().into())
}

struct KeywordDefiner {
    args: Args,
}

impl KeywordDefiner {
    fn build(self) -> TokenStream {
        let keywords = self.define_keywords();
        let prefill_method = self.define_prefill_method();
        quote! {
            #keywords
            #prefill_method
        }
    }

    fn define_keywords(&self) -> TokenStream {
        let mut stream = vec![];
        for (i, (kw, _)) in self.args.0.iter().enumerate() {
            let ident_id = Self::ident_id(i);
            stream.push(quote!(
                pub const #kw: crate::hir_def::ident::IdentId = #ident_id;
            ))
        }

        quote! {
            #(#stream)*
        }
    }

    fn define_prefill_method(&self) -> TokenStream {
        let mut prefills = vec![];
        for (kw, kw_str) in self.args.0.iter() {
            let kw_str = kw_str.value();
            prefills.push(quote!(
                let generated_kw = crate::hir_def::ident::IdentId::new(db, #kw_str.to_string());
                assert_eq!(generated_kw, #kw);
            ));
        }

        quote! {
            impl crate::hir_def::ident::IdentId {
                pub fn prefill(db: &dyn crate::HirDb) {
                    #(#prefills)*
                }
            }
        }
    }

    fn ident_id(index: usize) -> TokenStream {
        quote! {
            crate::hir_def::ident::IdentId(::salsa::Id::from_u32((#index) as u32))
        }
    }
}

struct Args(Vec<(syn::Ident, syn::LitStr)>);
impl syn::parse::Parse for Args {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let mut seen_kws = HashSet::new();
        let mut seen_kw_str = HashSet::new();
        let mut kws = vec![];

        while !input.is_empty() {
            let keyword;
            syn::parenthesized!(keyword in input);
            let kw = keyword.parse::<syn::Ident>()?;
            keyword.parse::<syn::Token![,]>()?;
            let kw_str = keyword.parse::<syn::LitStr>()?;

            if !seen_kws.insert(kw.to_string()) {
                return Err(Error::new_spanned(
                    kw.clone(),
                    format!("duplicated keyword `{kw}"),
                ));
            }
            if !seen_kw_str.insert(kw_str.value()) {
                return Err(Error::new_spanned(
                    kw_str.clone(),
                    format!("duplicated keyword string `{}`", kw_str.value()),
                ));
            }
            kws.push((kw, kw_str));

            if input.parse::<syn::Token![,]>().is_err() {
                break;
            }
        }

        if !input.is_empty() {
            return Err(Error::new_spanned(
                input.parse::<proc_macro2::TokenStream>()?,
                "unexpected token",
            ));
        }

        Ok(Args(kws))
    }
}
