extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, FnArg, ImplItem, ItemImpl,
};

/// Macro for generating tokio channels from [`lsp-types`](https://docs.rs/lsp-types).
///
/// This procedural macro annotates the `tower_lsp::LanguageServer` trait implementation and generates
/// a struct full of tokio broadcast channels that can be used to signal the server to handle
/// defined requests and notifications.
#[proc_macro_attribute]
pub fn dispatcher(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let lang_server_trait_impl = parse_macro_input!(item as ItemImpl);

    let method_calls = parse_method_calls(&lang_server_trait_impl);
    let channel_struct = gen_channel_struct(&method_calls);

    let tokens = quote! {
        #channel_struct
        #lang_server_trait_impl
    };

    tokens.into()
    // item
}

struct LspTypeChannel<'a> {
    tx_name: syn::Ident,
    rx_name: syn::Ident,
    params: Option<&'a syn::Type>,
}

fn parse_method_calls(lang_server_trait: &ItemImpl) -> Vec<LspTypeChannel> {
    let mut calls = Vec::new();

    for item in &lang_server_trait.items {
        let method = match item {
            ImplItem::Fn(m) => m,
            _ => continue,
        };

        let params = method.sig.inputs.iter().nth(1).and_then(|arg| match arg {
            FnArg::Typed(pat) => Some(&*pat.ty),
            _ => None,
        });

        // let result = match &method.sig.output {
        //     ReturnType::Default => None,
        //     ReturnType::Type(_, ty) => Some(&**ty),
        // };

        let handler_name = &method.sig.ident;
        let tx_name = format_ident!("{}_tx", handler_name);
        let rx_name = format_ident!("{}_rx", handler_name);

        calls.push(LspTypeChannel {
            tx_name,
            rx_name,
            params,
        });
    }

    calls
}

fn gen_channel_struct(channels: &[LspTypeChannel]) -> proc_macro2::TokenStream {
    // unit type
    let unit_type = syn::Type::Tuple(syn::TypeTuple {
        paren_token: syn::token::Paren::default(),
        elems: syn::punctuated::Punctuated::new(),
    });

    let channel_declarations: proc_macro2::TokenStream = channels
        .iter()
        .map(|channel| {
            let tx = &channel.tx_name;
            let rx = &channel.rx_name;
            let params = channel.params;

            // if params is None we need to use the type of () as the default
            let params = match params {
                Some(params) => params,
                None => &unit_type,
            };
            quote! {
                pub #tx: tokio::sync::broadcast::Sender<#params>,
                pub #rx: tokio::sync::broadcast::Receiver<#params>,
            }
        })
        .collect();

    let channel_instantiations: proc_macro2::TokenStream = channels
        .iter()
        .map(|channel| {
            let tx = &channel.tx_name;
            let rx = &channel.rx_name;
            quote! {
                let (#tx, #rx) = tokio::sync::broadcast::channel(100);
            }
        })
        .collect();

    let channel_assignments: proc_macro2::TokenStream = channels
        .iter()
        .map(|channel| {
            let tx = &channel.tx_name;
            let rx = &channel.rx_name;
            quote! {
                #tx,
                #rx,
            }
        })
        .collect();

    quote! {
        pub struct LspChannels {
            #channel_declarations
        }

        impl LspChannels {
            pub fn new() -> Self {
                #channel_instantiations
                Self {
                    #channel_assignments
                }
            }
        }
    }
}
