extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, FnArg, ImplItem, ItemImpl, ReturnType};

/// Macro for generating tokio channels from [`lsp-types`](https://docs.rs/lsp-types).
///
/// This procedural macro annotates the `tower_lsp::LanguageServer` trait implementation and generates
/// a struct full of tokio broadcast channels that can be used to signal the server to handle
/// defined requests and notifications.
#[proc_macro_attribute]
pub fn message_channels(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attr = parse_macro_input!(attr as Option<syn::Ident>);
    let channel_struct_name = format_ident!("{}", attr.map_or("MessageChannels".to_string(), |attr| attr.to_string()));

    let lang_server_trait_impl = parse_macro_input!(item as ItemImpl);

    let method_calls = parse_method_calls(&lang_server_trait_impl);
    let channel_struct = gen_channel_struct(&method_calls, channel_struct_name);

    let tokens = quote! {
        #channel_struct
        #lang_server_trait_impl
    };

    tokens.into()
    // item
}

struct MessageTypeChannel<'a> {
    // handler_name: &'a syn::Ident,
    tx_name: syn::Ident,
    sender_fn_name: syn::Ident,
    subscribe_fn_name: syn::Ident,
    rx_name: syn::Ident,
    params: Option<&'a syn::Type>,
    result: Option<&'a syn::Type>,
}

fn parse_method_calls(lang_server_trait: &ItemImpl) -> Vec<MessageTypeChannel> {
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

        let result = match &method.sig.output {
            ReturnType::Default => None,
            ReturnType::Type(_, ty) => Some(&**ty),
        };

        let handler_name = &method.sig.ident;
        let tx_name = format_ident!("{}_tx", handler_name);
        let sender_fn_name = format_ident!("send_{}", handler_name);
        let subscribe_fn_name = format_ident!("subscribe_{}", handler_name);

        let rx_name = format_ident!("{}_rx", handler_name);

        calls.push(MessageTypeChannel {
            tx_name,
            rx_name,
            sender_fn_name,
            subscribe_fn_name,
            params,
            result,
        });
    }

    calls
}

fn gen_channel_struct(channels: &[MessageTypeChannel], channel_struct_name: syn::Ident) -> proc_macro2::TokenStream {
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
            let result = channel.result;

            // if params is None we need to use the type of () as the default
            let params = match params {
                Some(params) => params,
                None => &unit_type,
            };

            let sender_type = match result {
                Some(result) => quote! { tokio::sync::broadcast::Sender<(#params, OneshotResponder<#result>)> },
                None => quote! { tokio::sync::broadcast::Sender<#params> },
            };

            let receiver_type = match result {
                Some(result) => quote! { tokio::sync::broadcast::Receiver<(#params, OneshotResponder<#result>)> },
                None => quote! { tokio::sync::broadcast::Receiver<#params> },
            };

            quote! {
                pub #tx: #sender_type,
                pub #rx: #receiver_type,
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

    let send_functions: proc_macro2::TokenStream = channels
        .iter()
        .map(|channel| {
            let tx = &channel.tx_name;
            // let rx = &channel.rx_name;
            let params = &channel.params;
            let params_type = match params {
                Some(params) => params,
                None => &unit_type,
            };
            let subscribe_fn_name = &channel.subscribe_fn_name;
            let sender_fn_name = &channel.sender_fn_name;
            let sender_fn_result = match channel.result {
                Some(result) => quote!{tokio::sync::oneshot::Receiver<#result>},
                None => quote!{()},
                // Some(result) => quote! { tokio::sync::broadcast::Receiver<(#params, OneshotResponder<tokio::sync::oneshot::Sender<#result>>)> },
                // None => quote! { tokio::sync::broadcast::Receiver<#params> },
            };
            let receiver_type = match channel.result {
                Some(result) => quote! { tokio::sync::broadcast::Receiver<(#params_type, OneshotResponder<#result>)> },
                None => quote! { tokio::sync::broadcast::Receiver<#params_type> },
            };

            let payload = match params {
                Some(_params) => quote! { params },
                None => quote! { () },
            };

            let send_payload = match channel.result {
                Some(result) => quote!{
                    let (tx, rx) = tokio::sync::oneshot::channel::<#result>();
                    let oneshot = OneshotResponder::from(tx);
                    let broadcast = self.#tx.clone();
                    info!("sending oneshot sender: {:?}", #payload);
                    match broadcast.send((#payload, oneshot)) {
                        Ok(_) => info!("sent oneshot sender"),
                        Err(e) => error!("failed to send oneshot sender"),
                    }
                    info!("returning oneshot receiver: {:?}", rx);
                    rx
                },
                None => quote!{
                    match self.#tx.send(#payload) {
                        Ok(_) => info!("sent notification"),
                        Err(e) => error!("failed to send notification: {:?}", e),
                    }
                },
            };

            let dispatcher_fn = match params {
                Some(params) => quote! {
                    pub fn #sender_fn_name(&self, params: #params) -> #sender_fn_result {
                        #send_payload
                    }
                },
                None => quote! {
                    pub fn #sender_fn_name(&self) -> #sender_fn_result {
                        #send_payload
                    }
                },
            };

            let subscriber_fn = match params {
                Some(_params) => quote! {
                    pub fn #subscribe_fn_name(&self) -> #receiver_type {
                        self.#tx.subscribe()
                    }
                },
                None => quote! {
                    pub fn #subscribe_fn_name(&self) -> #receiver_type {
                        self.#tx.subscribe()
                    }
                },
            };

            quote! {
                #dispatcher_fn
                #subscriber_fn
            }
        })
        .collect();

    quote! {
        pub struct #channel_struct_name {
            #channel_declarations
        }

        impl #channel_struct_name {
            pub fn new() -> Self {
                #channel_instantiations
                Self {
                    #channel_assignments
                }
            }
            #send_functions
        }
    }
}
