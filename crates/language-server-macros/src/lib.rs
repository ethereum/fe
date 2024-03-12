extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, FnArg, ImplItem, ItemImpl, ReturnType};

/// Macro for generating tokio channels from [`lsp-types`](https://docs.rs/lsp-types).
///
/// This procedural macro annotates the `tower_lsp::LanguageServer` trait implementation and generates
/// a struct full of tokio mpsc channels that can be used to signal the server to handle
/// defined requests and notifications.
#[proc_macro_attribute]
pub fn message_channels(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // let attr = parse_macro_input!(attr as Option<syn::Ident>);
    let channel_senders_struct_name = format_ident!(
        "MessageSenders",
        // attr.clone().map_or("MessageSenders".to_string(), |attr| attr.to_string())
    );

    let channel_receivers_struct_name = format_ident!(
        "MessageReceivers",
        // attr.map_or("MessageReceivers".to_string(), |attr| attr.to_string())
    );

    let lang_server_trait_impl = parse_macro_input!(item as ItemImpl);

    let method_calls = parse_method_calls(&lang_server_trait_impl);
    let channel_struct = gen_channel_structs(
        &method_calls,
        channel_senders_struct_name,
        channel_receivers_struct_name,
    );

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
    stream_name: syn::Ident,
    sender_fn_name: syn::Ident,
    // subscribe_fn_name: syn::Ident,
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
        let stream_name = format_ident!("{}_stream", handler_name);
        let sender_fn_name = format_ident!("send_{}", handler_name);

        let rx_name = format_ident!("{}_rx", handler_name);

        calls.push(MessageTypeChannel {
            tx_name,
            stream_name,
            rx_name,
            sender_fn_name,
            params,
            result,
        });
    }

    calls
}

fn gen_channel_structs(
    channels: &[MessageTypeChannel],
    channel_senders_struct_name: syn::Ident,
    channel_receivers_struct_name: syn::Ident,
) -> proc_macro2::TokenStream {
    // unit type
    let unit_type = syn::Type::Tuple(syn::TypeTuple {
        paren_token: syn::token::Paren::default(),
        elems: syn::punctuated::Punctuated::new(),
    });

    let channel_senders_declarations: proc_macro2::TokenStream = channels
        .iter()
        .map(|channel| {
            let tx = &channel.tx_name;
            let params = channel.params;
            let result = channel.result;

            // if params is None we need to use the type of () as the default
            let params = match params {
                Some(params) => params,
                None => &unit_type,
            };

            let sender_type = match result {
                Some(result) => quote! { tokio::sync::mpsc::Sender<(#params, tokio::sync::oneshot::Sender<#result>)> },
                None => quote! { tokio::sync::mpsc::Sender<#params> },
            };

            quote! {
                pub #tx: #sender_type,
            }
        })
        .collect();

    let channel_receivers_declarations: proc_macro2::TokenStream = channels
        .iter()
        .map(|channel| {
            let stream_name = &channel.stream_name;
            let params = channel.params;
            let result = channel.result;

            // if params is None we need to use the type of () as the default
            let params = match params {
                Some(params) => params,
                None => &unit_type,
            };
            let stream_type = match result {
                Some(result) => quote! { tokio_stream::wrappers::ReceiverStream<(#params, tokio::sync::oneshot::Sender<#result>)> },
                None => quote! { tokio_stream::wrappers::ReceiverStream<#params> },
            };

            quote! {
                pub #stream_name: #stream_type,
            }
        })
        .collect();

    let channel_instantiations: proc_macro2::TokenStream = channels
        .iter()
        .map(|channel| {
            let tx = &channel.tx_name;
            let rx = &channel.rx_name;
            quote! {
                let (#tx, #rx) = tokio::sync::mpsc::channel(10000);
            }
        })
        .collect();

    let channel_senders_assignments: proc_macro2::TokenStream = channels
        .iter()
        .map(|channel| {
            let tx = &channel.tx_name;
            quote! {
                #tx,
            }
        })
        .collect();

    let channel_receivers_assignments: proc_macro2::TokenStream = channels
        .iter()
        .map(|channel| {
            let stream_name = &channel.stream_name;
            let rx = &channel.rx_name;
            quote! {
                // #rx,
                #stream_name: tokio_stream::wrappers::ReceiverStream::new(#rx),
            }
        })
        .collect();

    let sender_dispatch_functions: proc_macro2::TokenStream = channels
        .iter()
        .map(|channel| {
            let tx = &channel.tx_name;
            let params = &channel.params;
            let sender_fn_name = &channel.sender_fn_name;
            let sender_fn_result = match channel.result {
                Some(result) => quote! {tokio::sync::oneshot::Receiver<#result>},
                None => quote! {()},
            };

            let payload = match params {
                Some(_params) => quote! { params },
                None => quote! { () },
            };

            let send_payload = match channel.result {
                Some(result) => quote! {
                    let (oneshot_tx, oneshot_rx) = tokio::sync::oneshot::channel::<#result>();
                    let mpsc = self.#tx.clone();
                    info!("sending oneshot sender: {:?}", #payload);
                    match mpsc.send((#payload, oneshot_tx)).await {
                        Ok(_) => info!("sent oneshot sender"),
                        Err(e) => error!("failed to send oneshot sender"),
                    }
                    info!("returning oneshot receiver: {:?}", oneshot_rx);
                    oneshot_rx
                },
                None => quote! {
                    match self.#tx.send(#payload).await {
                        Ok(_) => info!("sent notification"),
                        Err(e) => error!("failed to send notification: {:?}", e),
                    }
                },
            };

            let dispatcher_fn = match params {
                Some(params) => quote! {
                    pub async fn #sender_fn_name(&self, params: #params) -> #sender_fn_result {
                        #send_payload
                    }
                },
                None => quote! {
                    pub async fn #sender_fn_name(&self) -> #sender_fn_result {
                        #send_payload
                    }
                },
            };

            quote! {
                #dispatcher_fn
            }
        })
        .collect();

    quote! {
        pub struct #channel_receivers_struct_name {
            #channel_receivers_declarations
        }

        pub struct #channel_senders_struct_name {
            #channel_senders_declarations
        }

        pub fn setup_message_channels() -> (#channel_senders_struct_name, #channel_receivers_struct_name) {
            #channel_instantiations
            (
                #channel_senders_struct_name {
                    #channel_senders_assignments
                },
                #channel_receivers_struct_name {
                    #channel_receivers_assignments
                }
            )
        }

        impl #channel_senders_struct_name {
            #sender_dispatch_functions
        }
    }
}
