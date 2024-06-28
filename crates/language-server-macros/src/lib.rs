extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, FnArg, ImplItem, ItemImpl, ReturnType};

/// Generates message channels and dispatch methods for a `tower_lsp::LanguageServer` implementation.
///
/// This macro generates two structs:
/// - `MessageSenders`: Contains `tokio::sync::mpsc::UnboundedSender` channels for each method in the `LanguageServer` trait.
/// - `MessageReceivers`: Contains `tokio_stream::wrappers::UnboundedReceiverStream` streams for each method in the `LanguageServer` trait.
///
/// It also generates a `setup_message_channels` function that initializes the channels and returns instances of the `MessageSenders` and `MessageReceivers` structs.
///
/// # Example
///
/// ```rust,ignore
/// use tower_lsp::LanguageServer;
///
/// #[language_server_macros::message_channels]
/// #[tower_lsp::async_trait]
/// impl LanguageServer for Server {
///     // ...
/// }
/// ```
#[proc_macro_attribute]
pub fn message_channels(_attr: TokenStream, item: TokenStream) -> TokenStream {
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
                Some(result) => quote! { tokio::sync::mpsc::UnboundedSender<(#params, tokio::sync::oneshot::Sender<#result>)> },
                None => quote! { tokio::sync::mpsc::UnboundedSender<#params> },
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
                Some(result) => quote! { tokio_stream::wrappers::UnboundedReceiverStream<(#params, tokio::sync::oneshot::Sender<#result>)> },
                None => quote! { tokio_stream::wrappers::UnboundedReceiverStream<#params> },
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
                let (#tx, #rx) = tokio::sync::mpsc::unbounded_channel();
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
                #stream_name: tokio_stream::wrappers::UnboundedReceiverStream::new(#rx),
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
                    mpsc.send((#payload, oneshot_tx)).expect("send payload with oneshot");
                    info!("returning oneshot receiver: {:?}", oneshot_rx);
                    oneshot_rx
                },
                None => quote! {
                    self.#tx.send(#payload).expect("send payload");
                },
            };

            let dispatcher_fn = match params {
                Some(params) => quote! {
                    /// Forward the LSP request parameters to the designated channel.
                    ///
                    /// An oneshot receiver is returned which can optionally be used to get a response back from the channel listener.
                    pub fn #sender_fn_name(&self, params: #params) -> #sender_fn_result {
                        #send_payload
                    }
                },
                None => quote! {
                    /// Forward the LSP notification parameters to the designated channel.
                    pub fn #sender_fn_name(&self) -> #sender_fn_result {
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
        /// Struct containing `tokio::sync::mpsc::UnboundedSender` channels for each method in the `LanguageServer` trait.
        ///
        /// This struct is generated by the `#[message_channels]` macro. For each method in the `LanguageServer` trait,
        /// it generates a corresponding field with a name in the format `<method_name>_tx`.
        ///
        /// For each implemented LSP notification method, a channel of type `tokio::sync::mpsc::UnboundedSender<Params>` is generated, where `Params` is the method's parameter type.
        /// For each implemented LSP request methods, a channel of type `tokio::sync::mpsc::UnboundedSender<(Params, tokio::sync::oneshot::Sender<Result>)` is generated, where `Params` is the method's parameter type and `Result` is the method's return type.
        ///
        /// The macro also generates corresponding `send_<method_name>` helper methods for each implemented LSP method to allow sending
        /// requests or notifications through the respective channels.
        ///
        /// # Example
        ///
        /// ```rust,ignore
        /// use tower_lsp::{LanguageServer, Client, jsonrpc::Result};
        /// use async_lsp::lsp_types::{InitializeParams, InitializeResult};
        ///
        /// struct Backend {
        ///     messaging: MessageSenders,
        ///     client: Client,
        /// }
        ///
        /// #[tower_lsp::async_trait]
        /// impl LanguageServer for Backend {
        ///     async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        ///         let rx = self.messaging.send_initialize(params);
        ///
        ///         match rx.await {
        ///             Ok(result) => {
        ///                 self.client.log_message(async_lsp::lsp_types::MessageType::INFO, "Server initialized!").await;
        ///                 Ok(result)
        ///             }
        ///             Err(e) => {
        ///                 self.client.log_message(async_lsp::lsp_types::MessageType::ERROR, format!("Failed to initialize: {:?}", e)).await;
        ///                 Err(jsonrpc::Error::internal_error())
        ///             }
        ///         }
        ///     }
        ///
        ///     // Other LanguageServer methods...
        /// }
        /// ```
        pub struct #channel_receivers_struct_name {
            #channel_receivers_declarations
        }

        /// Struct containing `tokio_stream::wrappers::UnboundedReceiverStream` streams for each implemented `LanguageServer` trait method.
        ///
        /// This struct is generated by the `#[message_channels]` macro. For each implemented method of the `LanguageServer` trait,
        /// it generates a corresponding field with a name in the format `<method_name>_stream`.
        ///
        /// The type of each field depends on the signature of the corresponding `LanguageServer` method:
        /// - If the method has a return type, the field type is `tokio_stream::wrappers::UnboundedReceiverStream<(Params, tokio::sync::oneshot::Sender<Result>)>`,
        ///   where `Params` is the type of the method's parameter and `Result` is the return type.
        /// - If the method doesn't have a return type, the field type is `tokio_stream::wrappers::UnboundedReceiverStream<Params>`.
        ///
        /// These streams can be used to handle incoming requests or notifications for each `LanguageServer` method.
        ///
        /// # Example
        ///
        /// ```rust,ignore
        /// let (senders, receivers) = setup_message_channels();
        /// let mut initialized_stream = receivers.initialize_stream.fuse();
        /// loop {
        ///     select! {
        ///         Some((params, responder)) = initialized_stream.next() => {
        ///             // Handle initialization request
        ///             let result = async_lsp::lsp_types::InitializeResult { ... };
        ///             let _ = responder.send(Ok(result));
        ///         }
        ///         // ...
        ///     }
        /// }
        /// ```
        pub struct #channel_senders_struct_name {
            #channel_senders_declarations
        }

        /// Initializes the message channels and returns instances of the `MessageSenders` and `MessageReceivers` structs.
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
