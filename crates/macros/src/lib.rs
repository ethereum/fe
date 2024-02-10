mod kw;

#[proc_macro]
pub fn define_keywords(attrs: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match kw::define_keywords(attrs) {
        Ok(tokens) => tokens,
        Err(e) => e.to_compile_error().into(),
    }
}

type Error = syn::Error;
type Result<T> = syn::Result<T>;
