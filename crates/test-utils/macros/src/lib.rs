use std::{
    fs,
    path::{Path, PathBuf},
};

use quote::quote;

type Error = syn::Error;
type Result<T> = syn::Result<T>;

#[proc_macro]
pub fn build_snap_tests(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match expand(input) {
        Ok(ts) => ts,
        Err(err) => err.to_compile_error().into(),
    }
}

fn expand(input: proc_macro::TokenStream) -> Result<proc_macro::TokenStream> {
    let args: Args = syn::parse(input)?;

    let builder = SnapTestBuilder::from_args(args)?;
    builder.build().map(|ts| ts.into())
}

struct SnapTestBuilder {
    fixture_dir: PathBuf,
    snapshot_dir: PathBuf,
    target_fn: syn::Path,
    debug_snap: bool,
}

impl SnapTestBuilder {
    fn from_args(args: Args) -> Result<Self> {
        let workspace_root = cargo_workspace_dir();

        let fixture_dir: PathBuf = workspace_root.join(args.fixture_dir.value());
        let snapshot_dir: PathBuf = workspace_root.join(args.snapshot_dir.value());

        if !fixture_dir.is_dir() | !fixture_dir.exists() {
            return Err(Error::new_spanned(
                args.fixture_dir,
                format! {"invalid path for `fixture_dir`: `{}` is invalid path",
                fixture_dir.display()},
            ));
        } else if !snapshot_dir.is_dir() {
            return Err(Error::new_spanned(
                args.snapshot_dir,
                format! {"invalid path for `snapshot_dir`: `{}` is invalid path",
                snapshot_dir.display()},
            ));
        }

        Ok(Self {
            fixture_dir,
            snapshot_dir,
            target_fn: args.target_fn,
            debug_snap: args.debug_snap.value(),
        })
    }

    fn build(&self) -> Result<proc_macro2::TokenStream> {
        let mut tests = Vec::new();

        let dir = fs::read_dir(&self.fixture_dir).unwrap();
        for fixture in dir.flatten() {
            let fixture_path = fixture.path();
            if fixture_path.is_file()
                && fixture_path.extension().and_then(|ext| ext.to_str()) == Some("fe")
            {
                tests.push(self.build_test(&fixture_path));
            }
        }

        Ok(quote! {
            #(#tests)*
        })
    }

    fn build_test(&self, fixture_file: &Path) -> proc_macro2::TokenStream {
        let file_name = fixture_file.file_name().unwrap().to_str().unwrap();
        let file_stem_name = fixture_file.file_stem().unwrap().to_str().unwrap();
        let test_fn_ident = syn::Ident::new(file_stem_name, proc_macro2::Span::call_site());
        let fixture_file = fixture_file.to_str().unwrap();

        let snapshot_dir = self.snapshot_dir.to_str().unwrap();
        let target_fn = &self.target_fn;
        let snapshot = syn::Ident::new("snapshot", proc_macro2::Span::call_site());
        let get_snapshot = if self.debug_snap {
            quote! {
                let #snapshot = format!("{:#?}", #target_fn(&input));
            }
        } else {
            quote! {
                let #snapshot = format!("{}", #target_fn(&input));
            }
        };

        quote! {
            #[test]
            fn #test_fn_ident() {
                let input = ::std::fs::read_to_string(#fixture_file).unwrap();
                #get_snapshot
                let mut settings = ::fe_compiler_test_utils::_macro_support::_insta::Settings::new();
                settings.set_snapshot_path(#snapshot_dir);
                settings.set_input_file(#file_name);
                settings.set_prepend_module_to_snapshot(false);


                ::fe_compiler_test_utils::_insta_assert_snapshot!{
                    #snapshot,
                    settings
                }
            }
        }
    }
}

// FIXME: This is quite hacky and should be removed when `span::source_file` is
// stabilized.
// See [`Tracking issue for proc_macro::Span inspection APIs #54725`](https://github.com/rust-lang/rust/issues/54725) for more information.
fn cargo_workspace_dir() -> PathBuf {
    let mut cargo_workspace_dir: PathBuf = env!["CARGO_MANIFEST_DIR"].into();

    for _ in 0..2 {
        cargo_workspace_dir.pop();
    }

    cargo_workspace_dir
}
struct Args {
    fixture_dir: syn::LitStr,
    snapshot_dir: syn::LitStr,
    target_fn: syn::Path,
    debug_snap: syn::LitBool,
}

impl syn::parse::Parse for Args {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let error_msg = "expected `build_snap_tests! { 
            fixture_dir: ..,  
            snapshot_dir: ..,
            target_fn: ..,
            debug_snap: ..
        }`";

        let ident = input.parse::<syn::Ident>()?;
        if ident != "fixture_dir" {
            return Err(Error::new_spanned(ident, error_msg));
        }
        input.parse::<syn::Token![:]>()?;
        let fixture_dir = input.parse::<syn::LitStr>()?;
        input.parse::<syn::Token![,]>()?;

        let ident = input.parse::<syn::Ident>()?;
        if ident != "snapshot_dir" {
            return Err(Error::new_spanned(ident, error_msg));
        }
        input.parse::<syn::Token![:]>()?;
        let snapshot_dir = input.parse::<syn::LitStr>()?;
        input.parse::<syn::Token![,]>()?;

        let ident = input.parse::<syn::Ident>()?;
        if ident != "target_fn" {
            return Err(Error::new_spanned(ident, error_msg));
        }
        input.parse::<syn::Token![:]>()?;
        let target_fn = input.parse::<syn::Path>()?;
        input.parse::<syn::Token![,]>()?;

        let ident = input.parse::<syn::Ident>()?;
        if ident != "debug_snap" {
            return Err(Error::new_spanned(ident, error_msg));
        }
        input.parse::<syn::Token![:]>()?;
        let debug_snap = input.parse::<syn::LitBool>()?;

        Ok(Self {
            fixture_dir,
            snapshot_dir,
            target_fn,
            debug_snap,
        })
    }
}
