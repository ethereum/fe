#[doc(hidden)]
pub use insta as _insta;

// NOTE: Borrowed from `insta` implementation from
// [here](https://docs.rs/insta/1.26/src/insta/macros.rs.html#2-16)
/// Utility macro to return the name of the current function.
#[doc(hidden)]
#[macro_export]
macro_rules! _function_name {
    () => {{
        fn f() {}
        fn type_name_of_val<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let mut name = type_name_of_val(f).strip_suffix("::f").unwrap_or("");
        while let Some(rest) = name.strip_suffix("::{{closure}}") {
            name = rest;
        }
        name
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! _insta_assert_snapshot {
    ($value: expr, $setting: expr) => {
        $setting.bind(|| {
            $crate::_macro_support::_insta::_macro_support::assert_snapshot(
                $crate::_macro_support::_insta::_macro_support::AutoName.into(),
                &$value,
                env!("CARGO_MANIFEST_DIR"),
                $crate::_function_name!(),
                module_path!(),
                file!(),
                line!(),
                stringify!($value),
            )
            .unwrap()
        })
    };
}

/// Build a set of snapshot tests from a directory of fixtures.
/// `fixture_dir` and `snapshot_dir` should be relative to the workspace root.
/// `target_fn` should take `&str` and return `<T: Display>`.
#[macro_export]
macro_rules! build_snap_tests {
    ($fixture_dir: literal, $snapshot_dir: literal, $target_fn: path) => {
        fe_compiler_test_utils::_build_snap_tests! {
            fixture_dir: $fixture_dir,
            snapshot_dir: $snapshot_dir,
            target_fn: $target_fn,
            debug_snap: false
        }
    };
}

/// Build a set of snapshot tests from a directory of fixtures.
/// `fixture_dir` and `snapshot_dir` should be relative to the workspace root.
/// `target_fn` should take `&str` and return `<T: Debug>`.
#[macro_export]
macro_rules! build_debug_snap_tests {
    ($fixture_dir: literal, $snapshot_dir: literal, $target_fn: path) => {
        fe_compiler_test_utils::_build_snap_tests! {
            fixture_dir: $fixture_dir,
            snapshot_dir: $snapshot_dir,
            target_fn: $target_fn,
            debug_snap: true
        }
    };
}
