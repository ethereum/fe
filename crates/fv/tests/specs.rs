#![cfg(feature = "kevm-backend")]

/// Checks if a contract spec is valid.
macro_rules! test_spec {
    ($name:ident, $src_path:expr, $spec_path:expr) => {
        #[test]
        fn $name() {
            let src = fe_test_files::fixture(concat!("kspecs/", $src_path));
            let spec = fe_test_files::fixture(concat!("kspecs/", $spec_path));

            if let Err(output) =
                fv::run_spec(&stringify!($name).replace("_", "-"), $src_path, &src, &spec)
            {
                panic!("{}", output)
            }
        }
    };
}

/// Checks if a contract spec is invalid.
macro_rules! test_spec_invalid {
    ($name:ident, $src_path:expr, $spec_path:expr) => {
        #[test]
        fn $name() {
            let src = fe_test_files::fixture(concat!("kspecs/", $src_path));
            let spec = fe_test_files::fixture(concat!("kspecs/", $spec_path));

            match fv::run_spec(&stringify!($name).replace("_", "-"), $src_path, &src, &spec) {
                Ok(()) => panic!("spec is valid"),
                Err(output) => {
                    // we want to make sure it didn't fail for some other reason
                    if !output.contains("the claimed implication is not valid") {
                        panic!("spec claim was not checked {}", output)
                    }
                }
            }
        }
    };
}

test_spec! { sanity_returns_42, "sanity/foo.fe", "sanity/returns_42.k" }
test_spec! { sanity_returns_in, "sanity/foo.fe", "sanity/returns_in.k" }
test_spec! { sanity_returns_in_cond1, "sanity/foo.fe", "sanity/returns_in_cond1.k" }
test_spec! { sanity_returns_in_cond2, "sanity/foo.fe", "sanity/returns_in_cond2.k" }

// these are just for extra sanity
test_spec_invalid! { sanity_returns_42_invalid, "sanity/foo.fe", "sanity/returns_42_invalid.k" }
test_spec_invalid! { sanity_returns_in_invalid, "sanity/foo.fe", "sanity/returns_in_invalid.k" }
test_spec_invalid! { sanity_returns_in_cond2_invalid, "sanity/foo.fe", "sanity/returns_in_cond2_invalid.k" }
