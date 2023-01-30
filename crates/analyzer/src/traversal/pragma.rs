use crate::errors;
use fe_common::diagnostics::{Diagnostic, Label};
use fe_parser::ast;
use fe_parser::node::Node;
use semver::{Version, VersionReq};

pub fn check_pragma_version(stmt: &Node<ast::Pragma>) -> Option<Diagnostic> {
    let version_requirement = &stmt.kind.version_requirement;
    // This can't fail because the parser already validated it
    let requirement =
        VersionReq::parse(&version_requirement.kind).expect("Invalid version requirement");
    let actual_version =
        Version::parse(env!("CARGO_PKG_VERSION")).expect("Missing package version");

    if requirement.matches(&actual_version) {
        None
    } else {
        Some(errors::fancy_error(
            format!(
                "The current compiler version {actual_version} doesn't match the specified requirement"
            ),
            vec![Label::primary(
                version_requirement.span,
                "The specified version requirement",
            )],
            vec![format!(
                "Note: Use `pragma {actual_version}` to make the code compile"
            )],
        ))
    }
}
