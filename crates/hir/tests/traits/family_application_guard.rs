//! A family application written with a selected definition is reduced in one
//! place, when the written type is lowered, and a stored field type in one
//! place, when it is instantiated; every other reader of a type sees the
//! reduced type and needs no case of its own for a family.
//!
//! These checks read the compiler's source:
//! - the two reduce points still reduce;
//! - outside an explicit list of files, each with its reason, no code matches
//!   on a family, builds one, or reduces one, so no consumer handles family
//!   applications piecemeal again.

use std::path::{Path, PathBuf};

/// The `.rs` files under `crate_dir/src`, with their text.
fn sources(crate_dir: &Path) -> Vec<(PathBuf, String)> {
    fn visit(dir: &Path, out: &mut Vec<(PathBuf, String)>) {
        for entry in std::fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                visit(&path, out);
            } else if path.extension().is_some_and(|ext| ext == "rs") {
                // A checkout may have Windows line endings.
                let text = std::fs::read_to_string(&path)
                    .unwrap()
                    .replace("\r\n", "\n");
                out.push((path, text));
            }
        }
    }
    let mut out = Vec::new();
    visit(&crate_dir.join("src"), &mut out);
    out.sort();
    out
}

fn crates_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf()
}

/// The source of `path` from `crates/`, with forward slashes.
fn relative(path: &Path) -> String {
    path.strip_prefix(crates_dir())
        .unwrap()
        .to_string_lossy()
        .replace('\\', "/")
}

/// The text of the function `name` in `text`, up to the next item.
fn function<'a>(text: &'a str, name: &str) -> &'a str {
    let start = text
        .find(&format!("fn {name}"))
        .unwrap_or_else(|| panic!("function `{name}` is missing"));
    let rest = &text[start..];
    let end = rest[1..]
        .find("\nfn ")
        .or_else(|| rest[1..].find("\npub(crate) fn "))
        .or_else(|| rest[1..].find("\npub fn "))
        .map_or(rest.len(), |end| end + 1);
    &rest[..end]
}

/// What names a family directly: matching on one, building one, or reducing
/// one.
const FAMILY_FORMS: &[&str] = &[
    "TyData::TypeFamily",
    "as_family_application(",
    "family_as_projection(",
    "type_family(",
    "type_family_with_args(",
    "reduce_family_application(",
    "is_type_family(",
];

/// The files that may name a family directly, each with the reason.
const ALLOWED: &[(&str, &str)] = &[
    (
        "hir/src/analysis/ty/ty_def.rs",
        "the representation: constructors, kinds, flags, printing, and the reducer",
    ),
    (
        "hir/src/analysis/name_resolution/path_resolver.rs",
        "the reduce point for a written type",
    ),
    (
        "hir/src/analysis/ty/normalize.rs",
        "normalization reduces applications in generic code once their parameters are known, and the layout root collector reduces them with their arguments",
    ),
    (
        "hir/src/core/semantic/mod.rs",
        "an associated type's definition is bound as a family, and a family is named by its projection",
    ),
    (
        "hir/src/analysis/ty/fold.rs",
        "folding rebuilds a family, whose own parameters stay bound",
    ),
    (
        "hir/src/analysis/ty/subst.rs",
        "substitution rebuilds a family, whose own parameters stay bound",
    ),
    (
        "hir/src/analysis/ty/visitor.rs",
        "visiting walks a family's arguments and definition",
    ),
    (
        "hir/src/analysis/ty/unify.rs",
        "two unapplied families unify by their definitions; a family's own parameters are not inference variables",
    ),
    (
        "hir/src/analysis/ty/const_ty.rs",
        "constant canonicalization walks and rebuilds a family",
    ),
    (
        "hir/src/analysis/ty/ty_lower.rs",
        "the roots forwarded from an impl's parameters are collected in a family's definition",
    ),
    (
        "hir/src/analysis/ty/trait_resolution/mod.rs",
        "an unapplied family, passed as a type constructor, is well formed",
    ),
    (
        "hir/src/analysis/ty/trait_resolution/table_solver.rs",
        "the depth of a type counts a family's definition",
    ),
    (
        "hir/src/analysis/ty/ty_error.rs",
        "the arguments of a written family application are checked as written, before lowering replaces the application",
    ),
    (
        "hir/src/analysis/diagnostics.rs",
        "a message shows a family as its projection",
    ),
    (
        "hir/src/analysis/name_resolution/visibility_checker.rs",
        "a family is as visible as its associated type",
    ),
    (
        "hir/src/diagnosable.rs",
        "an impl's definition is checked as a family or as a plain type",
    ),
    (
        "mir/src/runtime/stable_key.rs",
        "an unapplied family passed as a type constructor has a stable key",
    ),
    (
        "codegen/src/function_symbols.rs",
        "an unapplied family passed as a type constructor names no symbol of its own",
    ),
];

#[test]
fn a_family_is_named_directly_only_where_listed() {
    let mut found = Vec::new();
    for krate in ["hir", "mir", "codegen"] {
        for (path, text) in sources(&crates_dir().join(krate)) {
            let path = relative(&path);
            if ALLOWED.iter().any(|(allowed, _)| *allowed == path) {
                continue;
            }
            for (idx, line) in text.lines().enumerate() {
                if line.trim_start().starts_with("//") {
                    continue;
                }
                if let Some(form) = FAMILY_FORMS.iter().find(|form| line.contains(*form)) {
                    found.push(format!("{path}:{}: `{form}`", idx + 1));
                }
            }
        }
    }
    assert!(
        found.is_empty(),
        "a family is named outside the reduce points and the listed files; read the \
         type as reduced instead, or add the file with its reason:\n{}",
        found.join("\n")
    );
}

#[test]
fn every_listed_file_still_names_a_family() {
    for (path, reason) in ALLOWED {
        let text = std::fs::read_to_string(crates_dir().join(path)).unwrap();
        assert!(
            FAMILY_FORMS.iter().any(|form| text.contains(form)),
            "{path} ({reason}) no longer names a family; remove it from the list"
        );
    }
}

#[test]
fn the_reduce_points_reduce() {
    let resolver = std::fs::read_to_string(
        crates_dir().join("hir/src/analysis/name_resolution/path_resolver.rs"),
    )
    .unwrap();
    let lowering = function(&resolver, "associated_type_resolution");
    assert!(
        lowering.contains(".reduce_family_application(db)"),
        "a written associated type is no longer reduced when it is lowered:\n{lowering}"
    );
    // The shorthand, `Self::F<..>` in an impl's definitions, and the
    // qualified spelling.
    assert_eq!(
        resolver
            .matches("return Ok(associated_type_resolution(")
            .count(),
        3,
        "a written associated type is resolved without the reduce point"
    );

    let adt = std::fs::read_to_string(crates_dir().join("hir/src/analysis/ty/adt_def.rs")).unwrap();
    let instantiation = function(&adt, "instantiate_adt_field_shape_in_mode");
    assert!(
        instantiation.contains("resolve_instantiated_field(db, adt, ty)"),
        "an instantiated field type is no longer resolved:\n{instantiation}"
    );
    let resolve = function(&adt, "resolve_instantiated_field");
    assert!(
        resolve.contains("normalize_ty("),
        "an instantiated field type is no longer normalized:\n{resolve}"
    );
}

/// A field type of an ADT applied to arguments is instantiated in one place,
/// `instantiate_adt_field_shape`, which stores it resolved; a reader that
/// instantiates the declared field type itself sees the associated types it
/// names unresolved.
#[test]
fn field_types_are_instantiated_in_one_place() {
    const OWNERS: &[&str] = &[
        "hir/src/analysis/ty/adt_def.rs",
        "hir/src/core/semantic/mod.rs",
    ];
    let mut found = Vec::new();
    for krate in ["hir", "mir", "codegen"] {
        for (path, text) in sources(&crates_dir().join(krate)) {
            let path = relative(&path);
            if OWNERS.contains(&path.as_str()) {
                continue;
            }
            let lines: Vec<&str> = text.lines().collect();
            for (idx, line) in lines.iter().enumerate() {
                let declared = line.contains(".field_tys(") || line.contains(".fields(db)[");
                let instantiated = lines[idx..lines.len().min(idx + 5)].iter().any(|line| {
                    line.contains(".instantiate(db") || line.contains(".instantiate(self.db")
                });
                if declared && instantiated {
                    found.push(format!("{path}:{}", idx + 1));
                }
            }
        }
    }
    assert!(
        found.is_empty(),
        "a field type is instantiated outside `instantiate_adt_field_shape`; use it, or \
         `VariantView::instantiated_field_tys`:\n{}",
        found.join("\n")
    );
}
