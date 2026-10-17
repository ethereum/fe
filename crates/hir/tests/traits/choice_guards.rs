//! Guards for trait choices (law L1/L2, guard G1). A choice is decided once,
//! by the checker, and every later stage reads the recorded decision. These
//! tests read the compiler's source and fail when a decision or an evidence
//! denotation is made anywhere else, so that a new consumer cannot quietly
//! re-derive a choice from syntax.

use std::{fs, path::Path};

fn sources() -> Vec<(String, String)> {
    fn walk(dir: &Path, out: &mut Vec<(String, String)>) {
        for entry in fs::read_dir(dir).expect("source directory is readable") {
            let path = entry.expect("directory entry").path();
            if path.is_dir() {
                walk(&path, out);
            } else if path.extension().is_some_and(|ext| ext == "rs") {
                let rel = path
                    .to_string_lossy()
                    .replace('\\', "/")
                    .split("/crates/hir/")
                    .last()
                    .expect("path has a last segment")
                    .to_string();
                out.push((rel, fs::read_to_string(&path).expect("source is readable")));
            }
        }
    }
    let mut out = Vec::new();
    let hir = Path::new(env!("CARGO_MANIFEST_DIR"));
    walk(&hir.join("src"), &mut out);
    // Runtime lowering is a consumer too.
    walk(&hir.join("../mir/src"), &mut out);
    out
}

/// Files in which `needle` occurs, other than `allowed`.
fn occurrences_outside(needle: &str, allowed: &[&str]) -> Vec<String> {
    sources()
        .into_iter()
        .filter(|(path, text)| text.contains(needle) && !allowed.contains(&path.as_str()))
        .map(|(path, _)| path)
        .collect()
}

const CHECKER: &str = "src/analysis/ty/ty_check/choice.rs";
const REPRESENTATION: &str = "src/analysis/ty/trait_choice.rs";

#[test]
fn only_the_checker_denotes_evidence() {
    // `denote_evidence` is the one reader of evidence-source syntax.
    assert_eq!(
        occurrences_outside("fn denote_evidence", &[CHECKER]),
        Vec::<String>::new()
    );
    assert_eq!(
        occurrences_outside("denote_evidence(", &[CHECKER]),
        Vec::<String>::new()
    );
}

#[test]
fn only_the_checker_makes_pending_evidence_choices() {
    assert_eq!(
        occurrences_outside(
            "Choice::Slot(SlotRef::EvidenceParam",
            &[CHECKER, REPRESENTATION]
        ),
        Vec::<String>::new()
    );
}

#[test]
fn only_the_checker_proves_known_choices() {
    assert_eq!(
        occurrences_outside("KnownChoice::prove(", &[CHECKER, REPRESENTATION]),
        Vec::<String>::new()
    );
    // A named implementation is its own choice in its body (the checker's
    // seed) and for its constants' recursion check.
    assert_eq!(
        occurrences_outside(
            "KnownChoice::own(",
            &[REPRESENTATION, CHECKER, "src/diagnosable.rs"]
        ),
        Vec::<String>::new()
    );
}

#[test]
fn evidence_goals_are_read_only_where_choices_are_decided_or_verified() {
    // Reading `Evidence<G>`'s goal is how a consumer would start re-deriving a
    // choice from an evidence value's type.
    assert_eq!(
        occurrences_outside(
            "evidence_goal(",
            &[
                CHECKER,
                "src/analysis/ty/corelib.rs",
                "src/analysis/semantic/consts.rs",
            ]
        ),
        Vec::<String>::new()
    );
}

#[test]
fn choices_resolve_through_one_function() {
    // Every consumer turns a recorded choice into an implementation through
    // `SlotChoices::resolve` or `AssocConstUse::selected_impl`; nothing reads a
    // frame after checking.
    assert_eq!(
        occurrences_outside(
            "choice_frames",
            &[
                CHECKER,
                "src/analysis/ty/ty_check/env.rs",
                "src/analysis/ty/ty_check/expr.rs",
                // These only stamp an obligation with the current frame,
                // which `frame_proves` reads.
                "src/analysis/ty/ty_check/mod.rs",
                "src/analysis/ty/ty_check/callable.rs",
                "src/analysis/ty/ty_check/stmt.rs",
            ]
        ),
        Vec::<String>::new()
    );
}

#[test]
fn trait_method_bodies_are_chosen_where_choices_are_read() {
    // Guard G2: ordinary method lookup is reached only after a use's recorded
    // choice was read, so it cannot answer a slot that a choice opened.
    assert_eq!(
        occurrences_outside(
            "resolve_trait_method_instance(",
            &[
                "src/analysis/ty/trait_def.rs",
                "src/analysis/ty/ty_check/callable.rs",
                "src/analysis/semantic/instance/const_ref.rs",
            ]
        ),
        Vec::<String>::new()
    );
    // A body can also be taken from a selected implementation directly; the
    // checker does so for every candidate of a body chosen later, and the
    // callee key and the checker's call for a recorded choice.
    assert_eq!(
        occurrences_outside(
            "method_instance_from_resolved(",
            &[
                "src/analysis/ty/trait_def.rs",
                CHECKER,
                "src/analysis/ty/ty_check/callable.rs",
                "src/analysis/semantic/instance/const_ref.rs",
            ]
        ),
        Vec::<String>::new()
    );
}

#[test]
fn frames_are_read_through_one_normalized_lookup() {
    // Frames are keyed by `SlotKey`, whose only constructor normalizes the
    // slot, so that every spelling of one slot (concrete, through an
    // associated type, `Self`) writes and reads one entry. Its field is
    // private to its module; this checks the one place that makes keys.
    assert_eq!(
        occurrences_outside("SlotKey::new(", &[CHECKER]),
        Vec::<String>::new()
    );
    let checker = text_of(CHECKER);
    assert_eq!(code(&checker).matches("SlotKey::new(").count(), 1);
    assert_eq!(code(&checker).matches(".lookup(").count(), 1);
}

#[test]
fn written_constant_arguments_are_scanned_in_one_place() {
    // Lowering evaluates a constant written in a path with the ordinary
    // implementation, so D5 reads the written syntax: one scan, run for
    // every expression and every pattern from the dispatch each goes
    // through, with written types going through `lower_ty`.
    for reader in [
        "hir_path_const_slot_reads(",
        "hir_generic_args_const_slot_reads(",
    ] {
        assert_eq!(
            occurrences_outside(reader, &[CHECKER, "src/analysis/ty/ty_lower.rs"]),
            Vec::<String>::new()
        );
        assert_eq!(code(&text_of(CHECKER)).matches(reader).count(), 1);
    }
    for (path, dispatch) in [
        (
            "src/analysis/ty/ty_check/expr.rs",
            "fn check_expr_with_result_context(",
        ),
        ("src/analysis/ty/ty_check/pat.rs", "fn check_pat("),
    ] {
        let text = text_of(path);
        let function = functions(&text)
            .into_iter()
            .find(|function| function.contains(dispatch))
            .expect("the dispatch exists");
        assert!(
            code(&function).contains("refuse_written_const_args("),
            "{path}: the dispatch does not scan written arguments"
        );
    }
    assert_eq!(
        occurrences_outside(
            "refuse_written_const_args(",
            &[
                CHECKER,
                "src/analysis/ty/ty_check/expr.rs",
                "src/analysis/ty/ty_check/pat.rs",
            ]
        ),
        Vec::<String>::new()
    );
}

#[test]
fn associated_outputs_are_read_through_one_selection() {
    // D16: a slot's associated types, rows and spaces are the same whichever
    // implementation answers it; readers select through
    // `output_implementation`, which also answers a slot that only named
    // implementations give, not through ordinary selection alone.
    for path in [
        "src/analysis/ty/effects/rows.rs",
        "src/analysis/ty/result_space.rs",
    ] {
        let text = text_of(path);
        assert!(
            !code(&text).contains("resolve_trait_impl_instance("),
            "{path}: an output reader selects the ordinary implementation alone"
        );
        assert!(code(&text).contains("output_implementation("));
    }
    assert!(
        code(&text_of("src/analysis/ty/normalize.rs")).contains("named_output_implementation(")
    );
}

#[test]
fn deliveries_reach_a_body_through_one_mapping() {
    // A call's deliveries are read only where the body that runs is mapped
    // (`trait_choice::body_deliveries`): the checker, the condition and
    // type-reach checks, and the callee key. Another reader would be a
    // second answer to "what does this body receive".
    assert_eq!(
        occurrences_outside(
            ".delivered()",
            &[
                CHECKER,
                "src/analysis/ty/ty_check/const_requirements.rs",
                "src/analysis/semantic/instance/const_ref.rs",
            ]
        ),
        Vec::<String>::new()
    );
    assert_eq!(
        occurrences_outside("fn body_deliveries", &[REPRESENTATION]),
        Vec::<String>::new()
    );
    // Each function that reads deliveries maps them to a body through the
    // one mapping for its kind: `body_deliveries` for a call,
    // `const_body_deliveries` (or `to_body`, built on it) for a constant.
    // The checker also records them, which needs no mapping.
    assert_eq!(
        occurrences_outside("fn const_body_deliveries", &[REPRESENTATION]),
        Vec::<String>::new()
    );
    for (path, text) in sources() {
        if path == CHECKER {
            continue;
        }
        for function in functions(&text) {
            let function = code(&function);
            if function.contains(".delivered()") {
                assert!(
                    function.contains("body_deliveries(") || function.contains(".to_body("),
                    "{path}: a function reads deliveries without mapping them to a body:\n{}",
                    function.lines().next().unwrap_or_default()
                );
            }
        }
    }
}

/// The source of the file at `path`.
fn text_of(path: &str) -> String {
    sources()
        .into_iter()
        .find(|(file, _)| file == path)
        .unwrap_or_else(|| panic!("{path} exists"))
        .1
}

/// `text` without its `//` comments, so a name in a comment does not count.
fn code(text: &str) -> String {
    text.lines()
        .map(|line| line.split_once("//").map_or(line, |(code, _)| code))
        .collect::<Vec<_>>()
        .join("\n")
}

/// `text` cut at each line that starts a function, so a check can hold per
/// function rather than per file.
fn functions(text: &str) -> Vec<String> {
    let mut out = vec![String::new()];
    for line in text.lines() {
        let trimmed = line.trim_start();
        let starts = trimmed.starts_with("fn ")
            || trimmed.starts_with("pub fn ")
            || (trimmed.starts_with("pub(") && trimmed.contains(") fn "));
        if starts {
            out.push(String::new());
        }
        let last = out.last_mut().expect("one chunk at least");
        last.push_str(line);
        last.push('\n');
    }
    out
}
