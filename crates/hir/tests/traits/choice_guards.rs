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
    // seed), for its constants' recursion check, and for a trait signature
    // compared under it (decision (e)).
    assert_eq!(
        occurrences_outside(
            "KnownChoice::own(",
            &[
                REPRESENTATION,
                CHECKER,
                "src/diagnosable.rs",
                "src/analysis/ty/method_cmp.rs",
            ]
        ),
        Vec::<String>::new()
    );
}

#[test]
fn evidence_goals_are_read_only_where_choices_are_decided_or_verified() {
    // Reading `Evidence<G>`'s goal is how a consumer would start re-deriving a
    // choice from an evidence value's type. `written_frames` reads it only to
    // know which slot a `with (e)` opens around a written constant, so that
    // constant reads that slot unsupplied; it decides no choice.
    assert_eq!(
        occurrences_outside(
            "evidence_goal(",
            &[
                CHECKER,
                "src/analysis/ty/ty_check/written_frames.rs",
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
fn bodies_are_chosen_through_one_accessor() {
    // A method or constant body is reached from a choice only through
    // `trait_def::chosen_method`, `chosen_constant` and the candidate lists,
    // which give a body with the choices it receives: the template functions
    // are private to `trait_def`, and `KnownChoice` applies its
    // implementation only for its outputs. Building an implementation
    // instance by hand stays in the two modules that own them.
    for raw in [
        "complete_selected_impl(",
        "instantiate_selected_impl(",
        ".method_instance(",
    ] {
        assert_eq!(
            occurrences_outside(raw, &["src/analysis/ty/trait_def.rs", REPRESENTATION]),
            Vec::<String>::new(),
            "{raw}"
        );
    }
}

#[test]
fn no_frame_shortcuts_go_through_has_frames() {
    // Every shortcut that skips choice work where nothing is opened asks
    // `has_frames`, so the shortcuts move together when a module-level frame
    // becomes the base of the frame list (MODULE_WITH_NOTE.md). The one raw
    // test is in its definition.
    for raw in [
        "frame.is_none()",
        ".current().is_none()",
        ".current().is_some()",
    ] {
        assert_eq!(occurrences_outside(raw, &[]), Vec::<String>::new(), "{raw}");
    }
    assert_eq!(
        occurrences_outside("frame.is_some()", &[CHECKER]),
        Vec::<String>::new()
    );
    let checker = code(&text_of(CHECKER));
    assert_eq!(checker.matches("frame.is_some()").count(), 1);
    assert!(
        checker.contains("fn has_frames(frame: Option<FrameId>) -> bool {\n    frame.is_some()")
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
    let checker = code(&text_of(CHECKER));
    assert_eq!(checker.matches("SlotKey::new(").count(), 1);
    // A frame's nodes are private to the `frames` module, so only its
    // methods read them; of those, only `lookup` gives a choice, and only
    // `frame_choice` calls it.
    let frames = checker
        .split("mod frames {")
        .nth(1)
        .expect("frames are kept in their own module");
    let frames = &frames[..frames
        .find("\npub(super) use frames::")
        .unwrap_or(frames.len())];
    let giving = functions(frames)
        .into_iter()
        .filter(|function| {
            // The signature, up to the body: what the method returns.
            let signature = &function[..function.find('{').unwrap_or(function.len())];
            signature.contains("fn ")
                && signature
                    .split("->")
                    .nth(1)
                    .is_some_and(|returned| returned.contains("Choice"))
        })
        .collect::<Vec<_>>();
    assert_eq!(
        giving.len(),
        1,
        "a frame method other than lookup gives a choice"
    );
    assert!(
        giving[0]
            .lines()
            .next()
            .unwrap_or_default()
            .contains("fn lookup(")
    );
    assert_eq!(checker.matches(".lookup(").count(), 1);
    assert!(
        !checker
            .split("mod frames {")
            .next()
            .unwrap_or_default()
            .contains(".nodes")
    );
}

#[test]
fn written_constant_arguments_are_scanned_in_one_place() {
    // Lowering evaluates a constant written in a path with the ordinary
    // implementation, so D5 reads the written syntax: one scan, run for
    // every expression and every pattern from the dispatch each goes
    // through, for each `with` key where it is lowered (under the header's
    // earlier openings), with written types going through `lower_ty`.
    let checker = code(&text_of(CHECKER));
    for reader in [
        "hir_path_const_slot_reads(",
        "hir_generic_args_const_slot_reads(",
    ] {
        assert_eq!(
            occurrences_outside(reader, &[CHECKER, "src/analysis/ty/ty_lower.rs"]),
            Vec::<String>::new()
        );
        for function in functions(&checker) {
            if function.contains(reader) {
                let head = function.lines().next().unwrap_or_default();
                assert!(
                    head.contains("fn refuse_written_const_args")
                        || head.contains("fn refuse_written_with_key"),
                    "written arguments are read outside the scan: {head}"
                );
            }
        }
    }
    // The reader skips no written syntax: its matches over types and
    // generic arguments have no wildcard and no arm that reads nothing,
    // except a type with no parts (`!`).
    let lower = code(&text_of("src/analysis/ty/ty_lower.rs"));
    for function in functions(&lower) {
        let head = function.lines().next().unwrap_or_default();
        if [
            "fn const_ty_reads",
            "fn const_path_reads",
            "fn const_arg_reads",
        ]
        .iter()
        .any(|name| head.contains(name))
        {
            let empty_arms = function
                .lines()
                .filter(|line| line.contains("=> {}") || line.trim_start().starts_with("_ =>"))
                .collect::<Vec<_>>();
            assert!(
                empty_arms
                    .iter()
                    .all(|line| line.contains("HirTyKind::Never => {}")),
                "{head}: an arm reads nothing: {empty_arms:?}"
            );
        }
    }
    for (path, dispatch, scan) in [
        (
            "src/analysis/ty/ty_check/expr.rs",
            "fn check_expr_with_result_context(",
            "refuse_written_const_args(",
        ),
        (
            "src/analysis/ty/ty_check/pat.rs",
            "fn check_pat(",
            "refuse_written_const_args(",
        ),
        (
            "src/analysis/ty/ty_check/expr.rs",
            "fn check_with(",
            "refuse_written_with_key(",
        ),
    ] {
        let text = text_of(path);
        let function = functions(&text)
            .into_iter()
            .find(|function| function.contains(dispatch))
            .expect("the dispatch exists");
        assert!(
            code(&function).contains(scan),
            "{path}: {dispatch} does not scan written arguments"
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
        let text = code(&text_of(path));
        assert!(
            !text.contains("resolve_trait_impl_instance("),
            "{path}: an output reader selects the ordinary implementation alone"
        );
        assert!(text.contains("output_implementation("));
        // One ordinary selection is allowed in rows: while an instance is
        // still inferred, it only decides whether an empty row expands, and
        // D16 makes a named implementation's row the same.
        let selections = text.matches("select_impl(").count();
        assert_eq!(
            selections,
            usize::from(path.ends_with("rows.rs")),
            "{path}: an output reader selects the ordinary implementation alone"
        );
    }
    assert!(
        code(&text_of("src/analysis/ty/normalize.rs")).contains("named_output_implementation(")
    );
}

#[test]
fn ordinary_selection_names_its_reason() {
    // Row 1: the ordinary selection and method lookup take a reason that
    // only `trait_choice` makes (`OrdinarySelection`), and each reason is
    // used only by the sites the closure list argues it for (`CLOSURE.md`).
    let const_ref = "src/analysis/semantic/instance/const_ref.rs";
    let trait_def = "src/analysis/ty/trait_def.rs";
    let allowed: &[(&str, &[(&str, &str)])] = &[
        (
            "no_recorded_choice",
            &[
                (trait_def, "fn chosen_method"),
                (trait_def, "fn chosen_constant"),
                (CHECKER, "fn ordinary_choice"),
            ],
        ),
        ("outputs_by_d16", &[(trait_def, "fn output_implementation")]),
        (
            "outermost_tier_only",
            &[
                ("src/analysis/ty/provider.rs", "fn state_index_lanes"),
                (trait_def, "fn assoc_const_body_template_for_trait_inst"),
                (trait_def, "fn trait_inst_selects_concrete_impl"),
            ],
        ),
        (
            "decides_nothing",
            &[
                (CHECKER, "fn add_premise_if_needed"),
                (CHECKER, "fn ordinary_proof_reads"),
                (const_ref, "fn semantic_callee_key_with_assumptions"),
            ],
        ),
        (
            "outermost_tier_type",
            &[(
                "src/analysis/ty/const_ty.rs",
                "fn const_ty_from_trait_const",
            )],
        ),
        (
            "for_tests",
            &[
                (trait_def, "fn probe"),
                ("src/analysis/ty/trait_resolution/mod.rs", "fn unsupported"),
            ],
        ),
    ];
    for (path, text) in sources() {
        if path == REPRESENTATION {
            continue;
        }
        let text = code(&text);
        for (at, _) in text.match_indices("OrdinarySelection::") {
            let rest = &text[at + "OrdinarySelection::".len()..];
            let name = &rest[..rest.find('(').unwrap_or(0)];
            assert!(
                allowed.iter().any(|(reason, _)| *reason == name),
                "{path}: an unknown reason `{name}`"
            );
        }
        for function in functions(&text) {
            let head = function.lines().next().unwrap_or_default();
            for (reason, sites) in allowed {
                if function.contains(&format!("OrdinarySelection::{reason}(")) {
                    assert!(
                        sites
                            .iter()
                            .any(|(file, name)| path == *file && head.contains(name)),
                        "{path}: `{reason}` is used in {head}"
                    );
                }
            }
        }
    }
    // The two entry points take a reason; there is no way around it.
    let trait_def_text = code(&text_of(trait_def));
    for entry in [
        "fn resolve_trait_impl_instance<",
        "fn resolve_trait_method_instance<",
    ] {
        let function = functions(&trait_def_text)
            .into_iter()
            .find(|function| function.contains(entry))
            .unwrap_or_else(|| panic!("{entry} exists"));
        let signature = &function[..function.find('{').unwrap_or(function.len())];
        assert!(
            signature.contains("OrdinarySelection"),
            "{entry} does not take a reason"
        );
    }
}

#[test]
fn written_constants_are_refused_where_they_are_read() {
    // D5 at the read: a constant written where a frame around it opens a
    // slot it reads (`written_frames`) never reads the ordinary
    // implementation. Each place a written constant is read asks the one
    // query: lowering a lone trait-constant path, evaluating a constant
    // body, and the check of an anonymous constant body (seeded with those
    // slots unsupplied). A refused constant is compared like an invalid type
    // and reported once by the body around it.
    for (path, function, reads) in [
        (
            "src/analysis/ty/ty_lower.rs",
            "fn lower_const_body_path",
            "written_frames(",
        ),
        (
            "src/analysis/ty/const_ty.rs",
            "fn evaluate_const_ty",
            "refuses_read(",
        ),
        (
            "src/analysis/ty/ty_check/env.rs",
            "fn new",
            "seed_written_frames(",
        ),
        (
            "src/analysis/ty/unify.rs",
            "fn unify_ty",
            "refused_at_read(",
        ),
        (
            "src/analysis/ty/ty_check/mod.rs",
            "fn checked_body",
            "unreported_choice_dependent_consts(",
        ),
    ] {
        let text = code(&text_of(path));
        let found = functions(&text)
            .into_iter()
            .filter(|chunk| {
                chunk
                    .lines()
                    .next()
                    .is_some_and(|head| head.contains(function))
            })
            .any(|chunk| chunk.contains(reads));
        assert!(found, "{path}: {function} does not read {reads}");
    }
    // One predicate decides a refused constant, for the unifier, the
    // reporter and the reads of a type's slots.
    assert_eq!(
        occurrences_outside(
            "fn refused_at_read",
            &["src/analysis/ty/ty_check/written_frames.rs"]
        ),
        Vec::<String>::new()
    );
}

#[test]
fn signatures_are_read_through_one_list() {
    // D5: the types a callee declares (arguments, result, effect keys) are
    // listed once (`declared_tys`); the call refusal, the read of every
    // registered call when the body is finished, and type reach take them
    // only from it.
    let checker = code(&text_of(CHECKER));
    let functions = functions(&checker);
    let declaring: Vec<&String> = functions
        .iter()
        .filter(|function| {
            function.contains(".arg_tys(") || function.contains("effective_effect_requirements(")
        })
        .collect();
    for function in &declaring {
        let head = function.lines().next().unwrap_or_default();
        assert!(
            head.contains("fn declared_tys") || head.contains("fn call_slot_reads"),
            "a signature is read outside the one list: {head}"
        );
    }
    for (reader, through) in [
        ("fn refuse_choice_dependent_signature", "call_slot_reads("),
        ("fn decided_call", "call_slot_reads("),
        ("fn own_type_reads", "declared_slot_reads("),
    ] {
        let function = functions
            .iter()
            .find(|function| function.contains(reader))
            .unwrap_or_else(|| panic!("{reader} exists"));
        assert!(
            function.contains(through),
            "{reader} does not read {through}"
        );
    }
}

#[test]
fn calls_are_decided_where_they_are_written() {
    // L1: every call form (an expression call, a value call, a `for` loop's
    // protocol calls, a factory call denoted where its value is used) is
    // decided by one recorder, under the frame recorded where it is written.
    let checker = code(&text_of(CHECKER));
    let checked = functions(&checker);
    for function in &checked {
        let head = function.lines().next().unwrap_or_default();
        if function.contains(".call_decisions(") {
            assert!(
                head.contains("fn decided_call"),
                "a call is decided outside the recorder: {head}"
            );
        }
    }
    let function = |name: &str| {
        checked
            .iter()
            .find(|function| function.lines().next().unwrap_or_default().contains(name))
            .unwrap_or_else(|| panic!("{name} exists"))
    };
    let recorder = function("fn record_choice_decisions");
    assert_eq!(recorder.matches(".decided_call(").count(), 2);
    assert!(recorder.contains("for_loop_plans"));
    let factory = function("fn closed_call");
    assert!(factory.contains(".decided_call(self.env.use_frames["));
    assert!(!factory.contains("choice_frames.current()"));
    // One classification of the calls whose callee body receives decisions:
    // the owed checks' `calls_directly` delegates to the recorder's.
    assert_eq!(
        occurrences_outside("fn is_function_value", &[CHECKER]),
        Vec::<String>::new()
    );
    let owed = code(&text_of("src/analysis/ty/ty_check/const_requirements.rs"));
    let owed_functions = functions(&owed);
    let wrapper = owed_functions
        .iter()
        .find(|function| {
            function
                .lines()
                .next()
                .unwrap_or_default()
                .contains("fn calls_directly")
        })
        .expect("the owed checks classify calls");
    assert!(wrapper.contains("super::choice::calls_directly("));
}

#[test]
fn type_reach_reads_one_list() {
    // D5: the types a body writes or names are listed once
    // (`body_type_reads`), with each call's declared types as the call
    // refusal reads them; function and constant bodies take type reach only
    // from it, and a constant use checks the body it delivers into.
    let checker = code(&text_of(CHECKER));
    let checked = functions(&checker);
    let function = |name: &str| {
        checked
            .iter()
            .find(|function| function.lines().next().unwrap_or_default().contains(name))
            .unwrap_or_else(|| panic!("{name} exists"))
    };
    // Each expression call and each `for` loop protocol call.
    assert_eq!(
        function("fn body_type_reads")
            .matches("call_slot_reads(")
            .count(),
        2
    );
    assert!(function("fn body_type_reads").contains("written_body_uses("));
    assert!(function("fn own_type_reads").contains("body_type_reads("));
    assert!(function("fn const_type_reads").contains("body_type_reads("));
    assert!(function("fn body_decisions").contains("const_type_reads("));
    for function in &checked {
        let head = function.lines().next().unwrap_or_default();
        if function.contains("type_slot_reads(db, typed.expr_ty(") {
            assert!(
                head.contains("fn body_type_reads"),
                "a body's types are read outside the one list: {head}"
            );
        }
    }
    // The written-syntax scan reads the constants written inside a compound
    // constant through the same layout.
    let lower = code(&text_of("src/analysis/ty/ty_lower.rs"));
    let lowered = functions(&lower);
    let scan = lowered
        .iter()
        .find(|function| {
            function
                .lines()
                .next()
                .unwrap_or_default()
                .contains("fn const_body_reads")
        })
        .expect("the scan reads compound constants");
    assert!(scan.contains("written_bodies(") && scan.contains("written_body_uses("));
}

#[test]
fn d13_is_checked_from_every_route() {
    // D13: a goal left to ordinary selection under an opening is followed
    // through one walk of the ordinary proof (`ordinary_proof_reads`), from
    // a call, a constant, a function value and `prove`, in the frame's
    // premises, and owed to callers when the slot it reads is the
    // function's own.
    let checker = code(&text_of(CHECKER));
    let checked = functions(&checker);
    let function = |name: &str| {
        checked
            .iter()
            .find(|function| function.lines().next().unwrap_or_default().contains(name))
            .unwrap_or_else(|| panic!("{name} exists"))
    };
    assert!(function("fn decide_prove").contains(".reads_opened_condition("));
    let walk = function("fn walk_conditions");
    assert!(walk.contains("ordinary_proof_reads(") && walk.contains(".opened("));
    assert!(function("fn owed_opened_conditions").contains("ordinary_proof_reads("));
    let owed = code(&text_of("src/analysis/ty/ty_check/const_requirements.rs"));
    let owed_checks = functions(&owed)
        .into_iter()
        .find(|function| {
            function
                .lines()
                .next()
                .unwrap_or_default()
                .contains("fn owed_checks<")
        })
        .expect("functions owe checks");
    assert!(owed_checks.contains("owed_opened_conditions("));
    // The walk does not count a proof it cannot follow as reading nothing.
    let proof = function("fn ordinary_proof_reads");
    assert!(proof.contains("_ => return true"));
}

#[test]
fn premises_compare_calls_as_written() {
    // D6: a callee's condition and the caller's identical premise are one
    // predicate. A recorded decision that forwards a bound names its owner's
    // own bound, which substitution does not rename, so the predicate key
    // compares a call as written (`Callable::as_written`).
    let owed = code(&text_of("src/analysis/ty/ty_check/const_requirements.rs"));
    let key = functions(&owed)
        .into_iter()
        .find(|function| {
            function
                .lines()
                .next()
                .unwrap_or_default()
                .contains("fn key(&self")
        })
        .expect("predicates have keys");
    assert!(key.contains(".as_written()"));
    assert!(!key.contains("callable.clone()"));
}

#[test]
fn evidence_is_admitted_after_inference() {
    // D2: a `with` operand taken as an effect value is read again once its
    // type is known and refused if it is evidence; a destructured part of an
    // aggregate is refused as a field is; a value of a type parameter that a
    // body opens or passes on owes callers that it is not evidence.
    let checker = code(&text_of(CHECKER));
    let checked = functions(&checker);
    let function = |name: &str| {
        checked
            .iter()
            .find(|function| function.lines().next().unwrap_or_default().contains(name))
            .unwrap_or_else(|| panic!("{name} exists"))
    };
    assert!(function("fn open_evidence_binding").contains("effect_with_operands.push("));
    assert!(function("fn record_choice_decisions").contains("effect_with_operands"));
    assert!(function("fn denote_evidence").contains("self.destructured("));
    let owed = code(&text_of("src/analysis/ty/ty_check/const_requirements.rs"));
    let owed_checks = functions(&owed)
        .into_iter()
        .find(|function| {
            function
                .lines()
                .next()
                .unwrap_or_default()
                .contains("fn owed_checks<")
        })
        .expect("functions owe checks");
    assert!(owed_checks.contains("effect_with_operands()"));
    assert!(owed_checks.contains("OwedCheck::NoEvidence"));
}

#[test]
fn deferred_calls_resolve_where_written() {
    // L3: a method call picked after the body is checked is resolved under
    // the frame and premises recorded where it is written.
    let checker_mod = code(&text_of("src/analysis/ty/ty_check/mod.rs"));
    assert!(checker_mod.contains("self.env.choice_frames.restore(pending.frame);"));
    assert!(checker_mod.contains("self.env.restore_assumptions(pending.premises);"));
    let expr = code(&text_of("src/analysis/ty/ty_check/expr.rs"));
    assert!(expr.contains("frame: self.env.choice_frames.current(),\n            premises: self.env.assumptions(),\n        });")
        || expr.contains("premises: self.env.assumptions(),"));
}

#[test]
fn constant_reads_include_their_body() {
    // D5: a constant read depends on the slots its body reads, not only its
    // own; the one list (`const_use_reads`) falls back to the trait's default
    // body when the body that runs is chosen later, and the read-time
    // refusal and the written-syntax scan take their slots from it.
    let checker = code(&text_of(CHECKER));
    let reads = functions(&checker)
        .into_iter()
        .find(|function| {
            function
                .lines()
                .next()
                .unwrap_or_default()
                .contains("fn const_use_reads")
        })
        .expect("constant uses have reads");
    assert!(reads.contains("trait_default_body(db)"));
    let lower = code(&text_of("src/analysis/ty/ty_lower.rs"));
    let lowered = functions(&lower);
    let find = |name: &str| {
        lowered
            .iter()
            .find(|function| function.lines().next().unwrap_or_default().contains(name))
            .unwrap_or_else(|| panic!("{name} exists"))
            .clone()
    };
    assert!(find("fn lower_const_body_path").contains("opens_constant_read("));
    assert!(find("fn const_body_reads").contains("constant_read_slots("));
}

#[test]
fn named_conditions_read_the_declaring_environment() {
    // RD130, decision (b): a named implementation's `where` conditions read
    // the environment where it is declared, in every spelling: no own-choice
    // seed and no declaration frame reaches them, or a condition would prove
    // itself.
    let env = code(&text_of("src/analysis/ty/ty_check/env.rs"));
    assert!(env.contains("&& !owner.is_const_predicate(db)"));
    let frames = code(&text_of("src/analysis/ty/ty_check/written_frames.rs"));
    let declaration = functions(&frames)
        .into_iter()
        .find(|function| {
            function
                .lines()
                .next()
                .unwrap_or_default()
                .contains("fn declaration_frames")
        })
        .expect("named declarations have frames");
    assert!(declaration.contains(".const_predicates(db)"));
}

#[test]
fn choice_walks_end_at_one_limit() {
    // D18: a walk that follows choices through implementations ends where the
    // base's instance limits end, through one check; no walk keeps a limit of
    // its own (a fixed 256 refused lawful programs twice).
    let owed = "src/analysis/ty/ty_check/const_requirements.rs";
    assert_eq!(
        occurrences_outside("choice_walk_step_within_limits(", &[CHECKER, owed]),
        Vec::<String>::new()
    );
    assert_eq!(
        code(&text_of(CHECKER))
            .matches("instance_args_within_limits(")
            .count(),
        1
    );
    for path in [CHECKER, owed] {
        let code = code(&text_of(path));
        // No comparison with a number of two or more digits, on either side,
        // and no numeric constant but the backstop.
        let bytes = code.as_bytes();
        for (at, op) in code.match_indices(['<', '>']) {
            let after = code[at + op.len()..].trim_start_matches('=').trim_start();
            let before = code[..at].trim_end();
            let digits_after = after.chars().take_while(char::is_ascii_digit).count();
            let digits_before = before
                .chars()
                .rev()
                .take_while(char::is_ascii_digit)
                .count();
            let is_compare = bytes.get(at + 1) != Some(&b'>') && (at == 0 || bytes[at - 1] != b'-');
            assert!(
                !is_compare || (digits_after < 2 && digits_before < 2),
                "{path}: a walk keeps a numeric limit of its own near {:?}",
                &code[at.saturating_sub(30)..(at + 30).min(code.len())]
            );
        }
        let limits = code
            .lines()
            .filter(|line| {
                let line = line.trim_start();
                (line.starts_with("const ") || line.starts_with("static "))
                    && line.contains(" = ")
                    && line
                        .split(" = ")
                        .nth(1)
                        .is_some_and(|value| value.starts_with(|c: char| c.is_ascii_digit()))
            })
            .count();
        assert_eq!(
            limits,
            usize::from(path == CHECKER),
            "{path}: a walk keeps a limit of its own"
        );
    }
    // Both walks measure a step the same way: its depth below where the walk
    // began (a body's below its call, a condition's below its goal). A body
    // becomes an instance and meets the base's instance limits; a condition
    // of a proof the base has found meets only the backstop.
    for (path, measure) in [
        (
            CHECKER,
            "choice_walk_step_within_limits(db, WalkStep::Proof, depth)",
        ),
        (
            owed,
            "choice_walk_step_within_limits(db, super::choice::WalkStep::Instance(body_args), walk.path.len(),)",
        ),
    ] {
        let code = code(&text_of(path)).split_whitespace().collect::<String>();
        let measure = measure.split_whitespace().collect::<String>();
        assert!(
            code.contains(&measure),
            "{path}: the walk does not measure depth"
        );
    }
}

#[test]
fn deliveries_reach_a_body_through_one_mapping() {
    // A call's deliveries are read only where the body that runs is mapped
    // (`trait_choice::body_deliveries`): the checker, the condition and
    // type-reach checks, and the callee key; a constant use's in the
    // accessor (`trait_def::chosen_constant`). Another reader would be a
    // second answer to "what does this body receive".
    assert_eq!(
        occurrences_outside(
            ".delivered()",
            &[
                CHECKER,
                "src/analysis/ty/ty_check/const_requirements.rs",
                "src/analysis/semantic/instance/const_ref.rs",
                "src/analysis/ty/trait_def.rs",
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
        // A function starts at any `fn` item, whatever its visibility and
        // qualifiers (`const`, `unsafe`, `async`, `extern`).
        let mut words = line.split_whitespace();
        let starts = words.by_ref().take_while(|word| *word != "fn").all(|word| {
            word.starts_with("pub")
                || ["const", "unsafe", "async", "extern", "\"C\""].contains(&word)
        }) && line.trim_start().contains("fn ")
            && !line.trim_start().starts_with("//");
        if starts {
            out.push(String::new());
        }
        let last = out.last_mut().expect("one chunk at least");
        last.push_str(line);
        last.push('\n');
    }
    out
}
