//! A normalization limit makes one part of a question unknown; it does not
//! end the question. A part that does not hold still decides, wherever it is
//! ("no and unknown is no"), so a loop over candidates, goals or parts must
//! not stop at the first limit: it uses `candidates::all_of`, `all_hold` or
//! `decide`, which ask every part.
//!
//! This check reads the decision code (`analysis/ty`, `analysis/name_resolution`
//! and `core/semantic/mod.rs`) for the shapes that stop at the first limit:
//! - inside a loop, a match arm on a limit (`Err(limit)`, `Unknown(limit)`,
//!   `Limit(limit)`) whose body returns or breaks;
//! - inside a loop, `?` on a call that can reach a limit, or in a function that
//!   returns one;
//! - parts collected into a `Result`, which keeps the first error.
//!
//! The sites that may stop at the first limit are listed in [`ALLOWED`], each
//! with the reason. The list can only shrink: an entry that no longer matches
//! a site fails the check too.

use super::candidate_guard::{production, relative, sources};

/// Calls whose result can be a normalization limit.
const FALLIBLE: &[&str] = &[
    "normalize_ty(",
    "normalize_ty_in_solver(",
    "is_goal_satisfiable(",
    "is_goal_query_satisfiable(",
    ".normalize(",
    "tree_size_within(",
    "all_of(",
    "impl_holds(",
];

/// The files the check reads.
const SCOPE: &[&str] = &[
    "analysis/ty/",
    "analysis/name_resolution/",
    "core/semantic/mod.rs",
];

/// Sites that stop at the first limit, by file and the text of the line, and
/// why that is right there.
const ALLOWED: &[(&str, &str, &str)] = &[
    (
        "analysis/name_resolution/path_resolver.rs",
        "candidates::Holds::Unknown(limit) => return Err(limit),",
        "inherent constants of one name on unifying self types are a definition error, so at most one impl is the answer",
    ),
    (
        "analysis/ty/method_cmp.rs",
        "ConstraintEntailment::Limit(limit) => {",
        "compare_constraints: an unproved bound and a limit are both errors at the impl method; only the message differs",
    ),
    (
        "analysis/ty/mod.rs",
        "let missing: Result<Vec<_>, _> = [",
        "ABI field checks: a missing trait and a limit are both errors at the field; only the message differs",
    ),
    (
        "analysis/ty/msg_selector.rs",
        "Err(reached) => {",
        "message selector checks: a missing ABI trait and a limit are both errors; only the message differs",
    ),
    (
        "analysis/ty/normalize.rs",
        ".transpose()?;",
        "layout roots are a value, not a decision: every root is needed, so a limit in any makes the whole unknown",
    ),
    (
        "analysis/ty/normalize.rs",
        "collect(db, *arg, scope, assumptions, visiting, uses)?;",
        "layout roots are a value, not a decision: every root is needed, so a limit in any makes the whole unknown",
    ),
    (
        "analysis/ty/normalize.rs",
        "let (size, child_depth) = measure(db, child, limit, depth_limit - 1, memo)?;",
        "measuring a type's size: passing the bound is the answer",
    ),
    (
        "analysis/ty/normalize.rs",
        ".ok_or(NormalizationLimit::Work)?;",
        "measuring a type's size: passing the bound is the answer",
    ),
    (
        "analysis/ty/method_table.rs",
        "if match_probe_key(table, key_ty, ty, scope, query.assumptions)? {",
        "MethodBucket::probe: with two known matches the call is ambiguous whatever the limit gives; only the message differs",
    ),
    (
        "analysis/ty/trait_def.rs",
        ".collect::<Result<Vec<_>, _>>()?;",
        "normalizing a trait instance's arguments is a value, not a decision: every argument is needed",
    ),
    (
        "core/semantic/mod.rs",
        ".map_err(ImplTraitLowerError::NormalizationLimit)?;",
        "impl overlap: a conflict and a limit are both errors at the impl; only the message differs",
    ),
];

/// The code of a line, without a trailing comment.
fn code(line: &str) -> &str {
    line.split("//").next().unwrap_or_default().trim()
}

/// Where the first limit named in a match pattern in `text` ends.
fn limit_pattern(text: &str) -> Option<usize> {
    let mut first: Option<(usize, usize)> = None;
    for head in ["Err(", "Unknown(", "Limit(", "NormalizationLimit("] {
        for name in ["limit)", "reached)", "l)"] {
            let pattern = format!("{head}{name}");
            for (at, _) in text.match_indices(&pattern) {
                let starts_word = text[..at]
                    .chars()
                    .last()
                    .is_none_or(|ch| !ch.is_alphanumeric() && ch != '_');
                if starts_word && first.is_none_or(|(start, _)| at < start) {
                    first = Some((at, at + pattern.len()));
                }
            }
        }
    }
    first.map(|(_, end)| end)
}

/// Whether `text` holds the word `word`.
fn has_word(text: &str, word: &str) -> bool {
    text.match_indices(word).any(|(at, _)| {
        let before = text[..at].chars().last();
        let after = text[at + word.len()..].chars().next();
        before.is_none_or(|ch| !ch.is_alphanumeric() && ch != '_')
            && after.is_none_or(|ch| !ch.is_alphanumeric() && ch != '_')
    })
}

/// The body of the match arm that starts at `rest` (after the pattern).
fn arm_body(rest: &str) -> Option<&str> {
    let rest = rest.trim_start().strip_prefix("=>")?.trim_start();
    if rest.starts_with('{') {
        let mut depth = 0;
        for (idx, ch) in rest.char_indices() {
            depth += i32::from(ch == '{') - i32::from(ch == '}');
            if depth == 0 {
                return Some(&rest[..=idx]);
            }
        }
        Some(rest)
    } else {
        Some(rest.split(',').next().unwrap_or_default())
    }
}

/// The sites of `text` that stop at the first limit: (line number, line).
fn first_limit_sites(text: &str) -> Vec<(usize, String)> {
    let lines: Vec<&str> = text.lines().collect();
    let mut sites = Vec::new();
    let mut loops: Vec<i32> = Vec::new();
    let mut depth = 0;
    let mut pending_loop = false;
    let mut signature = String::new();
    let mut in_signature = false;
    for (idx, line) in lines.iter().enumerate() {
        let line = code(line);
        let starts_fn = [
            "fn ",
            "pub fn ",
            "pub(crate) fn ",
            "pub(super) fn ",
            "pub(in ",
        ]
        .iter()
        .any(|start| line.starts_with(start));
        if starts_fn {
            signature.clear();
            in_signature = true;
        }
        if in_signature {
            signature.push_str(line);
            if line.ends_with('{') || line.ends_with(';') {
                in_signature = false;
            }
        }
        if line.starts_with("for ")
            || line.starts_with("while ")
            || line.starts_with("loop ")
            || line.contains(" for ") && line.contains(" in ")
        {
            pending_loop = true;
        }
        let opens_loop = pending_loop && line.ends_with('{');
        if opens_loop {
            pending_loop = false;
        }
        if !loops.is_empty() {
            if let Some(end) = limit_pattern(line) {
                let rest: String = std::iter::once(&line[end..])
                    .chain(lines[idx + 1..].iter().take(30).map(|line| code(line)))
                    .collect::<Vec<_>>()
                    .join(" ");
                if arm_body(&rest)
                    .is_some_and(|body| has_word(body, "return") || has_word(body, "break"))
                {
                    sites.push((idx + 1, line.to_string()));
                }
            }
            let returns_limit = signature
                .rsplit("->")
                .next()
                .is_some_and(|ret| ret.contains("NormalizationLimit"));
            if line.contains(")?")
                && (FALLIBLE.iter().any(|call| line.contains(call))
                    || line.contains("limit")
                    || line.contains("Limit")
                    || returns_limit)
            {
                sites.push((idx + 1, line.to_string()));
            }
        }
        let names_limit = |context: &str| {
            limit_pattern(context).is_some() || FALLIBLE.iter().any(|call| context.contains(call))
        };
        if line.contains("collect::<Result<") {
            let mut start = idx;
            while start > 0
                && idx - start < 12
                && !code(lines[start - 1]).ends_with([';', '{', '}'])
            {
                start -= 1;
            }
            if names_limit(&lines[start..=idx].join(" ")) {
                sites.push((idx + 1, line.to_string()));
            }
        } else if line.contains(": Result<Vec<") && line.contains(", _> =") {
            let mut end = idx;
            while end + 1 < lines.len() && end - idx < 15 && !lines[end].contains(".collect()") {
                end += 1;
            }
            if names_limit(&lines[idx..=end].join(" ")) {
                sites.push((idx + 1, line.to_string()));
            }
        }
        let before = depth;
        depth += line.matches('{').count() as i32 - line.matches('}').count() as i32;
        if opens_loop {
            loops.push(before);
        }
        while loops.last().is_some_and(|&open| depth <= open) {
            loops.pop();
        }
    }
    sites
}

#[test]
fn no_question_stops_at_the_first_limit() {
    let mut found = Vec::new();
    for (path, text) in sources() {
        let name = relative(&path);
        if !SCOPE.iter().any(|scope| name.starts_with(scope)) {
            continue;
        }
        for (line_no, line) in first_limit_sites(production(&text)) {
            found.push((name.clone(), line_no, line));
        }
    }
    let mut problems = Vec::new();
    for (name, line_no, line) in &found {
        if !ALLOWED
            .iter()
            .any(|(file, text, _)| file == name && line == text)
        {
            problems.push(format!(
                "{name}:{line_no}: `{line}` stops at the first limit; use `candidates::all_of`, \
                 `all_hold` or `decide`, or list it in ALLOWED with the reason"
            ));
        }
    }
    for (file, text, _) in ALLOWED {
        if !found
            .iter()
            .any(|(name, _, line)| name == file && line == text)
        {
            problems.push(format!(
                "{file}: `{text}` is allowed but no longer stops at the first limit; remove it from ALLOWED"
            ));
        }
    }
    assert!(problems.is_empty(), "{}", problems.join("\n"));
}

/// The check finds the shapes it is meant to find.
#[test]
fn the_first_limit_shapes_are_found() {
    let stops = [
        "fn f() {\n    for part in parts {\n        match ask(part) {\n            Err(limit) => return Err(limit),\n            Ok(_) => {}\n        }\n    }\n}\n",
        "fn f() {\n    for part in parts {\n        match ask(part) {\n            Err(limit) => {\n                found = Some(limit);\n                break;\n            }\n            Ok(_) => {}\n        }\n    }\n}\n",
        "fn f() -> Result<(), NormalizationLimit> {\n    for part in parts {\n        check(part)?;\n    }\n}\n",
        "fn f() {\n    let all = parts\n        .iter()\n        .map(|part| normalize_ty(db, part, scope, assumptions))\n        .collect::<Result<Vec<_>, _>>();\n}\n",
    ];
    for text in stops {
        assert_eq!(first_limit_sites(text).len(), 1, "{text}");
    }
    let asks_every_part = "fn f() {\n    for part in parts {\n        match ask(part) {\n            Err(limit) => {\n                unknown = Some(limit);\n                continue;\n            }\n            Ok(false) => return Holds::No,\n            Ok(true) => {}\n        }\n    }\n}\n";
    assert!(first_limit_sites(asks_every_part).is_empty());
}
