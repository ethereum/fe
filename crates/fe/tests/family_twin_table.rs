//! Associated types with parameters agree with their plain twins through
//! the whole pipeline that `fe check` runs, instance building and borrow
//! checking included. The cases are shared with the analysis test in
//! `crates/hir/tests/traits/family_twin_cases.rs`.

use std::process::Command;

include!("../../hir/tests/traits/family_twin_cases.rs");

/// The error codes `fe check` reports for `src`, sorted; `crash` for a panic
/// or a stack overflow.
fn codes(src: &str) -> Vec<String> {
    use std::sync::atomic::{AtomicUsize, Ordering};
    static NEXT: AtomicUsize = AtomicUsize::new(0);
    let dir = std::env::temp_dir().join(format!("fe-family-twins-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join(format!("p{}.fe", NEXT.fetch_add(1, Ordering::Relaxed)));
    std::fs::write(&path, src).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_fe"))
        .args(["check", "--color", "never", "--standalone"])
        .arg(&path)
        .output()
        .expect("fe runs");
    let _ = std::fs::remove_file(&path);
    let text = String::from_utf8_lossy(&output.stdout).into_owned()
        + &String::from_utf8_lossy(&output.stderr);
    let mut codes: Vec<String> = text
        .lines()
        .filter_map(|line| line.strip_prefix("error["))
        .filter_map(|rest| rest.split_once(']'))
        .map(|(code, _)| code.to_string())
        .collect();
    if text.contains("panicked") || text.contains("overflowed its stack") {
        codes.push("crash".to_string());
    }
    codes.sort();
    codes
}

#[test]
fn families_agree_with_their_plain_twins_through_the_pipeline() {
    check_all_twins(&codes, 8, false);
}

#[test]
fn family_rows_of_the_limit_matrix_through_the_pipeline() {
    family_limit::check(&codes);
}

#[test]
fn nested_family_applications_through_the_pipeline() {
    check_nested_applications(&codes);
}

/// The self-checking `fe test` fixtures `fixtures/fe_test/family_twin_*.fe`,
/// each a family and a plain version of one program asserting equal results,
/// pass at both optimization levels (the fixture harness runs only the
/// default one).
#[test]
fn family_twin_fixtures_pass_at_each_optimization_level() {
    let dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/fe_test");
    let mut paths: Vec<_> = std::fs::read_dir(&dir)
        .unwrap()
        .filter_map(|entry| Some(entry.ok()?.path()))
        .filter(|path| {
            path.file_name()
                .and_then(|name| name.to_str())
                .is_some_and(|name| name.starts_with("family_twin_") && name.ends_with(".fe"))
        })
        .collect();
    paths.sort();
    assert!(!paths.is_empty());
    let runs: Vec<(String, &str)> = paths
        .iter()
        .flat_map(|path| ["0", "2"].map(|level| (path.display().to_string(), level)))
        .collect();
    let failures: Vec<String> = std::thread::scope(|scope| {
        let handles: Vec<_> = runs
            .iter()
            .map(|(path, level)| {
                scope.spawn(move || {
                    let output = Command::new(env!("CARGO_BIN_EXE_fe"))
                        .args(["test", "--color", "never", "--jobs", "1", "-O", level])
                        .arg(path)
                        .output()
                        .expect("fe runs");
                    (!output.status.success()).then(|| {
                        format!(
                            "{path} at -O {level}:\n{}{}",
                            String::from_utf8_lossy(&output.stdout),
                            String::from_utf8_lossy(&output.stderr)
                        )
                    })
                })
            })
            .collect();
        handles
            .into_iter()
            .filter_map(|h| h.join().unwrap())
            .collect()
    });
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
