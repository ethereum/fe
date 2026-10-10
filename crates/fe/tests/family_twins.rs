//! A program that writes an associated type with parameters behaves like its
//! twin, the same program with a plain associated type of the same
//! definition: `fe check` reports the same errors, and `fe test` gives the
//! same results at `-O 0` and `-O 2`. Each case is a pair of files in
//! `fixtures/family_twins`, `NAME.family.fe` and `NAME.plain.fe`.

use std::{path::Path, process::Command};

/// What `fe` reports for `path`: the error codes of `fe check`, sorted, and
/// for a program with tests, the exit status and the test results of
/// `fe test` at each optimization level. A crash is reported as such.
fn outcome(path: &Path) -> String {
    let run = |args: &[&str]| {
        let output = Command::new(env!("CARGO_BIN_EXE_fe"))
            .args(args)
            .arg(path)
            .env("NO_COLOR", "1")
            .output()
            .expect("fe runs");
        let text = String::from_utf8_lossy(&output.stdout).into_owned()
            + &String::from_utf8_lossy(&output.stderr);
        (text, output.status.code().unwrap_or(-1))
    };
    let summary = |text: &str| {
        let mut items: Vec<String> = text
            .lines()
            .filter_map(|line| {
                let line = line.trim_start();
                if let Some(rest) = line.strip_prefix("error[") {
                    rest.split_once(']')
                        .map(|(code, _)| format!("error[{code}]"))
                } else if line.starts_with("PASS") || line.starts_with("FAIL") {
                    line.split_whitespace().next().map(str::to_string)
                } else {
                    None
                }
            })
            .collect();
        if text.contains("panicked") || text.contains("overflowed its stack") {
            items.push("crash".to_string());
        }
        items.sort();
        items.join(" ")
    };
    let (check, status) = run(&["check", "--color", "never", "--standalone"]);
    let mut out = format!("check {status}: {}", summary(&check));
    let source = std::fs::read_to_string(path).unwrap();
    if source.contains("#[test]") {
        for level in ["0", "2"] {
            let (test, status) = run(&["test", "--color", "never", "-O", level]);
            out.push_str(&format!("; test -O {level} {status}: {}", summary(&test)));
        }
    }
    out
}

#[test]
fn a_family_program_behaves_like_its_plain_twin() {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/family_twins");
    let mut names: Vec<String> = std::fs::read_dir(&dir)
        .unwrap()
        .filter_map(|entry| {
            let name = entry.ok()?.file_name().into_string().ok()?;
            name.strip_suffix(".family.fe").map(str::to_string)
        })
        .collect();
    names.sort();
    assert!(!names.is_empty());
    let results: Vec<(String, String, String)> = std::thread::scope(|scope| {
        let handles: Vec<_> = names
            .iter()
            .map(|name| {
                let dir = &dir;
                scope.spawn(move || {
                    let family = outcome(&dir.join(format!("{name}.family.fe")));
                    let plain = outcome(&dir.join(format!("{name}.plain.fe")));
                    (name.clone(), family, plain)
                })
            })
            .collect();
        handles.into_iter().map(|h| h.join().unwrap()).collect()
    });
    let differing: Vec<String> = results
        .iter()
        .filter(|(_, family, plain)| family != plain)
        .map(|(name, family, plain)| format!("{name}:\n  family: {family}\n  plain:  {plain}"))
        .collect();
    assert!(differing.is_empty(), "{}", differing.join("\n"));
}

/// A struct or enum generic over a factory stores, once instantiated, the
/// types its fields name: every program in `fixtures/family_passes` checks
/// and its tests pass at both optimization levels.
#[test]
fn an_instantiated_field_is_the_type_it_names() {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/family_passes");
    let mut paths: Vec<_> = std::fs::read_dir(&dir)
        .unwrap()
        .filter_map(|entry| Some(entry.ok()?.path()))
        .filter(|path| path.extension().is_some_and(|ext| ext == "fe"))
        .collect();
    paths.sort();
    assert!(!paths.is_empty());
    let wrong: Vec<String> = paths
        .iter()
        .map(|path| (path, outcome(path)))
        .filter(|(_, outcome)| outcome != "check 0: ; test -O 0 0: PASS; test -O 2 0: PASS")
        .map(|(path, outcome)| format!("{}: {outcome}", path.display()))
        .collect();
    assert!(wrong.is_empty(), "{}", wrong.join("\n"));
}
