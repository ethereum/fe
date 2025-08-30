use dir_test::{dir_test, Fixture};
use std::process::Command;
use test_utils::snap_test;

// Helper function to normalize paths in output for portability
fn normalize_output(output: &str) -> String {
    // Get the project root directory
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let project_root = std::path::Path::new(manifest_dir)
        .parent()
        .expect("parent")
        .parent()
        .expect("parent");

    // Replace absolute paths with relative ones
    output.replace(&project_root.to_string_lossy().to_string(), "<project>")
}

// Helper function to run fe check
fn run_fe_check(path: &str) -> (String, i32) {
    run_fe_command("check", path)
}

// Helper function to run fe tree
fn run_fe_tree(path: &str) -> (String, i32) {
    run_fe_command("tree", path)
}

// Helper function to run fe binary with specified subcommand
fn run_fe_command(subcommand: &str, path: &str) -> (String, i32) {
    // Build the fe binary
    let cargo_exe = std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string());
    let output = Command::new(&cargo_exe)
        .args(["build", "--bin", "fe"])
        .output()
        .expect("Failed to build fe binary");

    if !output.status.success() {
        panic!(
            "Failed to build fe binary: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    // Run fe subcommand
    let fe_binary = std::env::current_exe()
        .expect("Failed to get current exe")
        .parent()
        .expect("Failed to get parent")
        .parent()
        .expect("Failed to get parent")
        .join("fe");

    let output = Command::new(&fe_binary)
        .args([subcommand, path])
        .env("NO_COLOR", "1") // Disable color output for consistent snapshots across environments
        .output()
        .unwrap_or_else(|_| panic!("Failed to run fe {}", subcommand));

    // Combine stdout and stderr for snapshot
    let mut full_output = String::new();
    if !output.stdout.is_empty() {
        full_output.push_str("=== STDOUT ===\n");
        full_output.push_str(&String::from_utf8_lossy(&output.stdout));
    }
    if !output.stderr.is_empty() {
        if !full_output.is_empty() {
            full_output.push('\n');
        }
        full_output.push_str("=== STDERR ===\n");
        full_output.push_str(&String::from_utf8_lossy(&output.stderr));
    }
    let exit_code = output.status.code().unwrap_or(-1);
    full_output.push_str(&format!("\n=== EXIT CODE: {exit_code} ==="));

    // Normalize paths for portability
    let normalized_output = normalize_output(&full_output);
    (normalized_output, exit_code)
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/cli_output/single_files",
    glob: "*.fe",
)]
fn test_cli_single_file(fixture: Fixture<&str>) {
    let (output, _) = run_fe_check(fixture.path());
    snap_test!(output, fixture.path());
}

// Back to dir_test - the unstable numbers are annoying but at least it works
#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/cli_output/ingots",
    glob: "**/fe.toml",
)]
fn test_cli_ingot(fixture: Fixture<&str>) {
    let ingot_dir = std::path::Path::new(fixture.path())
        .parent()
        .expect("fe.toml should have parent");

    let (output, _) = run_fe_check(ingot_dir.to_str().unwrap());

    // Use the ingot directory name for the snapshot to avoid numbering
    let ingot_name = ingot_dir.file_name().unwrap().to_str().unwrap();
    let snapshot_path = ingot_dir.join(ingot_name);
    snap_test!(output, snapshot_path.to_str().unwrap());
}

#[dir_test(
    dir: "$CARGO_MANIFEST_DIR/tests/fixtures/cli_output/ingots",
    glob: "**/fe.toml",
)]
fn test_tree_output(fixture: Fixture<&str>) {
    let ingot_dir = std::path::Path::new(fixture.path())
        .parent()
        .expect("fe.toml should have parent");

    let (output, _) = run_fe_tree(ingot_dir.to_str().unwrap());

    // Use the ingot directory name for the snapshot with _tree suffix
    let ingot_name = ingot_dir.file_name().unwrap().to_str().unwrap();
    let snapshot_path = ingot_dir.join(format!("{}_tree", ingot_name));
    snap_test!(output, snapshot_path.to_str().unwrap());
}
