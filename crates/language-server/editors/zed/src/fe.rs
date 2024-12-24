use zed_extension_api::{self as zed, Result};

const SERVER_PATH: &str = "fe-language-server";

struct FeAnalyzerExtension;
impl FeAnalyzerExtension {
    fn server_script_path(&mut self, worktree: &zed::Worktree) -> Result<String> {
        worktree
            .which(SERVER_PATH)
            .ok_or_else(|| "fe-language-server not found in PATH".into())
    }
}

impl zed::Extension for FeAnalyzerExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &zed::LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let server_path = self.server_script_path(worktree)?;
        Ok(zed::Command {
            command: server_path,
            env: Default::default(),
            args: Default::default(),
        })
    }
}

zed::register_extension!(FeAnalyzerExtension);
