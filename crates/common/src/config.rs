use serde_semver::semver::Version;
use smol_str::SmolStr;

#[derive(Default, Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct IngotMetadata {
    pub name: Option<SmolStr>,
    pub version: Option<Version>,
}
