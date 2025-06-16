use camino::Utf8PathBuf;
use petgraph::graph::DiGraph;
use radix_immutable::{StringPrefixView, StringTrie, Trie};
use salsa::Setter;
use smol_str::SmolStr;
use url::Url;

use crate::config::IngotArguments;
use crate::InputDb;
use crate::{file::File, indexmap::IndexMap};

#[derive(Debug)]
pub enum InputIndexError {
    CannotReuseInput,
}

#[salsa::input]
#[derive(Debug)]
pub struct Workspace {
    files: StringTrie<Url, File>,
    paths: IndexMap<File, Url>,
    graph: DiGraph<Url, (SmolStr, IngotArguments)>,
}

#[salsa::tracked]
impl Workspace {
    pub fn default(db: &dyn InputDb) -> Self {
        Workspace::new(db, Trie::new(), IndexMap::new(), DiGraph::default())
    }
    pub(crate) fn set(
        &self,
        db: &mut dyn InputDb,
        url: Url,
        file: File,
    ) -> Result<File, InputIndexError> {
        // Check if the file is already associated with another URL
        let paths = self.paths(db);
        if let Some(existing_url) = paths.get(&file) {
            if existing_url != &url {
                return Err(InputIndexError::CannotReuseInput);
            }
        }

        let files = self.files(db);
        self.set_files(db).to(files.insert(url.clone(), file));
        let mut paths = self.paths(db);
        paths.insert(file, url);
        self.set_paths(db).to(paths);
        Ok(file)
    }

    pub fn join_graph(
        &self,
        db: &mut dyn InputDb,
        graph: DiGraph<Url, (SmolStr, IngotArguments)>,
        join_edges: Vec<(Url, Url, (SmolStr, IngotArguments))>,
    ) {
        let old_graph = self.graph(db);
        let combined_graph = join_graphs(&old_graph, &graph, &join_edges);
        self.set_graph(db).to(combined_graph);
    }

    pub fn get(&self, db: &dyn InputDb, url: &Url) -> Option<File> {
        self.files(db).get(url).cloned()
    }

    pub fn remove(&self, db: &mut dyn InputDb, url: &Url) -> Option<File> {
        if let Some(_file) = self.files(db).get(url) {
            let files = self.files(db);
            if let (files, Some(file)) = files.remove(url) {
                self.set_files(db).to(files);
                let mut paths = self.paths(db);
                paths.remove(&file);
                Some(file)
            } else {
                None
            }
        } else {
            None
        }
    }

    #[salsa::tracked]
    pub fn items_at_base(self, db: &dyn InputDb, base: Url) -> StringPrefixView<'_, Url, File> {
        self.files(db).view_subtrie(base)
    }

    pub fn get_path(&self, db: &dyn InputDb, file: File) -> Option<Url> {
        self.paths(db).get(&file).cloned()
    }

    #[salsa::tracked]
    pub fn get_relative_path(self, db: &dyn InputDb, base: Url, file: File) -> Option<Utf8PathBuf> {
        // Get the file path
        let file_url = match self.paths(db).get(&file) {
            Some(url) => url.clone(),
            None => return None,
        };

        // Get the relative path between the base URL and file URL
        base.make_relative(&file_url).map(Utf8PathBuf::from)
    }

    pub fn touch(&self, db: &mut dyn InputDb, url: Url, initial_content: Option<String>) -> File {
        // Check if the file already exists
        if let Some(file) = self.get(db, &url) {
            return file;
        }
        let initial = initial_content.unwrap_or_default();

        let input_file = File::__new_impl(db, initial);
        self.set(db, url, input_file)
            .expect("Failed to create file")
    }

    pub fn all_files(&self, db: &dyn InputDb) -> StringTrie<Url, File> {
        self.files(db)
    }

    pub fn get_graph(&self, db: &dyn InputDb) -> DiGraph<Url, (SmolStr, IngotArguments)> {
        self.graph(db).clone()
    }
}

use petgraph::graph::NodeIndex;
use std::collections::HashMap;

fn join_graphs(
    g1: &DiGraph<Url, (SmolStr, IngotArguments)>,
    g2: &DiGraph<Url, (SmolStr, IngotArguments)>,
    join_edges: &[(Url, Url, (SmolStr, IngotArguments))],
) -> DiGraph<Url, (SmolStr, IngotArguments)> {
    let mut combined = DiGraph::new();

    let mut node_map = HashMap::<Url, NodeIndex>::new();

    // Insert g1 nodes
    for idx in g1.node_indices() {
        let url = &g1[idx];
        let new_idx = combined.add_node(url.clone());
        node_map.insert(url.clone(), new_idx);
    }

    // Insert g2 nodes
    for idx in g2.node_indices() {
        let url = &g2[idx];
        let new_idx = combined.add_node(url.clone());
        node_map.insert(url.clone(), new_idx);
    }

    // Insert g1 edges
    for edge in g1.edge_references() {
        let source_url = &g1[edge.source()];
        let target_url = &g1[edge.target()];
        combined.add_edge(
            node_map[source_url],
            node_map[target_url],
            edge.weight().clone(),
        );
    }

    // Insert g2 edges
    for edge in g2.edge_references() {
        let source_url = &g2[edge.source()];
        let target_url = &g2[edge.target()];
        combined.add_edge(
            node_map[source_url],
            node_map[target_url],
            edge.weight().clone(),
        );
    }

    // Insert join edges (across g1 and g2)
    for (from_url, to_url, weight) in join_edges {
        if let (Some(&from), Some(&to)) = (node_map.get(from_url), node_map.get(to_url)) {
            combined.add_edge(from, to, weight.clone());
        }
    }

    combined
}

use petgraph::visit::EdgeRef;

#[cfg(test)]
mod tests {
    use super::*;

    use crate::define_input_db;

    define_input_db!(TestDatabase);

    #[test]
    fn test_input_index_basic() {
        let mut db = TestDatabase::default();
        let index = db.workspace();

        // Create a file and add it to the index
        let file = File::__new_impl(&db, "test content".to_string());
        let url = Url::parse("file:///test.fe").unwrap();

        index
            .set(&mut db, url.clone(), file)
            .expect("Failed to set file");

        // Test we can look up the file
        let retrieved_file = index.get(&db, &url);
        assert!(retrieved_file.is_some());
        assert_eq!(retrieved_file.unwrap(), file);

        // Test removal
        let removed_file = index.remove(&mut db, &url);
        assert!(removed_file.is_some());
        assert_eq!(removed_file.unwrap(), file);

        // Verify it's gone
        let retrieved_file = index.get(&db, &url);
        assert!(retrieved_file.is_none());
    }

    #[test]
    fn test_input_index_path() {
        let mut db = TestDatabase::default();
        let index = db.workspace();

        // Create a file and add it to the index
        let file = File::__new_impl(&db, "test content".to_string());
        let url = Url::parse("file:///test.fe").unwrap();

        index
            .set(&mut db, url.clone(), file)
            .expect("Failed to set file");

        // Test we can look up the path
        let path = index.get_path(&db, file);
        assert!(path.is_some());
        assert_eq!(path.unwrap(), url);

        // Test we can look up a cloned file's path
        #[allow(clippy::clone_on_copy)]
        let cloned_file = file.clone();
        let path = index.get_path(&db, cloned_file);
        assert!(path.is_some());
        assert_eq!(path.unwrap(), url);
    }
}
