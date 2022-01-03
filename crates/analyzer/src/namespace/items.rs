use crate::builtins;
use crate::context;
use crate::context::Analysis;
use crate::errors::{self, TypeError};
use crate::impl_intern_key;
use crate::namespace::types::{self, GenericType};
use crate::traversal::pragma::check_pragma_version;
use crate::AnalyzerDb;
use fe_common::diagnostics::Diagnostic;
use fe_common::files::{FileStore, SourceFile, SourceFileId};
use fe_parser::ast;
use fe_parser::ast::Expr;
use fe_parser::node::{Node, Span};
use indexmap::indexmap;
use indexmap::IndexMap;
use smol_str::SmolStr;
use std::collections::BTreeMap;
use std::ops::Deref;
use std::path::Path;
use std::rc::Rc;
use strum::IntoEnumIterator;

/// A named item. This does not include things inside of
/// a function body.
#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy)]
pub enum Item {
    Ingot(IngotId),
    Module(ModuleId),
    Type(TypeDef),
    // GenericType probably shouldn't be a separate category.
    // Any of the items inside TypeDef (struct, alias, etc)
    // could be optionally generic.
    GenericType(GenericType),
    // Events aren't normal types; they *could* be moved into
    // TypeDef, but it would have consequences.
    Event(EventId),
    Function(FunctionId),
    Constant(ModuleConstantId),
    // Needed until we can represent keccak256 as a FunctionId.
    // We can't represent keccak256's arg type yet.
    BuiltinFunction(builtins::GlobalFunction),
    Intrinsic(builtins::Intrinsic),
    // This should go away soon. The globals (block, msg, etc) will be replaced
    // with a context struct that'll appear in the fn parameter list.
    Object(builtins::GlobalObject),
}

impl Item {
    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        match self {
            Item::Type(id) => id.name(db),
            Item::GenericType(id) => id.name(),
            Item::Event(id) => id.name(db),
            Item::Function(id) => id.name(db),
            Item::BuiltinFunction(id) => id.as_ref().into(),
            Item::Intrinsic(id) => id.as_ref().into(),
            Item::Object(id) => id.as_ref().into(),
            Item::Constant(id) => id.name(db),
            Item::Ingot(id) => id.name(db),
            Item::Module(id) => id.name(db),
        }
    }

    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        match self {
            Item::Type(id) => id.name_span(db),
            Item::GenericType(_) => None,
            Item::Event(id) => Some(id.name_span(db)),
            Item::Function(id) => Some(id.name_span(db)),
            Item::BuiltinFunction(_) => None,
            Item::Intrinsic(_) => None,
            Item::Object(_) => None,
            Item::Constant(id) => Some(id.name_span(db)),
            Item::Ingot(_) => None,
            Item::Module(_) => None,
        }
    }

    pub fn is_builtin(&self) -> bool {
        match self {
            Item::Type(TypeDef::Primitive(_)) => true,
            Item::Type(_) => false,
            Item::GenericType(_) => true,
            Item::Event(_) => false,
            Item::Function(_) => false,
            Item::BuiltinFunction(_) => true,
            Item::Intrinsic(_) => true,
            Item::Object(_) => true,
            Item::Constant(_) => false,
            Item::Ingot(_) => false,
            Item::Module(_) => false,
        }
    }

    pub fn is_struct(&self, val: &StructId) -> bool {
        matches!(self, Item::Type(TypeDef::Struct(current)) if current == val)
    }

    pub fn item_kind_display_name(&self) -> &'static str {
        match self {
            Item::Type(_) => "type",
            Item::GenericType(_) => "type",
            Item::Event(_) => "event",
            Item::Function(_) => "function",
            Item::BuiltinFunction(_) => "function",
            Item::Intrinsic(_) => "intrinsic function",
            Item::Object(_) => "object",
            Item::Constant(_) => "constant",
            Item::Ingot(_) => "ingot",
            Item::Module(_) => "module",
        }
    }

    pub fn items(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, Item>> {
        match self {
            Item::Ingot(ingot) => ingot.items(db),
            Item::Module(module) => module.items(db),
            Item::Type(_) => todo!("cannot access items in types yet"),
            Item::GenericType(_)
            | Item::Event(_)
            | Item::Function(_)
            | Item::Constant(_)
            | Item::BuiltinFunction(_)
            | Item::Intrinsic(_)
            | Item::Object(_) => Rc::new(indexmap! {}),
        }
    }

    pub fn parent(&self, db: &dyn AnalyzerDb) -> Option<Item> {
        match self {
            Item::Type(id) => id.parent(db),
            Item::GenericType(_) => None,
            Item::Event(id) => Some(id.parent(db)),
            Item::Function(id) => Some(id.parent(db)),
            Item::BuiltinFunction(_) => None,
            Item::Intrinsic(_) => None,
            Item::Object(_) => None,
            Item::Constant(id) => Some(id.parent(db)),
            Item::Ingot(_) => None,
            Item::Module(id) => id.parent(db),
        }
    }

    pub fn path(&self, db: &dyn AnalyzerDb) -> Rc<Vec<SmolStr>> {
        // The path is used to generate a yul identifier,
        // eg `foo::Bar::new` becomes `$$foo$Bar$new`.
        // Right now, the ingot name is the os path, so it could
        // be "my project/src".
        // For now, we'll just leave the ingot out of the path,
        // because we can only compile a single ingot anyway.
        match self.parent(db) {
            Some(Item::Ingot(_)) | None => Rc::new(vec![self.name(db)]),
            Some(parent) => {
                let mut path = parent.path(db).as_ref().clone();
                path.push(self.name(db));
                Rc::new(path)
            }
        }
    }

    pub fn dependency_graph(&self, db: &dyn AnalyzerDb) -> Option<Rc<DepGraph>> {
        match self {
            Item::Type(TypeDef::Contract(id)) => Some(id.dependency_graph(db)),
            Item::Type(TypeDef::Struct(id)) => Some(id.dependency_graph(db)),
            Item::Function(id) => Some(id.dependency_graph(db)),
            _ => None,
        }
    }

    pub fn resolve_path_segments(
        &self,
        db: &dyn AnalyzerDb,
        segments: &[Node<SmolStr>],
    ) -> Analysis<Option<Item>> {
        let mut curr_item = *self;

        for node in segments {
            curr_item = match curr_item.items(db).get(&node.kind) {
                Some(item) => *item,
                None => {
                    return Analysis {
                        value: None,
                        diagnostics: Rc::new(vec![errors::error(
                            "unresolved path item",
                            node.span,
                            "not found",
                        )]),
                    }
                }
            }
        }

        Analysis {
            value: Some(curr_item),
            diagnostics: Rc::new(vec![]),
        }
    }

    /// Downcast utility function
    pub fn as_contract(&self) -> Option<ContractId> {
        match self {
            Item::Type(TypeDef::Contract(id)) => Some(*id),
            _ => None,
        }
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        match self {
            Item::Type(id) => id.sink_diagnostics(db, sink),
            Item::GenericType(_) => {}
            Item::Event(id) => id.sink_diagnostics(db, sink),
            Item::Function(id) => id.sink_diagnostics(db, sink),
            Item::BuiltinFunction(_) | Item::Intrinsic(_) | Item::Object(_) => {}
            Item::Constant(id) => id.sink_diagnostics(db, sink),
            Item::Ingot(id) => id.sink_diagnostics(db, sink),
            Item::Module(id) => id.sink_diagnostics(db, sink),
        }
    }
}

// Placeholder; someday std::prelude will be a proper module.
pub fn std_prelude_items() -> IndexMap<SmolStr, Item> {
    let mut items = indexmap! {
        SmolStr::new("bool") => Item::Type(TypeDef::Primitive(types::Base::Bool)),
        SmolStr::new("address") => Item::Type(TypeDef::Primitive(types::Base::Address)),
    };
    items.extend(types::Integer::iter().map(|typ| {
        (
            typ.as_ref().into(),
            Item::Type(TypeDef::Primitive(types::Base::Numeric(typ))),
        )
    }));
    items.extend(types::GenericType::iter().map(|typ| (typ.name(), Item::GenericType(typ))));
    items.extend(
        builtins::GlobalFunction::iter()
            .map(|fun| (fun.as_ref().into(), Item::BuiltinFunction(fun))),
    );
    items
        .extend(builtins::Intrinsic::iter().map(|fun| (fun.as_ref().into(), Item::Intrinsic(fun))));
    items
        .extend(builtins::GlobalObject::iter().map(|obj| (obj.as_ref().into(), Item::Object(obj))));
    items
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Default)]
pub struct Global {
    pub ingots: BTreeMap<SmolStr, IngotId>,
}

impl Global {
    pub fn try_new(
        db: &dyn AnalyzerDb,
        files: &FileStore,
        deps: &IndexMap<SmolStr, Vec<SourceFileId>>,
    ) -> Result<Analysis<Self>, Vec<Diagnostic>> {
        let mut diagnostics = vec![];
        let mut fatal_diagnostics = vec![];

        let ingots = deps
            .into_iter()
            .filter_map(|(name, file_ids)| {
                // dep map is left empty for now
                match IngotId::try_new(db, files, name, file_ids, &indexmap! {}) {
                    Ok(analysis) => {
                        diagnostics.extend(analysis.diagnostics.deref().clone());
                        Some((name.to_owned(), analysis.value))
                    }
                    Err(diagnostics) => {
                        fatal_diagnostics.extend(diagnostics);
                        None
                    }
                }
            })
            .collect();

        if fatal_diagnostics.is_empty() {
            Ok(Analysis {
                value: Global { ingots },
                diagnostics: Rc::new(diagnostics),
            })
        } else {
            Err(fatal_diagnostics)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub struct GlobalId(pub(crate) u32);
impl_intern_key!(GlobalId);
impl GlobalId {
    pub fn try_new(
        db: &dyn AnalyzerDb,
        files: &FileStore,
        deps: &IndexMap<SmolStr, Vec<SourceFileId>>,
    ) -> Result<Analysis<Self>, Vec<Diagnostic>> {
        match Global::try_new(db, files, deps) {
            Ok(analysis) => Ok(Analysis {
                value: db.intern_global(Rc::new(analysis.value)),
                diagnostics: analysis.diagnostics,
            }),
            Err(diagnostics) => Err(diagnostics),
        }
    }

    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Global> {
        db.lookup_intern_global(*self)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ingot {
    pub name: SmolStr,
    // pub version: String,
    pub global: GlobalId,
    // `BTreeMap` implements `Hash`, which is required for an ID.
    pub fe_files: BTreeMap<SourceFileId, (SourceFile, ast::Module)>,
}

impl Ingot {
    pub fn try_new(
        db: &dyn AnalyzerDb,
        files: &FileStore,
        name: &str,
        file_ids: &[SourceFileId],
        deps: &IndexMap<SmolStr, Vec<SourceFileId>>,
    ) -> Result<Analysis<Self>, Vec<Diagnostic>> {
        let global_analysis = GlobalId::try_new(db, files, deps)?;

        let mut diagnostics = global_analysis.diagnostics.deref().clone();
        let mut fatal_diagnostics = vec![];

        let ingot = Self {
            name: name.into(),
            global: global_analysis.value,
            fe_files: file_ids
                .iter()
                .filter_map(|file_id| {
                    let file = files.get_file(*file_id).expect("missing file for ID");
                    match fe_parser::parse_file(*file_id, &file.content) {
                        Ok((ast, parser_diagnostics)) => {
                            diagnostics.extend(parser_diagnostics);
                            Some((*file_id, (file.to_owned(), ast)))
                        }
                        Err(diagnostics) => {
                            fatal_diagnostics.extend(diagnostics);
                            None
                        }
                    }
                })
                .collect(),
        };

        if fatal_diagnostics.is_empty() {
            Ok(Analysis {
                value: ingot,
                diagnostics: Rc::new(diagnostics),
            })
        } else {
            Err(fatal_diagnostics)
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct IngotId(pub(crate) u32);
impl_intern_key!(IngotId);
impl IngotId {
    pub fn try_new(
        db: &dyn AnalyzerDb,
        files: &FileStore,
        name: &str,
        file_ids: &[SourceFileId],
        deps: &IndexMap<SmolStr, Vec<SourceFileId>>,
    ) -> Result<Analysis<Self>, Vec<Diagnostic>> {
        match Ingot::try_new(db, files, name, file_ids, deps) {
            Ok(analysis) => Ok(Analysis {
                value: db.intern_ingot(Rc::new(analysis.value)),
                diagnostics: analysis.diagnostics,
            }),
            Err(diagnostics) => Err(diagnostics),
        }
    }

    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Ingot> {
        db.lookup_intern_ingot(*self)
    }

    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).name.clone()
    }

    /// Returns the `src/main.fe` module, if it exists.
    pub fn main_module(&self, db: &dyn AnalyzerDb) -> Option<ModuleId> {
        db.ingot_main_module(*self).value
    }

    /// Returns the `src/lib.fe` module, if it exists.
    pub fn lib_module(&self, db: &dyn AnalyzerDb) -> Option<ModuleId> {
        db.ingot_lib_module(*self).value
    }

    /// Returns the `src/lib.fe` or `src/main.fe` module, whichever one exists.
    pub fn root_module(&self, db: &dyn AnalyzerDb) -> Option<ModuleId> {
        db.ingot_root_module(*self)
    }

    /// Returns all of the modules inside of `src`, except for the root module.
    pub fn root_sub_modules(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, ModuleId>> {
        db.ingot_root_sub_modules(*self)
    }

    pub fn items(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, Item>> {
        self.root_module(db).expect("missing root module").items(db)
    }

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];
        self.sink_diagnostics(db, &mut diagnostics);
        diagnostics
    }

    pub fn all_modules(&self, db: &dyn AnalyzerDb) -> Rc<Vec<ModuleId>> {
        db.ingot_all_modules(*self)
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        let modules = self.all_modules(db);

        for module in modules.iter() {
            module.sink_diagnostics(db, sink)
        }

        sink.push_all(db.ingot_main_module(*self).diagnostics.iter());
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ModuleFileContent {
    Dir {
        // directories will have a corresponding source file. we can remove
        // the `dir_path` attribute when this is added.
        // file: SourceFileId,
        dir_path: SmolStr,
    },
    File {
        file: SourceFileId,
    },
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ModuleContext {
    Ingot(IngotId),
    Global(GlobalId),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Module {
    pub name: SmolStr,
    pub context: ModuleContext,
    pub file_content: ModuleFileContent,
    pub ast: ast::Module,
}

impl Module {
    pub fn try_new(
        db: &dyn AnalyzerDb,
        files: &FileStore,
        file_id: SourceFileId,
        deps: &IndexMap<SmolStr, Vec<SourceFileId>>,
    ) -> Result<Analysis<Self>, Vec<Diagnostic>> {
        let global_analysis = GlobalId::try_new(db, files, deps)?;
        let mut diagnostics = global_analysis.diagnostics.deref().clone();

        let file = files.get_file(file_id).expect("missing file");
        let name = Path::new(&file.name)
            .file_stem()
            .expect("missing file name")
            .to_string_lossy()
            .to_string();

        let (ast, parser_diagnostics) = fe_parser::parse_file(file_id, &file.content)?;
        diagnostics.extend(parser_diagnostics);

        let module = Module {
            name: name.into(),
            context: ModuleContext::Global(global_analysis.value),
            file_content: ModuleFileContent::File { file: file_id },
            ast,
        };

        Ok(Analysis {
            value: module,
            diagnostics: Rc::new(diagnostics),
        })
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct ModuleId(pub(crate) u32);
impl_intern_key!(ModuleId);
impl ModuleId {
    pub fn try_new(
        db: &dyn AnalyzerDb,
        files: &FileStore,
        file_id: SourceFileId,
        deps: &IndexMap<SmolStr, Vec<SourceFileId>>,
    ) -> Result<Analysis<Self>, Vec<Diagnostic>> {
        match Module::try_new(db, files, file_id, deps) {
            Ok(analysis) => Ok(Analysis {
                value: db.intern_module(Rc::new(analysis.value)),
                diagnostics: analysis.diagnostics,
            }),
            Err(diagnostics) => Err(diagnostics),
        }
    }

    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Module> {
        db.lookup_intern_module(*self)
    }

    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).name.clone()
    }

    pub fn file_content(&self, db: &dyn AnalyzerDb) -> ModuleFileContent {
        self.data(db).file_content.clone()
    }

    pub fn ingot_path(&self, db: &dyn AnalyzerDb) -> SmolStr {
        match self.context(db) {
            ModuleContext::Ingot(ingot) => match self.file_content(db) {
                ModuleFileContent::Dir { dir_path } => dir_path,
                ModuleFileContent::File { file } => {
                    ingot.data(db).fe_files[&file].0.name.as_str().into()
                }
            },
            ModuleContext::Global(_) => panic!("cannot get path"),
        }
    }

    pub fn ast(&self, db: &dyn AnalyzerDb) -> ast::Module {
        self.data(db).ast.clone()
    }

    pub fn context(&self, db: &dyn AnalyzerDb) -> ModuleContext {
        self.data(db).context.clone()
    }

    /// Includes duplicate names
    pub fn all_items(&self, db: &dyn AnalyzerDb) -> Rc<Vec<Item>> {
        db.module_all_items(*self)
    }

    /// Returns a map of the named items in the module
    pub fn items(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, Item>> {
        db.module_item_map(*self).value
    }

    /// Returns a `name -> (name_span, external_item)` map for all `use` statements in a module.
    pub fn used_items(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, (Span, Item)>> {
        db.module_used_item_map(*self).value
    }

    /// Returns all of the internal items, except for used items. This is used when resolving
    /// use statements, as it does not create a query cycle.
    pub fn non_used_internal_items(&self, db: &dyn AnalyzerDb) -> IndexMap<SmolStr, Item> {
        let global_items = self.global_items(db);
        let sub_modules = self.sub_modules(db);

        sub_modules
            .deref()
            .clone()
            .into_iter()
            .map(|(name, module)| (name, Item::Module(module)))
            .chain(global_items)
            .collect()
    }

    /// Returns all of the internal items. Internal items refers to the set of items visible when
    /// inside of a module.
    pub fn internal_items(&self, db: &dyn AnalyzerDb) -> IndexMap<SmolStr, Item> {
        let global_items = self.global_items(db);
        let defined_items = self.items(db);
        let sub_modules = self.sub_modules(db);

        sub_modules
            .deref()
            .clone()
            .into_iter()
            .map(|(name, module)| (name, Item::Module(module)))
            .chain(global_items)
            .chain(defined_items.deref().clone())
            .collect()
    }

    /// Resolve a path that starts with an item defined in the module.
    pub fn resolve_path(&self, db: &dyn AnalyzerDb, path: &ast::Path) -> Analysis<Option<Item>> {
        Item::Module(*self).resolve_path_segments(db, &path.segments)
    }

    /// Resolve a path that starts with an internal item. We omit used items to avoid a query cycle.
    pub fn resolve_path_non_used_internal(
        &self,
        db: &dyn AnalyzerDb,
        path: &ast::Path,
    ) -> Analysis<Option<Item>> {
        let segments = &path.segments;
        let first_segment = &segments[0];

        if let Some(curr_item) = self.non_used_internal_items(db).get(&first_segment.kind) {
            curr_item.resolve_path_segments(db, &segments[1..])
        } else {
            Analysis {
                value: None,
                diagnostics: Rc::new(vec![errors::error(
                    "unresolved path item",
                    first_segment.span,
                    "not found",
                )]),
            }
        }
    }

    /// Resolve a path that starts with an internal item.
    pub fn resolve_path_internal(
        &self,
        db: &dyn AnalyzerDb,
        path: &ast::Path,
    ) -> Analysis<Option<Item>> {
        let segments = &path.segments;
        let first_segment = &segments[0];

        if let Some(curr_item) = self.internal_items(db).get(&first_segment.kind) {
            curr_item.resolve_path_segments(db, &segments[1..])
        } else {
            Analysis {
                value: None,
                diagnostics: Rc::new(vec![errors::error(
                    "unresolved path item",
                    first_segment.span,
                    "not found",
                )]),
            }
        }
    }

    /// Resolve a use tree entirely. We set internal to true if the first path item is internal.
    ///
    /// e.g. `foo::bar::{baz::bing}`
    ///       ---        ---
    ///        ^          ^ baz is not internal
    ///        foo is internal
    pub fn resolve_use_tree(
        &self,
        db: &dyn AnalyzerDb,
        tree: &Node<ast::UseTree>,
        internal: bool,
    ) -> Analysis<Rc<IndexMap<SmolStr, (Span, Item)>>> {
        let mut diagnostics = vec![];

        // Again, the path resolution method we use depends on whether or not the first item
        // is internal.
        let resolve_path = |module: &ModuleId, db: &dyn AnalyzerDb, path: &ast::Path| {
            if internal {
                module.resolve_path_non_used_internal(db, path)
            } else {
                module.resolve_path(db, path)
            }
        };

        match &tree.kind {
            ast::UseTree::Glob { prefix } => {
                let prefix_module = resolve_path(self, db, prefix);
                diagnostics.extend(prefix_module.diagnostics.deref().clone());

                let items = match prefix_module.value {
                    Some(Item::Module(module)) => module
                        .items(db)
                        .deref()
                        .clone()
                        .into_iter()
                        .map(|(name, item)| (name, (tree.span, item)))
                        .collect(),
                    Some(item) => {
                        diagnostics.push(errors::error(
                            format!("cannot glob import from {}", item.item_kind_display_name()),
                            prefix.segments.last().expect("path is empty").span,
                            "prefix item must be a module",
                        ));
                        indexmap! {}
                    }
                    None => indexmap! {},
                };

                Analysis {
                    value: Rc::new(items),
                    diagnostics: Rc::new(diagnostics),
                }
            }
            ast::UseTree::Nested { prefix, children } => {
                let prefix_module = resolve_path(self, db, prefix);
                diagnostics.extend(prefix_module.diagnostics.deref().clone());

                let items = match prefix_module.value {
                    Some(Item::Module(module)) => {
                        children.iter().fold(indexmap! {}, |mut accum, node| {
                            let child_items = module.resolve_use_tree(db, node, false);
                            diagnostics.extend(child_items.diagnostics.deref().clone());

                            for (name, (name_span, item)) in child_items.value.iter() {
                                if let Some((other_name_span, other_item)) =
                                    accum.insert(name.to_owned(), (*name_span, *item))
                                {
                                    diagnostics.push(errors::duplicate_name_error(
                                        &format!(
                                            "a {} with the same name has already been imported",
                                            other_item.item_kind_display_name()
                                        ),
                                        name,
                                        other_name_span,
                                        *name_span,
                                    ));
                                }
                            }

                            accum
                        })
                    }
                    Some(item) => {
                        diagnostics.push(errors::error(
                            format!("cannot glob import from {}", item.item_kind_display_name()),
                            prefix.segments.last().unwrap().span,
                            "prefix item must be a module",
                        ));
                        indexmap! {}
                    }
                    None => indexmap! {},
                };

                Analysis {
                    value: Rc::new(items),
                    diagnostics: Rc::new(diagnostics),
                }
            }
            ast::UseTree::Simple { path, rename } => {
                let item = resolve_path(self, db, path);

                let items = match item.value {
                    Some(item) => {
                        let (item_name, item_name_span) = if let Some(name) = rename {
                            (name.kind.clone(), name.span)
                        } else {
                            let name_segment_node = path.segments.last().expect("path is empty");
                            (name_segment_node.kind.clone(), name_segment_node.span)
                        };

                        indexmap! { item_name => (item_name_span, item) }
                    }
                    None => indexmap! {},
                };

                Analysis {
                    value: Rc::new(items),
                    diagnostics: item.diagnostics,
                }
            }
        }
    }

    pub fn resolve_name(&self, db: &dyn AnalyzerDb, name: &str) -> Option<Item> {
        self.internal_items(db).get(name).copied()
    }

    pub fn sub_modules(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, ModuleId>> {
        db.module_sub_modules(*self)
    }

    pub fn parent(&self, db: &dyn AnalyzerDb) -> Option<Item> {
        self.parent_module(db).map(Item::Module).or_else(|| {
            if let ModuleContext::Ingot(ingot) = self.data(db).context {
                Some(Item::Ingot(ingot))
            } else {
                None
            }
        })
    }

    pub fn parent_module(&self, db: &dyn AnalyzerDb) -> Option<ModuleId> {
        db.module_parent_module(*self)
    }

    /// All contracts, including duplicates
    pub fn all_contracts(&self, db: &dyn AnalyzerDb) -> Rc<Vec<ContractId>> {
        db.module_contracts(*self)
    }

    /// Returns the global item for this module.
    pub fn global(&self, db: &dyn AnalyzerDb) -> GlobalId {
        match self.context(db) {
            ModuleContext::Ingot(ingot) => ingot.data(db).global,
            ModuleContext::Global(global) => global,
        }
    }

    /// Returns the map of ingot deps, built-ins, and the ingot itself as "ingot".
    pub fn global_items(&self, db: &dyn AnalyzerDb) -> IndexMap<SmolStr, Item> {
        let mut items = self
            .global(db)
            .data(db)
            .ingots
            .clone()
            .into_iter()
            .map(|(name, ingot)| (name, Item::Ingot(ingot)))
            .chain(std_prelude_items())
            .collect::<IndexMap<_, _>>();

        if let ModuleContext::Ingot(ingot) = self.context(db) {
            items.insert("ingot".into(), Item::Ingot(ingot));
        }

        items
    }

    /// All structs, including duplicatecrates/analyzer/src/db.rss
    pub fn all_structs(&self, db: &dyn AnalyzerDb) -> Rc<Vec<StructId>> {
        db.module_structs(*self)
    }

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];
        self.sink_diagnostics(db, &mut diagnostics);
        diagnostics
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        let ast::Module { body } = &self.data(db).ast;
        for stmt in body {
            if let ast::ModuleStmt::Pragma(inner) = stmt {
                if let Some(diag) = check_pragma_version(inner) {
                    sink.push(&diag)
                }
            }
        }

        // duplicate item name errors
        sink.push_all(db.module_item_map(*self).diagnostics.iter());

        // errors for each item
        self.all_items(db)
            .iter()
            .for_each(|id| id.sink_diagnostics(db, sink));
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ModuleConstant {
    pub ast: Node<ast::ConstantDecl>,
    pub module: ModuleId,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct ModuleConstantId(pub(crate) u32);
impl_intern_key!(ModuleConstantId);

impl ModuleConstantId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<ModuleConstant> {
        db.lookup_intern_module_const(*self)
    }
    pub fn span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.span
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Result<types::Type, TypeError> {
        db.module_constant_type(*self).value
    }

    pub fn is_base_type(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.typ(db), Ok(types::Type::Base(_)))
    }

    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).ast.kind.name.kind.clone()
    }

    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.kind.name.span
    }

    pub fn value(&self, db: &dyn AnalyzerDb) -> ast::Expr {
        self.data(db).ast.kind.value.kind.clone()
    }

    pub fn parent(&self, db: &dyn AnalyzerDb) -> Item {
        Item::Module(self.data(db).module)
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        db.module_constant_type(*self)
            .diagnostics
            .iter()
            .for_each(|d| sink.push(d));

        if !matches!(
            self.value(db),
            Expr::Bool(_) | Expr::Num(_) | Expr::Str(_) | Expr::Unit
        ) {
            sink.push(&errors::error(
                "non-literal expressions not yet supported for constants",
                self.data(db).ast.kind.value.span,
                "not a literal",
            ))
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub enum TypeDef {
    Alias(TypeAliasId),
    Struct(StructId),
    Contract(ContractId),
    Primitive(types::Base),
}
impl TypeDef {
    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        match self {
            TypeDef::Alias(id) => id.name(db),
            TypeDef::Struct(id) => id.name(db),
            TypeDef::Contract(id) => id.name(db),
            TypeDef::Primitive(typ) => typ.name(),
        }
    }

    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        match self {
            TypeDef::Alias(id) => Some(id.name_span(db)),
            TypeDef::Struct(id) => Some(id.name_span(db)),
            TypeDef::Contract(id) => Some(id.name_span(db)),
            TypeDef::Primitive(_) => None,
        }
    }

    pub fn typ(&self, db: &dyn AnalyzerDb) -> Result<types::Type, TypeError> {
        match self {
            TypeDef::Alias(id) => id.typ(db),
            TypeDef::Struct(id) => Ok(types::Type::Struct(types::Struct {
                id: *id,
                name: id.name(db),
                field_count: id.fields(db).len(), // for the EvmSized trait
            })),
            TypeDef::Contract(id) => Ok(types::Type::Contract(types::Contract {
                id: *id,
                name: id.name(db),
            })),
            TypeDef::Primitive(base) => Ok(types::Type::Base(*base)),
        }
    }

    pub fn parent(&self, db: &dyn AnalyzerDb) -> Option<Item> {
        match self {
            TypeDef::Alias(id) => Some(id.parent(db)),
            TypeDef::Struct(id) => Some(id.parent(db)),
            TypeDef::Contract(id) => Some(id.parent(db)),
            TypeDef::Primitive(_) => None,
        }
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        match self {
            TypeDef::Alias(id) => id.sink_diagnostics(db, sink),
            TypeDef::Struct(id) => id.sink_diagnostics(db, sink),
            TypeDef::Contract(id) => id.sink_diagnostics(db, sink),
            TypeDef::Primitive(_) => {}
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct TypeAlias {
    pub ast: Node<ast::TypeAlias>,
    pub module: ModuleId,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct TypeAliasId(pub(crate) u32);
impl_intern_key!(TypeAliasId);

impl TypeAliasId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<TypeAlias> {
        db.lookup_intern_type_alias(*self)
    }
    pub fn span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.span
    }
    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).ast.name().into()
    }
    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.kind.name.span
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Result<types::Type, TypeError> {
        db.type_alias_type(*self).value
    }
    pub fn parent(&self, db: &dyn AnalyzerDb) -> Item {
        Item::Module(self.data(db).module)
    }
    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        db.type_alias_type(*self)
            .diagnostics
            .iter()
            .for_each(|d| sink.push(d))
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Contract {
    pub name: SmolStr,
    pub ast: Node<ast::Contract>,
    pub module: ModuleId,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct ContractId(pub(crate) u32);
impl_intern_key!(ContractId);
impl ContractId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Contract> {
        db.lookup_intern_contract(*self)
    }
    pub fn span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.span
    }
    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).ast.name().into()
    }
    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.kind.name.span
    }

    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).module
    }

    pub fn fields(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, ContractFieldId>> {
        db.contract_field_map(*self).value
    }

    pub fn field_type(
        &self,
        db: &dyn AnalyzerDb,
        name: &str,
    ) -> Option<(Result<types::Type, TypeError>, usize)> {
        let fields = db.contract_field_map(*self).value;
        let (index, _, field) = fields.get_full(name)?;
        Some((field.typ(db), index))
    }

    pub fn resolve_name(&self, db: &dyn AnalyzerDb, name: &str) -> Option<Item> {
        self.function(db, name)
            .filter(|f| !f.takes_self(db))
            .map(Item::Function)
            .or_else(|| self.event(db, name).map(Item::Event))
            .or_else(|| self.module(db).resolve_name(db, name))
    }

    pub fn init_function(&self, db: &dyn AnalyzerDb) -> Option<FunctionId> {
        db.contract_init_function(*self).value
    }

    /// User functions, public and not. Excludes `__init__`.
    pub fn functions(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, FunctionId>> {
        db.contract_function_map(*self).value
    }

    /// Lookup a function by name. Searches all user functions, private or not. Excludes init function.
    pub fn function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionId> {
        self.functions(db).get(name).copied()
    }

    /// Excludes `__init__`.
    pub fn public_functions(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, FunctionId>> {
        db.contract_public_function_map(*self)
    }

    /// Get a function that takes self by its name.
    pub fn self_function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionId> {
        self.function(db, name).filter(|f| f.takes_self(db))
    }

    /// Lookup an event by name.
    pub fn event(&self, db: &dyn AnalyzerDb, name: &str) -> Option<EventId> {
        self.events(db).get(name).copied()
    }

    /// A map of events defined within the contract.
    pub fn events(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, EventId>> {
        db.contract_event_map(*self).value
    }

    pub fn parent(&self, db: &dyn AnalyzerDb) -> Item {
        Item::Module(self.data(db).module)
    }

    /// Dependency graph of the contract type, which consists of the field types
    /// and the dependencies of those types.
    ///
    /// NOTE: Contract items should *only*
    pub fn dependency_graph(&self, db: &dyn AnalyzerDb) -> Rc<DepGraph> {
        db.contract_dependency_graph(*self).0
    }

    /// Dependency graph of the (imaginary) `__call__` function, which
    /// dispatches to the contract's public functions.
    pub fn runtime_dependency_graph(&self, db: &dyn AnalyzerDb) -> Rc<DepGraph> {
        db.contract_runtime_dependency_graph(*self).0
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        // fields
        db.contract_field_map(*self).sink_diagnostics(sink);
        db.contract_all_fields(*self)
            .iter()
            .for_each(|field| field.sink_diagnostics(db, sink));

        // events
        db.contract_event_map(*self).sink_diagnostics(sink);
        db.contract_all_events(*self)
            .iter()
            .for_each(|event| event.sink_diagnostics(db, sink));

        // functions
        db.contract_init_function(*self).sink_diagnostics(sink);
        db.contract_function_map(*self).sink_diagnostics(sink);
        db.contract_all_functions(*self)
            .iter()
            .for_each(|id| id.sink_diagnostics(db, sink));
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct ContractField {
    pub ast: Node<ast::Field>,
    pub parent: ContractId,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct ContractFieldId(pub(crate) u32);
impl_intern_key!(ContractFieldId);
impl ContractFieldId {
    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).ast.name().into()
    }
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<ContractField> {
        db.lookup_intern_contract_field(*self)
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Result<types::Type, TypeError> {
        db.contract_field_type(*self).value
    }
    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        sink.push_all(db.contract_field_type(*self).diagnostics.iter())
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Function {
    pub ast: Node<ast::Function>,
    pub module: ModuleId,
    pub parent: Option<Class>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct FunctionId(pub(crate) u32);
impl_intern_key!(FunctionId);
impl FunctionId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Function> {
        db.lookup_intern_function(*self)
    }
    pub fn span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.span
    }
    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).ast.name().into()
    }
    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.kind.name.span
    }

    // This should probably be scrapped in favor of `parent()`
    pub fn class(&self, db: &dyn AnalyzerDb) -> Option<Class> {
        self.data(db).parent
    }
    pub fn parent(&self, db: &dyn AnalyzerDb) -> Item {
        let data = self.data(db);
        data.parent
            .map(|class| class.as_item())
            .unwrap_or(Item::Module(data.module))
    }

    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).module
    }

    pub fn takes_self(&self, db: &dyn AnalyzerDb) -> bool {
        self.signature(db).self_decl.is_some()
    }
    pub fn self_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        if self.takes_self(db) {
            self.data(db)
                .ast
                .kind
                .args
                .iter()
                .find_map(|arg| matches!(arg.kind, ast::FunctionArg::Zelf).then(|| arg.span))
        } else {
            None
        }
    }

    pub fn is_public(&self, db: &dyn AnalyzerDb) -> bool {
        self.pub_span(db).is_some()
    }
    pub fn is_constructor(&self, db: &dyn AnalyzerDb) -> bool {
        self.name(db) == "__init__"
    }
    pub fn pub_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        self.data(db).ast.kind.pub_
    }
    pub fn is_unsafe(&self, db: &dyn AnalyzerDb) -> bool {
        self.unsafe_span(db).is_some()
    }
    pub fn unsafe_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        self.data(db).ast.kind.unsafe_
    }
    pub fn signature(&self, db: &dyn AnalyzerDb) -> Rc<types::FunctionSignature> {
        db.function_signature(*self).value
    }
    pub fn body(&self, db: &dyn AnalyzerDb) -> Rc<context::FunctionBody> {
        db.function_body(*self).value
    }
    pub fn dependency_graph(&self, db: &dyn AnalyzerDb) -> Rc<DepGraph> {
        db.function_dependency_graph(*self).0
    }
    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        sink.push_all(db.function_signature(*self).diagnostics.iter());
        sink.push_all(db.function_body(*self).diagnostics.iter());
    }
}

/// A `Class` is an item that can have member functions.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Class {
    Contract(ContractId),
    Struct(StructId),
}
impl Class {
    pub fn function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionId> {
        match self {
            Class::Contract(id) => id.function(db, name),
            Class::Struct(id) => id.function(db, name),
        }
    }
    pub fn self_function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionId> {
        let fun = self.function(db, name)?;
        fun.takes_self(db).then(|| fun)
    }

    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        match self {
            Class::Contract(inner) => inner.name(db),
            Class::Struct(inner) => inner.name(db),
        }
    }
    pub fn kind(&self) -> &str {
        match self {
            Class::Contract(_) => "contract",
            Class::Struct(_) => "struct",
        }
    }
    pub fn as_item(&self) -> Item {
        match self {
            Class::Contract(id) => Item::Type(TypeDef::Contract(*id)),
            Class::Struct(id) => Item::Type(TypeDef::Struct(*id)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum MemberFunction {
    BuiltIn(builtins::ValueMethod),
    Function(FunctionId),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Struct {
    pub ast: Node<ast::Struct>,
    pub module: ModuleId,
}

#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct StructId(pub(crate) u32);
impl_intern_key!(StructId);
impl StructId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Struct> {
        db.lookup_intern_struct(*self)
    }
    pub fn span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.span
    }
    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).ast.name().into()
    }
    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.kind.name.span
    }
    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).module
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<types::Struct> {
        db.struct_type(*self)
    }

    pub fn has_private_field(&self, db: &dyn AnalyzerDb) -> bool {
        self.private_fields(db).iter().count() > 0
    }

    pub fn field(&self, db: &dyn AnalyzerDb, name: &str) -> Option<StructFieldId> {
        self.fields(db).get(name).copied()
    }
    pub fn field_type(
        &self,
        db: &dyn AnalyzerDb,
        name: &str,
    ) -> Option<Result<types::FixedSize, TypeError>> {
        Some(self.field(db, name)?.typ(db))
    }

    pub fn fields(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, StructFieldId>> {
        db.struct_field_map(*self).value
    }
    pub fn functions(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, FunctionId>> {
        db.struct_function_map(*self).value
    }
    pub fn function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionId> {
        self.functions(db).get(name).copied()
    }
    pub fn self_function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionId> {
        self.function(db, name).filter(|f| f.takes_self(db))
    }
    pub fn parent(&self, db: &dyn AnalyzerDb) -> Item {
        Item::Module(self.data(db).module)
    }
    pub fn private_fields(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, StructFieldId>> {
        Rc::new(
            self.fields(db)
                .iter()
                .filter_map(|(name, field)| {
                    if field.is_public(db) {
                        None
                    } else {
                        Some((name.clone(), *field))
                    }
                })
                .collect(),
        )
    }
    pub fn dependency_graph(&self, db: &dyn AnalyzerDb) -> Rc<DepGraph> {
        db.struct_dependency_graph(*self).0
    }
    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        sink.push_all(db.struct_field_map(*self).diagnostics.iter());
        db.struct_all_fields(*self)
            .iter()
            .for_each(|id| id.sink_diagnostics(db, sink));

        db.struct_all_functions(*self)
            .iter()
            .for_each(|id| id.sink_diagnostics(db, sink));
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct StructField {
    pub ast: Node<ast::Field>,
    pub parent: StructId,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct StructFieldId(pub(crate) u32);
impl_intern_key!(StructFieldId);
impl StructFieldId {
    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).ast.name().into()
    }
    pub fn span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.span
    }
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<StructField> {
        db.lookup_intern_struct_field(*self)
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Result<types::FixedSize, TypeError> {
        db.struct_field_type(*self).value
    }
    pub fn is_public(&self, db: &dyn AnalyzerDb) -> bool {
        self.data(db).ast.kind.is_pub
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        db.struct_field_type(*self).sink_diagnostics(sink)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Event {
    pub ast: Node<ast::Event>,
    pub contract: ContractId,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct EventId(pub(crate) u32);
impl_intern_key!(EventId);

impl EventId {
    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).ast.name().into()
    }
    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.kind.name.span
    }
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Event> {
        db.lookup_intern_event(*self)
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<types::Event> {
        db.event_type(*self).value
    }
    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).contract.module(db)
    }
    pub fn parent(&self, db: &dyn AnalyzerDb) -> Item {
        Item::Type(TypeDef::Contract(self.data(db).contract))
    }
    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        sink.push_all(db.event_type(*self).diagnostics.iter());
    }
}

pub trait DiagnosticSink {
    fn push(&mut self, diag: &Diagnostic);
    fn push_all<'a>(&mut self, iter: impl Iterator<Item = &'a Diagnostic>) {
        iter.for_each(|diag| self.push(diag))
    }
}

impl DiagnosticSink for Vec<Diagnostic> {
    fn push(&mut self, diag: &Diagnostic) {
        self.push(diag.clone())
    }
    fn push_all<'a>(&mut self, iter: impl Iterator<Item = &'a Diagnostic>) {
        self.extend(iter.cloned())
    }
}

pub type DepGraph = petgraph::graphmap::DiGraphMap<Item, DepLocality>;
#[derive(Debug, Clone)]
pub struct DepGraphWrapper(pub Rc<DepGraph>);
impl PartialEq for DepGraphWrapper {
    fn eq(&self, other: &DepGraphWrapper) -> bool {
        self.0.all_edges().eq(other.0.all_edges()) && self.0.nodes().eq(other.0.nodes())
    }
}
impl Eq for DepGraphWrapper {}

/// [`DepGraph`] edge label. "Locality" refers to the deployed state;
/// `Local` dependencies are those that will be compiled together, while
/// `External` dependencies will only be reachable via an evm CALL* op.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DepLocality {
    Local,
    External,
}

pub fn walk_local_dependencies<F>(graph: &DepGraph, root: Item, mut fun: F)
where
    F: FnMut(Item),
{
    use petgraph::visit::{Bfs, EdgeFiltered};

    let mut bfs = Bfs::new(
        &EdgeFiltered::from_fn(graph, |(_, _, loc)| *loc == DepLocality::Local),
        root,
    );
    while let Some(node) = bfs.next(&graph) {
        fun(node)
    }
}
