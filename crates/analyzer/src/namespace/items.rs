use crate::context::{self, Analysis, Constant};
use crate::display::Displayable;
use crate::errors::{self, IncompleteItem, TypeError};
use crate::namespace::types::{self, GenericType, Type, TypeId};
use crate::traversal::pragma::check_pragma_version;
use crate::AnalyzerDb;
use crate::{builtins, errors::ConstEvalError};
use fe_common::diagnostics::Diagnostic;
use fe_common::diagnostics::Label;
use fe_common::files::{common_prefix, Utf8Path};
use fe_common::{impl_intern_key, FileKind, SourceFileId};
use fe_parser::ast::GenericParameter;
use fe_parser::node::{Node, Span};
use fe_parser::{ast, node::NodeId};
use indexmap::{indexmap, IndexMap};
use smol_str::SmolStr;
use std::ops::Deref;
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
    Trait(TraitId),
    Impl(ImplId),
    Function(FunctionId),
    Constant(ModuleConstantId),
    // Needed until we can represent keccak256 as a FunctionId.
    // We can't represent keccak256's arg type yet.
    BuiltinFunction(builtins::GlobalFunction),
    Intrinsic(builtins::Intrinsic),
}

impl Item {
    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        match self {
            Item::Type(id) => id.name(db),
            Item::Trait(id) => id.name(db),
            Item::Impl(id) => id.name(db),
            Item::GenericType(id) => id.name(),
            Item::Event(id) => id.name(db),
            Item::Function(id) => id.name(db),
            Item::BuiltinFunction(id) => id.as_ref().into(),
            Item::Intrinsic(id) => id.as_ref().into(),
            Item::Constant(id) => id.name(db),
            Item::Ingot(id) => id.name(db),
            Item::Module(id) => id.name(db),
        }
    }

    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        match self {
            Item::Type(id) => id.name_span(db),
            Item::Trait(id) => Some(id.name_span(db)),
            Item::GenericType(_) => None,
            Item::Event(id) => Some(id.name_span(db)),
            Item::Function(id) => Some(id.name_span(db)),
            Item::Constant(id) => Some(id.name_span(db)),
            Item::BuiltinFunction(_)
            | Item::Intrinsic(_)
            | Item::Ingot(_)
            | Item::Module(_)
            | Item::Impl(_) => None,
        }
    }

    pub fn is_public(&self, db: &dyn AnalyzerDb) -> bool {
        match self {
            // TODO: Consider whether to allow `pub module`.
            Self::Ingot(_)
            | Self::Module(_)
            | Self::BuiltinFunction(_)
            | Self::Intrinsic(_)
            | Self::Impl(_)
            | Self::GenericType(_) => true,
            Self::Type(id) => id.is_public(db),
            Self::Trait(id) => id.is_public(db),
            Self::Event(id) => id.is_public(db),
            Self::Function(id) => id.is_public(db),
            Self::Constant(id) => id.is_public(db),
        }
    }

    pub fn is_builtin(&self) -> bool {
        match self {
            Item::Type(TypeDef::Primitive(_))
            | Item::GenericType(_)
            | Item::BuiltinFunction(_)
            | Item::Intrinsic(_) => true,
            Item::Type(_)
            | Item::Trait(_)
            | Item::Impl(_)
            | Item::Event(_)
            | Item::Function(_)
            | Item::Constant(_)
            | Item::Ingot(_)
            | Item::Module(_) => false,
        }
    }

    pub fn is_struct(&self, val: &StructId) -> bool {
        matches!(self, Item::Type(TypeDef::Struct(current)) if current == val)
    }

    pub fn is_contract(&self) -> bool {
        matches!(self, Item::Type(TypeDef::Contract(_)))
    }

    pub fn item_kind_display_name(&self) -> &'static str {
        match self {
            Item::Type(TypeDef::Struct(_)) => "struct",
            Item::Type(_) | Item::GenericType(_) => "type",
            Item::Trait(_) => "trait",
            Item::Impl(_) => "impl",
            Item::Event(_) => "event",
            Item::Function(_) | Item::BuiltinFunction(_) => "function",
            Item::Intrinsic(_) => "intrinsic function",
            Item::Constant(_) => "constant",
            Item::Ingot(_) => "ingot",
            Item::Module(_) => "module",
        }
    }

    pub fn items(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, Item>> {
        match self {
            Item::Ingot(ingot) => ingot.items(db),
            Item::Module(module) => module.items(db),
            Item::Type(val) => val.items(db),
            Item::GenericType(_)
            | Item::Event(_)
            | Item::Trait(_)
            | Item::Impl(_)
            | Item::Function(_)
            | Item::Constant(_)
            | Item::BuiltinFunction(_)
            | Item::Intrinsic(_) => Rc::new(indexmap! {}),
        }
    }

    pub fn parent(&self, db: &dyn AnalyzerDb) -> Option<Item> {
        match self {
            Item::Type(id) => id.parent(db),
            Item::Trait(id) => Some(id.parent(db)),
            Item::Impl(id) => Some(id.parent(db)),
            Item::GenericType(_) => None,
            Item::Event(id) => Some(id.parent(db)),
            Item::Function(id) => Some(id.parent(db)),
            Item::Constant(id) => Some(id.parent(db)),
            Item::Module(id) => Some(id.parent(db)),
            Item::BuiltinFunction(_) | Item::Intrinsic(_) | Item::Ingot(_) => None,
        }
    }

    pub fn module(&self, db: &dyn AnalyzerDb) -> Option<ModuleId> {
        if let Self::Module(id) = self {
            return Some(*id);
        }

        let mut cur_item = *self;
        while let Some(item) = cur_item.parent(db) {
            if let Self::Module(id) = item {
                return Some(id);
            }
            cur_item = item;
        }

        None
    }

    pub fn path(&self, db: &dyn AnalyzerDb) -> Rc<[SmolStr]> {
        // The path is used to generate a yul identifier,
        // eg `foo::Bar::new` becomes `$$foo$Bar$new`.
        // Right now, the ingot name is the os path, so it could
        // be "my project/src".
        // For now, we'll just leave the ingot out of the path,
        // because we can only compile a single ingot anyway.
        match self.parent(db) {
            Some(Item::Ingot(_)) | None => [self.name(db)][..].into(),
            Some(parent) => {
                let mut path = parent.path(db).to_vec();
                path.push(self.name(db));
                path.into()
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
                        diagnostics: Rc::new([errors::error(
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
            diagnostics: Rc::new([]),
        }
    }

    pub fn function_sig(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionSigId> {
        match self {
            Item::Type(TypeDef::Contract(id)) => id.function(db, name).map(|fun| fun.sig(db)),
            Item::Type(TypeDef::Struct(id)) => id.function(db, name).map(|fun| fun.sig(db)),
            Item::Impl(id) => id.function(db, name).map(|fun| fun.sig(db)),
            Item::Trait(id) => id.function(db, name),
            _ => None,
        }
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        match self {
            Item::Type(id) => id.sink_diagnostics(db, sink),
            Item::Trait(id) => id.sink_diagnostics(db, sink),
            Item::Impl(id) => id.sink_diagnostics(db, sink),
            Item::Event(id) => id.sink_diagnostics(db, sink),
            Item::Function(id) => id.sink_diagnostics(db, sink),
            Item::GenericType(_) | Item::BuiltinFunction(_) | Item::Intrinsic(_) => {}
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
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum IngotMode {
    /// The target of compilation. Expected to have a main.fe file.
    Main,
    /// A library; expected to have a lib.fe file.
    Lib,
    /// A fake ingot, created to hold a single module with any filename.
    StandaloneModule,
}

/// An `Ingot` is composed of a tree of `Module`s (set via
/// [`AnalyzerDb::set_ingot_module_tree`]), and doesn't have direct knowledge of
/// files.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Ingot {
    pub name: SmolStr,
    // pub version: SmolStr,
    pub mode: IngotMode,
    pub src_dir: SmolStr,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct IngotId(pub(crate) u32);
impl_intern_key!(IngotId);
impl IngotId {
    pub fn std_lib(db: &mut dyn AnalyzerDb) -> Self {
        IngotId::from_files(
            db,
            "std",
            IngotMode::Lib,
            FileKind::Std,
            &fe_library::std_src_files(),
            indexmap!(),
        )
    }

    pub fn from_files(
        db: &mut dyn AnalyzerDb,
        name: &str,
        mode: IngotMode,
        file_kind: FileKind,
        files: &[(impl AsRef<str>, impl AsRef<str>)],
        deps: IndexMap<SmolStr, IngotId>,
    ) -> Self {
        // The common prefix of all file paths will be stored as the ingot
        // src dir path, and all module file paths will be considered to be
        // relative to this prefix.
        let file_path_prefix = if files.len() == 1 {
            // If there's only one source file, the "common prefix" is everything
            // before the file name.
            Utf8Path::new(files[0].0.as_ref())
                .parent()
                .unwrap_or_else(|| "".into())
                .to_path_buf()
        } else {
            files
                .iter()
                .map(|(path, _)| Utf8Path::new(path).to_path_buf())
                .reduce(|pref, path| common_prefix(&pref, &path))
                .expect("`IngotId::from_files`: empty file list")
        };

        let ingot = db.intern_ingot(Rc::new(Ingot {
            name: name.into(),
            mode,
            src_dir: file_path_prefix.as_str().into(),
        }));

        // Intern the source files
        let file_ids = files
            .iter()
            .map(|(path, content)| {
                SourceFileId::new(
                    db.upcast_mut(),
                    file_kind,
                    path.as_ref(),
                    content.as_ref().into(),
                )
            })
            .collect();

        db.set_root_ingot(ingot);
        db.set_ingot_files(ingot, file_ids);
        db.set_ingot_external_ingots(ingot, Rc::new(deps));
        ingot
    }

    pub fn external_ingots(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, IngotId>> {
        db.ingot_external_ingots(*self)
    }

    pub fn all_modules(&self, db: &dyn AnalyzerDb) -> Rc<[ModuleId]> {
        db.ingot_modules(*self)
    }

    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Ingot> {
        db.lookup_intern_ingot(*self)
    }

    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).name.clone()
    }

    /// Returns the `main.fe`, or `lib.fe` module, depending on the ingot "mode"
    /// (IngotMode).
    pub fn root_module(&self, db: &dyn AnalyzerDb) -> Option<ModuleId> {
        db.ingot_root_module(*self)
    }

    pub fn items(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, Item>> {
        self.root_module(db).expect("missing root module").items(db)
    }

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];
        self.sink_diagnostics(db, &mut diagnostics);
        diagnostics
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        if self.root_module(db).is_none() {
            let file_name = match self.data(db).mode {
                IngotMode::Lib => "lib",
                IngotMode::Main => "main",
                IngotMode::StandaloneModule => unreachable!(), // always has a root module
            };
            sink.push(&Diagnostic::error(format!(
                "The ingot named \"{}\" is missing a `{}` module. \
                 \nPlease add a `src/{}.fe` file to the base directory.",
                self.name(db),
                file_name,
                file_name,
            )));
        }
        for module in self.all_modules(db).iter() {
            module.sink_diagnostics(db, sink)
        }
    }

    pub fn sink_external_ingot_diagnostics(
        &self,
        db: &dyn AnalyzerDb,
        sink: &mut impl DiagnosticSink,
    ) {
        for ingot in self.external_ingots(db).values() {
            ingot.sink_diagnostics(db, sink)
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum ModuleSource {
    File(SourceFileId),
    /// For directory modules without a corresponding source file
    /// (which will soon not be allowed, and this variant can go away).
    Dir(SmolStr),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Module {
    pub name: SmolStr,
    pub ingot: IngotId,
    pub source: ModuleSource,
}

/// Id of a [`Module`], which corresponds to a single Fe source file.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct ModuleId(pub(crate) u32);
impl_intern_key!(ModuleId);
impl ModuleId {
    pub fn new_standalone(db: &mut dyn AnalyzerDb, path: &str, content: &str) -> Self {
        let std = IngotId::std_lib(db);
        let ingot = IngotId::from_files(
            db,
            "",
            IngotMode::StandaloneModule,
            FileKind::Local,
            &[(path, content)],
            indexmap! { "std".into() => std },
        );

        ingot
            .root_module(db)
            .expect("ModuleId::new_standalone ingot has no root module")
    }

    pub fn new(db: &dyn AnalyzerDb, name: &str, source: ModuleSource, ingot: IngotId) -> Self {
        db.intern_module(
            Module {
                name: name.into(),
                ingot,
                source,
            }
            .into(),
        )
    }

    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Module> {
        db.lookup_intern_module(*self)
    }

    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).name.clone()
    }

    pub fn file_path_relative_to_src_dir(&self, db: &dyn AnalyzerDb) -> SmolStr {
        db.module_file_path(*self)
    }

    pub fn ast(&self, db: &dyn AnalyzerDb) -> Rc<ast::Module> {
        db.module_parse(*self).value
    }

    pub fn ingot(&self, db: &dyn AnalyzerDb) -> IngotId {
        self.data(db).ingot
    }

    pub fn is_incomplete(&self, db: &dyn AnalyzerDb) -> bool {
        db.module_is_incomplete(*self)
    }

    pub fn is_in_std(&self, db: &dyn AnalyzerDb) -> bool {
        self.ingot(db).name(db) == "std"
    }

    /// Includes duplicate names
    pub fn all_items(&self, db: &dyn AnalyzerDb) -> Rc<[Item]> {
        db.module_all_items(*self)
    }

    /// Includes duplicate names
    pub fn all_impls(&self, db: &dyn AnalyzerDb) -> Rc<[ImplId]> {
        db.module_all_impls(*self)
    }

    pub fn impls(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<(TraitId, TypeId), ImplId>> {
        db.module_impl_map(*self).value
    }

    /// Returns a map of the named items in the module
    pub fn items(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, Item>> {
        db.module_item_map(*self).value
    }

    /// Returns a `name -> (name_span, external_item)` map for all `use`
    /// statements in a module.
    pub fn used_items(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, (Span, Item)>> {
        db.module_used_item_map(*self).value
    }

    /// Returns `true` if the `item` is in scope of the module.
    pub fn is_in_scope(&self, db: &dyn AnalyzerDb, item: Item) -> bool {
        if let Some(val) = item.module(db) {
            if val == *self {
                return true;
            }
        }

        if let Some((_, val)) = self.used_items(db).get(&item.name(db)) {
            if *val == item {
                return true;
            }
        }
        false
    }

    /// Returns all of the internal items, except for `use`d items. This is used
    /// when resolving `use` statements, as it does not create a query
    /// cycle.
    pub fn non_used_internal_items(&self, db: &dyn AnalyzerDb) -> IndexMap<SmolStr, Item> {
        let global_items = self.global_items(db);

        self.submodules(db)
            .iter()
            .map(|module| (module.name(db), Item::Module(*module)))
            .chain(global_items)
            .collect()
    }

    /// Returns all of the internal items. Internal items refers to the set of
    /// items visible when inside of a module.
    pub fn internal_items(&self, db: &dyn AnalyzerDb) -> IndexMap<SmolStr, Item> {
        let global_items = self.global_items(db);
        let defined_items = self.items(db);
        self.submodules(db)
            .iter()
            .map(|module| (module.name(db), Item::Module(*module)))
            .chain(global_items)
            .chain(defined_items.deref().clone())
            .collect()
    }

    /// Resolve a path that starts with an item defined in the module.
    pub fn resolve_path(&self, db: &dyn AnalyzerDb, path: &ast::Path) -> Analysis<Option<Item>> {
        Item::Module(*self).resolve_path_segments(db, &path.segments)
    }

    /// Resolve a path that starts with an internal item. We omit used items to
    /// avoid a query cycle.
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
                diagnostics: Rc::new([errors::error(
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
                diagnostics: Rc::new([errors::error(
                    "unresolved path item",
                    first_segment.span,
                    "not found",
                )]),
            }
        }
    }

    /// Returns `Err(IncompleteItem)` if the name could not be resolved, and the
    /// module was not completely parsed (due to a syntax error).
    pub fn resolve_name(
        &self,
        db: &dyn AnalyzerDb,
        name: &str,
    ) -> Result<Option<Item>, IncompleteItem> {
        if let Some(thing) = self.internal_items(db).get(name) {
            Ok(Some(*thing))
        } else if self.is_incomplete(db) {
            Err(IncompleteItem::new())
        } else {
            Ok(None)
        }
    }

    pub fn resolve_constant(
        &self,
        db: &dyn AnalyzerDb,
        name: &str,
    ) -> Result<Option<ModuleConstantId>, IncompleteItem> {
        if let Some(constant) = self
            .all_constants(db)
            .iter()
            .find(|id| id.name(db) == name)
            .copied()
        {
            Ok(Some(constant))
        } else if self.is_incomplete(db) {
            Err(IncompleteItem::new())
        } else {
            Ok(None)
        }
    }
    pub fn submodules(&self, db: &dyn AnalyzerDb) -> Rc<[ModuleId]> {
        db.module_submodules(*self)
    }

    pub fn parent(&self, db: &dyn AnalyzerDb) -> Item {
        self.parent_module(db)
            .map(Item::Module)
            .unwrap_or_else(|| Item::Ingot(self.ingot(db)))
    }

    pub fn parent_module(&self, db: &dyn AnalyzerDb) -> Option<ModuleId> {
        db.module_parent_module(*self)
    }

    /// All contracts, including from submodules and including duplicates
    pub fn all_contracts(&self, db: &dyn AnalyzerDb) -> Vec<ContractId> {
        self.submodules(db)
            .iter()
            .flat_map(|module| module.all_contracts(db))
            .chain((*db.module_contracts(*self)).to_vec())
            .collect::<Vec<_>>()
    }

    /// Returns the map of ingot deps, built-ins, and the ingot itself as
    /// "ingot".
    pub fn global_items(&self, db: &dyn AnalyzerDb) -> IndexMap<SmolStr, Item> {
        let ingot = self.ingot(db);
        let mut items = ingot
            .external_ingots(db)
            .iter()
            .map(|(name, ingot)| (name.clone(), Item::Ingot(*ingot)))
            .chain(std_prelude_items())
            .collect::<IndexMap<_, _>>();

        if ingot.data(db).mode != IngotMode::StandaloneModule {
            items.insert("ingot".into(), Item::Ingot(ingot));
        }
        items
    }

    /// All module constants.
    pub fn all_constants(&self, db: &dyn AnalyzerDb) -> Rc<Vec<ModuleConstantId>> {
        db.module_constants(*self)
    }

    pub fn diagnostics(&self, db: &dyn AnalyzerDb) -> Vec<Diagnostic> {
        let mut diagnostics = vec![];
        self.sink_diagnostics(db, &mut diagnostics);
        diagnostics
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        let data = self.data(db);
        if let ModuleSource::File(_) = data.source {
            sink.push_all(db.module_parse(*self).diagnostics.iter())
        }
        let ast = self.ast(db);
        for stmt in &ast.body {
            if let ast::ModuleStmt::Pragma(inner) = stmt {
                if let Some(diag) = check_pragma_version(inner) {
                    sink.push(&diag)
                }
            }
        }

        // duplicate item name errors
        sink.push_all(db.module_item_map(*self).diagnostics.iter());

        // duplicate impl errors
        sink.push_all(db.module_impl_map(*self).diagnostics.iter());

        // errors for each item
        self.all_items(db)
            .iter()
            .for_each(|id| id.sink_diagnostics(db, sink));

        self.all_impls(db)
            .iter()
            .for_each(|id| id.sink_diagnostics(db, sink));
    }

    #[doc(hidden)]
    // DO NOT USE THIS METHOD except for testing purpose.
    pub fn from_raw_internal(raw: u32) -> Self {
        Self(raw)
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
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Result<types::TypeId, TypeError> {
        db.module_constant_type(*self).value
    }

    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).ast.kind.name.kind.clone()
    }

    pub fn constant_value(&self, db: &dyn AnalyzerDb) -> Result<Constant, ConstEvalError> {
        db.module_constant_value(*self).value
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

    pub fn is_public(&self, db: &dyn AnalyzerDb) -> bool {
        self.data(db).ast.kind.pub_qual.is_some()
    }

    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).module
    }

    pub fn node_id(&self, db: &dyn AnalyzerDb) -> NodeId {
        self.data(db).ast.id
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        db.module_constant_type(*self)
            .diagnostics
            .iter()
            .for_each(|d| sink.push(d));
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
    pub fn items(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, Item>> {
        match self {
            TypeDef::Struct(val) => {
                Rc::new(
                    val.functions(db)
                        .iter()
                        .filter_map(|(name, field)| {
                            if field.takes_self(db) {
                                // In the future we probably want to resolve instance methods as well. But this would require
                                // the caller to pass an instance as the first argument e.g. `Rectangle::can_hold(self_instance, other)`.
                                // This isn't yet supported so for now path access to functions is limited to static functions only.
                                None
                            } else {
                                Some((name.to_owned(), Item::Function(*field)))
                            }
                        })
                        .collect(),
                )
            }
            _ => todo!("cannot access items in types yet"),
        }
    }

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

    pub fn typ(&self, db: &dyn AnalyzerDb) -> Result<Type, TypeError> {
        match self {
            TypeDef::Alias(id) => Ok(id.type_id(db)?.typ(db)),
            TypeDef::Struct(id) => Ok(Type::Struct(*id)),
            TypeDef::Contract(id) => Ok(Type::Contract(*id)),
            TypeDef::Primitive(base) => Ok(Type::Base(*base)),
        }
    }

    pub fn type_id(&self, db: &dyn AnalyzerDb) -> Result<TypeId, TypeError> {
        Ok(db.intern_type(self.typ(db)?))
    }

    pub fn is_public(&self, db: &dyn AnalyzerDb) -> bool {
        match self {
            Self::Alias(id) => id.is_public(db),
            Self::Struct(id) => id.is_public(db),
            Self::Contract(id) => id.is_public(db),
            Self::Primitive(_) => true,
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
    pub fn is_public(&self, db: &dyn AnalyzerDb) -> bool {
        self.data(db).ast.kind.pub_qual.is_some()
    }
    pub fn type_id(&self, db: &dyn AnalyzerDb) -> Result<types::TypeId, TypeError> {
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
    pub fn is_public(&self, db: &dyn AnalyzerDb) -> bool {
        self.data(db).ast.kind.pub_qual.is_some()
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
    ) -> Option<(Result<types::TypeId, TypeError>, usize)> {
        let fields = db.contract_field_map(*self).value;
        let (index, _, field) = fields.get_full(name)?;
        Some((field.typ(db), index))
    }

    pub fn resolve_name(
        &self,
        db: &dyn AnalyzerDb,
        name: &str,
    ) -> Result<Option<Item>, IncompleteItem> {
        if let Some(item) = self
            .function(db, name)
            .filter(|f| !f.takes_self(db))
            .map(Item::Function)
            .or_else(|| self.event(db, name).map(Item::Event))
        {
            Ok(Some(item))
        } else {
            self.module(db).resolve_name(db, name)
        }
    }

    pub fn init_function(&self, db: &dyn AnalyzerDb) -> Option<FunctionId> {
        db.contract_init_function(*self).value
    }

    pub fn call_function(&self, db: &dyn AnalyzerDb) -> Option<FunctionId> {
        db.contract_call_function(*self).value
    }

    pub fn all_functions(&self, db: &dyn AnalyzerDb) -> Rc<[FunctionId]> {
        db.contract_all_functions(*self)
    }

    /// User functions, public and not. Excludes `__init__` and `__call__`.
    pub fn functions(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, FunctionId>> {
        db.contract_function_map(*self).value
    }

    /// Lookup a function by name. Searches all user functions, private or not.
    /// Excludes `__init__` and `__call__`.
    pub fn function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionId> {
        self.functions(db).get(name).copied()
    }

    /// Excludes `__init__` and `__call__`.
    pub fn public_functions(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, FunctionId>> {
        db.contract_public_function_map(*self)
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
        db.contract_call_function(*self).sink_diagnostics(sink);
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
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Result<types::TypeId, TypeError> {
        db.contract_field_type(*self).value
    }
    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        sink.push_all(db.contract_field_type(*self).diagnostics.iter())
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct FunctionSig {
    pub ast: Node<ast::FunctionSignature>,
    pub parent: Option<Item>,
    pub module: ModuleId,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct FunctionSigId(pub(crate) u32);
impl_intern_key!(FunctionSigId);

impl FunctionSigId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<FunctionSig> {
        db.lookup_intern_function_sig(*self)
    }

    pub fn takes_self(&self, db: &dyn AnalyzerDb) -> bool {
        self.signature(db).self_decl.is_some()
    }

    pub fn self_type(&self, db: &dyn AnalyzerDb) -> Option<types::TypeId> {
        match self.parent(db) {
            Item::Type(TypeDef::Contract(cid)) => Some(types::Type::SelfContract(cid).id(db)),
            Item::Type(TypeDef::Struct(sid)) => Some(types::Type::Struct(sid).id(db)),
            Item::Impl(id) => Some(id.receiver(db)),
            Item::Type(TypeDef::Primitive(ty)) => Some(db.intern_type(Type::Base(ty))),
            _ => None,
        }
    }
    pub fn self_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        if self.takes_self(db) {
            self.data(db)
                .ast
                .kind
                .args
                .iter()
                .find_map(|arg| matches!(arg.kind, ast::FunctionArg::Self_).then(|| arg.span))
        } else {
            None
        }
    }
    pub fn signature(&self, db: &dyn AnalyzerDb) -> Rc<types::FunctionSignature> {
        db.function_signature(*self).value
    }

    pub fn is_public(&self, db: &dyn AnalyzerDb) -> bool {
        self.is_trait_fn(db) || self.is_impl_fn(db) || self.pub_span(db).is_some()
    }
    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).ast.kind.name.kind.clone()
    }
    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.kind.name.span
    }
    pub fn unsafe_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        self.data(db).ast.kind.unsafe_
    }
    pub fn is_constructor(&self, db: &dyn AnalyzerDb) -> bool {
        self.name(db) == "__init__"
    }
    pub fn pub_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        self.data(db).ast.kind.pub_
    }

    pub fn self_item(&self, db: &dyn AnalyzerDb) -> Option<Item> {
        let data = self.data(db);
        data.parent
    }

    pub fn parent(&self, db: &dyn AnalyzerDb) -> Item {
        self.self_item(db)
            .unwrap_or_else(|| Item::Module(self.module(db)))
    }

    /// Looks up the `FunctionId` based on the parent of the function signature
    pub fn function(&self, db: &dyn AnalyzerDb) -> Option<FunctionId> {
        match self.parent(db) {
            Item::Type(TypeDef::Struct(id)) => id.function(db, &self.name(db)),
            Item::Impl(id) => id.function(db, &self.name(db)),
            Item::Type(TypeDef::Contract(id)) => id.function(db, &self.name(db)),
            _ => None,
        }
    }

    pub fn is_trait_fn(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.parent(db), Item::Trait(_))
    }

    pub fn is_module_fn(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.parent(db), Item::Module(_))
    }

    pub fn is_impl_fn(&self, db: &dyn AnalyzerDb) -> bool {
        matches!(self.parent(db), Item::Impl(_))
    }

    pub fn is_generic(&self, db: &dyn AnalyzerDb) -> bool {
        !self.data(db).ast.kind.generic_params.kind.is_empty()
    }

    pub fn generic_params(&self, db: &dyn AnalyzerDb) -> Vec<GenericParameter> {
        self.data(db).ast.kind.generic_params.kind.clone()
    }

    pub fn generic_param(&self, db: &dyn AnalyzerDb, param_name: &str) -> Option<GenericParameter> {
        self.generic_params(db)
            .iter()
            .find(|param| match param {
                GenericParameter::Unbounded(name) if name.kind == param_name => true,
                GenericParameter::Bounded { name, .. } if name.kind == param_name => true,
                _ => false,
            })
            .cloned()
    }

    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).module
    }

    pub fn is_contract_func(self, db: &dyn AnalyzerDb) -> bool {
        matches! {self.parent(db), Item::Type(TypeDef::Contract(_))}
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        sink.push_all(db.function_signature(*self).diagnostics.iter());
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Function {
    pub ast: Node<ast::Function>,
    pub sig: FunctionSigId,
}

impl Function {
    pub fn new(
        db: &dyn AnalyzerDb,
        ast: &Node<ast::Function>,
        parent: Option<Item>,
        module: ModuleId,
    ) -> Self {
        let sig = db.intern_function_sig(Rc::new(FunctionSig {
            ast: ast.kind.sig.clone(),
            parent,
            module,
        }));
        Function {
            sig,
            ast: ast.clone(),
        }
    }
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
        self.sig(db).name(db)
    }
    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.sig(db).name_span(db)
    }
    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.sig(db).module(db)
    }
    pub fn self_type(&self, db: &dyn AnalyzerDb) -> Option<types::TypeId> {
        self.sig(db).self_type(db)
    }
    pub fn parent(&self, db: &dyn AnalyzerDb) -> Item {
        self.sig(db).parent(db)
    }

    pub fn takes_self(&self, db: &dyn AnalyzerDb) -> bool {
        self.sig(db).takes_self(db)
    }
    pub fn self_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        self.sig(db).self_span(db)
    }
    pub fn is_generic(&self, db: &dyn AnalyzerDb) -> bool {
        self.sig(db).is_generic(db)
    }
    pub fn is_public(&self, db: &dyn AnalyzerDb) -> bool {
        self.sig(db).is_public(db)
    }
    pub fn is_constructor(&self, db: &dyn AnalyzerDb) -> bool {
        self.sig(db).is_constructor(db)
    }
    pub fn is_unsafe(&self, db: &dyn AnalyzerDb) -> bool {
        self.unsafe_span(db).is_some()
    }
    pub fn unsafe_span(&self, db: &dyn AnalyzerDb) -> Option<Span> {
        self.sig(db).unsafe_span(db)
    }
    pub fn signature(&self, db: &dyn AnalyzerDb) -> Rc<types::FunctionSignature> {
        db.function_signature(self.data(db).sig).value
    }
    pub fn sig(&self, db: &dyn AnalyzerDb) -> FunctionSigId {
        self.data(db).sig
    }
    pub fn body(&self, db: &dyn AnalyzerDb) -> Rc<context::FunctionBody> {
        db.function_body(*self).value
    }
    pub fn dependency_graph(&self, db: &dyn AnalyzerDb) -> Rc<DepGraph> {
        db.function_dependency_graph(*self).0
    }
    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        sink.push_all(db.function_signature(self.data(db).sig).diagnostics.iter());
        sink.push_all(db.function_body(*self).diagnostics.iter());
    }
    pub fn is_contract_func(self, db: &dyn AnalyzerDb) -> bool {
        self.sig(db).is_contract_func(db)
    }
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

    pub fn is_public(&self, db: &dyn AnalyzerDb) -> bool {
        self.data(db).ast.kind.pub_qual.is_some()
    }

    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).module
    }

    pub fn as_type(&self, db: &dyn AnalyzerDb) -> TypeId {
        db.intern_type(Type::Struct(*self))
    }

    pub fn has_private_field(&self, db: &dyn AnalyzerDb) -> bool {
        self.fields(db).values().any(|field| !field.is_public(db))
    }

    pub fn field(&self, db: &dyn AnalyzerDb, name: &str) -> Option<StructFieldId> {
        self.fields(db).get(name).copied()
    }

    pub fn fields(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, StructFieldId>> {
        db.struct_field_map(*self).value
    }

    pub fn all_functions(&self, db: &dyn AnalyzerDb) -> Rc<[FunctionId]> {
        db.struct_all_functions(*self)
    }

    pub fn functions(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, FunctionId>> {
        db.struct_function_map(*self).value
    }
    pub fn function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionId> {
        self.functions(db).get(name).copied()
    }
    pub fn parent(&self, db: &dyn AnalyzerDb) -> Item {
        Item::Module(self.data(db).module)
    }
    pub fn dependency_graph(&self, db: &dyn AnalyzerDb) -> Rc<DepGraph> {
        db.struct_dependency_graph(*self).value.0
    }
    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        sink.push_all(db.struct_field_map(*self).diagnostics.iter());
        sink.push_all(db.struct_dependency_graph(*self).diagnostics.iter());

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
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Result<types::TypeId, TypeError> {
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
pub struct Impl {
    pub trait_id: TraitId,
    pub receiver: TypeId,
    pub module: ModuleId,
    pub ast: Node<ast::Impl>,
}

#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct ImplId(pub(crate) u32);
impl_intern_key!(ImplId);
impl ImplId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Impl> {
        db.lookup_intern_impl(*self)
    }
    pub fn span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.span
    }

    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).module
    }

    pub fn all_functions(&self, db: &dyn AnalyzerDb) -> Rc<[FunctionId]> {
        db.impl_all_functions(*self)
    }

    pub fn trait_id(&self, db: &dyn AnalyzerDb) -> TraitId {
        self.data(db).trait_id
    }

    pub fn receiver(&self, db: &dyn AnalyzerDb) -> TypeId {
        self.data(db).receiver
    }

    pub fn ast(&self, db: &dyn AnalyzerDb) -> Node<ast::Impl> {
        self.data(db).ast.clone()
    }

    pub fn functions(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, FunctionId>> {
        db.impl_function_map(*self).value
    }
    pub fn function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionId> {
        self.functions(db).get(name).copied()
    }
    pub fn parent(&self, db: &dyn AnalyzerDb) -> Item {
        Item::Module(self.data(db).module)
    }
    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        format!(
            "{}_{}",
            self.trait_id(db).name(db),
            self.receiver(db).display(db)
        )
        .into()
    }

    fn validate_type_or_trait_is_in_ingot(
        &self,
        db: &dyn AnalyzerDb,
        sink: &mut impl DiagnosticSink,
        type_module: Option<ModuleId>,
    ) {
        let impl_module = self.data(db).module;
        let is_allowed = match type_module {
            None => impl_module == self.data(db).trait_id.module(db),
            Some(val) => val == impl_module || self.data(db).trait_id.module(db) == impl_module,
        };

        if !is_allowed {
            sink.push(&errors::fancy_error(
                "illegal `impl`. Either type or trait must be in the same ingot as the `impl`",
                vec![Label::primary(
                    self.data(db).ast.span,
                    format!(
                        "Neither `{}` nor `{}` are in the ingot of the `impl` block",
                        self.data(db).receiver.display(db),
                        self.data(db).trait_id.name(db)
                    ),
                )],
                vec![],
            ))
        }
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        match &self.data(db).receiver.typ(db) {
            Type::Contract(_) | Type::Map(_) | Type::SelfContract(_) | Type::Generic(_) => sink
                .push(&errors::fancy_error(
                    format!(
                        "`impl` blocks aren't allowed for {}",
                        self.data(db).receiver.display(db)
                    ),
                    vec![Label::primary(
                        self.data(db).ast.span,
                        "illegal `impl` block",
                    )],
                    vec![],
                )),
            Type::Struct(id) => {
                self.validate_type_or_trait_is_in_ingot(db, sink, Some(id.module(db)))
            }
            Type::Base(_) | Type::Array(_) | Type::Tuple(_) | Type::String(_) => {
                self.validate_type_or_trait_is_in_ingot(db, sink, None)
            }
        }

        if !self.trait_id(db).is_public(db) && self.trait_id(db).module(db) != self.module(db) {
            let trait_module_name = self.trait_id(db).module(db).name(db);
            let trait_name = self.trait_id(db).name(db);
            sink.push(&errors::fancy_error(
                     &format!(
                         "the trait `{}` is private",
                         trait_name,
                     ),
                     vec![
                         Label::primary(self.trait_id(db).data(db).ast.kind.name.span, "this trait is not `pub`"),
                     ],
                     vec![
                         format!("`{}` can only be used within `{}`", trait_name, trait_module_name),
                         format!("Hint: use `pub trait {trait_}` to make `{trait_}` visible from outside of `{module}`", trait_=trait_name, module=trait_module_name),
                     ],
                 ));
        }

        for impl_fn in self.all_functions(db).iter() {
            impl_fn.sink_diagnostics(db, sink);

            if let Some(trait_fn) = self.trait_id(db).function(db, &impl_fn.name(db)) {
                if impl_fn.signature(db).params != trait_fn.signature(db).params {
                    // TODO: This could be a nicer, more detailed report
                    sink.push(&errors::fancy_error(
                        format!(
                            "method `{}` has incompatible parameters for `{}` of trait `{}`",
                            impl_fn.name(db),
                            trait_fn.name(db),
                            self.trait_id(db).name(db)
                        ),
                        vec![
                            Label::primary(
                                impl_fn.data(db).ast.kind.sig.span,
                                "signature of method in `impl` block",
                            ),
                            Label::primary(
                                trait_fn.data(db).ast.span,
                                format!(
                                    "signature of method in trait `{}`",
                                    self.trait_id(db).name(db)
                                ),
                            ),
                        ],
                        vec![],
                    ));
                }

                if impl_fn.signature(db).return_type != trait_fn.signature(db).return_type {
                    // TODO: This could be a nicer, more detailed report
                    sink.push(&errors::fancy_error(
                        format!(
                            "method `{}` has an incompatible return type for `{}` of trait `{}`",
                            impl_fn.name(db),
                            trait_fn.name(db),
                            self.trait_id(db).name(db)
                        ),
                        vec![
                            Label::primary(
                                impl_fn.data(db).ast.kind.sig.span,
                                "signature of method in `impl` block",
                            ),
                            Label::primary(
                                trait_fn.data(db).ast.span,
                                format!(
                                    "signature of method in trait `{}`",
                                    self.trait_id(db).name(db)
                                ),
                            ),
                        ],
                        vec![],
                    ));
                }
            } else {
                sink.push(&errors::fancy_error(
                    format!(
                        "method `{}` is not a member of trait `{}`",
                        impl_fn.name(db),
                        self.trait_id(db).name(db)
                    ),
                    vec![Label::primary(
                        impl_fn.data(db).ast.span,
                        format!("not a member of trait `{}`", self.trait_id(db).name(db)),
                    )],
                    vec![],
                ))
            }
        }

        for trait_fn in self.trait_id(db).all_functions(db).iter() {
            if self.function(db, &trait_fn.name(db)).is_none() {
                sink.push(&errors::fancy_error(
                    format!(
                        "not all members of trait `{}` implemented, missing: `{}`",
                        self.trait_id(db).name(db),
                        trait_fn.name(db)
                    ),
                    vec![Label::primary(
                        trait_fn.data(db).ast.span,
                        "this trait function is missing in `impl` block",
                    )],
                    vec![],
                ))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Trait {
    pub ast: Node<ast::Trait>,
    pub module: ModuleId,
}

#[derive(Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct TraitId(pub(crate) u32);
impl_intern_key!(TraitId);
impl TraitId {
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Trait> {
        db.lookup_intern_trait(*self)
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

    pub fn is_public(&self, db: &dyn AnalyzerDb) -> bool {
        self.data(db).ast.kind.pub_qual.is_some()
    }

    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).module
    }

    pub fn is_implemented_for(&self, db: &dyn AnalyzerDb, ty: TypeId) -> bool {
        self.module(db)
            .all_impls(db)
            .iter()
            .any(|val| &val.trait_id(db) == self && val.receiver(db) == ty)
    }

    pub fn is_in_std(&self, db: &dyn AnalyzerDb) -> bool {
        self.module(db).is_in_std(db)
    }

    pub fn parent(&self, db: &dyn AnalyzerDb) -> Item {
        Item::Module(self.data(db).module)
    }
    pub fn all_functions(&self, db: &dyn AnalyzerDb) -> Rc<[FunctionSigId]> {
        db.trait_all_functions(*self)
    }

    pub fn functions(&self, db: &dyn AnalyzerDb) -> Rc<IndexMap<SmolStr, FunctionSigId>> {
        db.trait_function_map(*self).value
    }

    pub fn function(&self, db: &dyn AnalyzerDb, name: &str) -> Option<FunctionSigId> {
        self.functions(db).get(name).copied()
    }

    pub fn sink_diagnostics(&self, db: &dyn AnalyzerDb, sink: &mut impl DiagnosticSink) {
        db.trait_all_functions(*self).iter().for_each(|id| {
            if !id.takes_self(db) {
                sink.push(&errors::fancy_error(
                    "associated functions aren't yet supported in traits",
                    vec![Label::primary(
                        id.data(db).ast.span,
                        "function doesn't take `self`",
                    )],
                    vec!["Hint: add a `self` param to this function".into()],
                ));
            }
            id.sink_diagnostics(db, sink)
        });
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Event {
    pub ast: Node<ast::Event>,
    pub module: ModuleId,
    pub contract: Option<ContractId>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct EventId(pub(crate) u32);
impl_intern_key!(EventId);

impl EventId {
    pub fn name(&self, db: &dyn AnalyzerDb) -> SmolStr {
        self.data(db).ast.name().into()
    }
    pub fn span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.span
    }
    pub fn name_span(&self, db: &dyn AnalyzerDb) -> Span {
        self.data(db).ast.kind.name.span
    }
    pub fn data(&self, db: &dyn AnalyzerDb) -> Rc<Event> {
        db.lookup_intern_event(*self)
    }
    pub fn is_public(&self, db: &dyn AnalyzerDb) -> bool {
        self.data(db).ast.kind.pub_qual.is_some()
    }
    pub fn typ(&self, db: &dyn AnalyzerDb) -> Rc<types::Event> {
        db.event_type(*self).value
    }
    pub fn module(&self, db: &dyn AnalyzerDb) -> ModuleId {
        self.data(db).module
    }
    pub fn parent(&self, db: &dyn AnalyzerDb) -> Item {
        if let Some(contract_id) = self.data(db).contract {
            Item::Type(TypeDef::Contract(contract_id))
        } else {
            Item::Module(self.module(db))
        }
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
