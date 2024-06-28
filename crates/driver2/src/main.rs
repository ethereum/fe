use clap::Parser;
use fe_driver2::DriverDataBase;
use hir::hir_def::TopLevelMod;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The file to compile.
    #[arg()]
    file_path: String,

    /// Dump a graphviz dot file of the scope graph for the given file.
    #[arg(long = "dump-scope-graph", default_value_t = false)]
    dump_scope_graph: bool,
}

pub fn main() {
    let args = Args::parse();
    let path = std::path::Path::new(&args.file_path);
    if !path.exists() {
        eprintln!("file '{}' does not exist", args.file_path);
        std::process::exit(2);
    }
    let source = std::fs::read_to_string(&args.file_path).unwrap();

    let mut db = DriverDataBase::default();
    let input_file = db.standalone(path, &source);
    let top_mod = db.top_mod(input_file);
    let diags = db.run_on_top_mod(top_mod);
    diags.emit(&db);

    if args.dump_scope_graph {
        println!("{}", dump_scope_graph(&db, top_mod));
    }
}

fn dump_scope_graph(db: &DriverDataBase, top_mod: TopLevelMod) -> String {
    let mut s = vec![];
    top_mod.scope_graph(db).write_as_dot(db, &mut s).unwrap();
    String::from_utf8(s).unwrap()
}
