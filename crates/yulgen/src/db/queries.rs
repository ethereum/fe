use crate::db::YulgenDb;
use crate::mappers;
use fe_analyzer::namespace::items::ModuleId;
use indexmap::map::IndexMap;
use yultsur::yul;

pub fn compile_module(db: &dyn YulgenDb, module: ModuleId) -> IndexMap<String, String> {
    let analyzer_db = db.upcast();
    mappers::module::module(analyzer_db, module)
        .drain()
        .map(|(name, object)| (name, to_safe_json(object)))
        .collect()
}

fn to_safe_json(obj: yul::Object) -> String {
    normalize_object(obj).to_string().replace("\"", "\\\"")
}

fn normalize_object(obj: yul::Object) -> yul::Object {
    let data = obj
        .data
        .into_iter()
        .map(|data| yul::Data {
            name: data.name,
            value: data
                .value
                .replace('\\', "\\\\\\\\")
                .replace('\n', "\\\\n")
                .replace("\"", "\\\\\"")
                .replace('\r', "\\\\r")
                .replace('\t', "\\\\t"),
        })
        .collect::<Vec<_>>();
    yul::Object {
        name: obj.name,
        code: obj.code,
        objects: obj
            .objects
            .into_iter()
            .map(normalize_object)
            .collect::<Vec<_>>(),
        data,
    }
}
