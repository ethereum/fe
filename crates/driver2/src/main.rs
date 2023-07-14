use fe_driver2::DriverDataBase;

pub fn main() {
    let arg = std::env::args().nth(1).unwrap();

    let mut db = DriverDataBase::default();
    let source = std::fs::read_to_string(&arg).unwrap();
    db.run_on_file(std::path::Path::new(&arg), &source);
    db.emit_diags();
}
