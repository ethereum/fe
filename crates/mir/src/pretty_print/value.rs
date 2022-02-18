use std::fmt::{self, Write};

use crate::{
    db::MirDb,
    ir::{constant::ConstantValue, function::BodyDataStore, Value, ValueId},
};

use super::PrettyPrint;

impl PrettyPrint for ValueId {
    fn pretty_print<W: Write>(
        &self,
        db: &dyn MirDb,
        store: &BodyDataStore,
        w: &mut W,
    ) -> fmt::Result {
        match store.value_data(*self) {
            Value::Temporary(_) | Value::Local(_) => write!(w, "_{}", self.index()),
            Value::Immediate(imm) => write!(w, "{}", imm.value),
            Value::Constant(constant) => {
                let const_value = constant.constant.data(db);
                write!(w, "const ")?;
                match &const_value.value {
                    ConstantValue::Immediate(num) => write!(w, "{}", num),
                    ConstantValue::Str(s) => write!(w, r#""{}""#, s),
                    ConstantValue::Bool(b) => write!(w, "{}", b),
                }
            }
            Value::Unit(_) => write!(w, "()"),
        }
    }
}

impl PrettyPrint for &[ValueId] {
    fn pretty_print<W: Write>(
        &self,
        db: &dyn MirDb,
        store: &BodyDataStore,
        w: &mut W,
    ) -> fmt::Result {
        if self.is_empty() {
            return Ok(());
        }

        let arg_len = self.len();
        for arg in self.iter().take(arg_len - 1) {
            arg.pretty_print(db, store, w)?;
            write!(w, ", ")?;
        }
        let arg = self[arg_len - 1];
        arg.pretty_print(db, store, w)
    }
}
