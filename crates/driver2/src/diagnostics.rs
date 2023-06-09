use codespan_reporting::diagnostic::{self as cs};
use common::{
    diagnostics::{LabelStyle, Severity},
    InputFile,
};
use hir::diagnostics::DiagnosticVoucher;

use crate::DriverDataBase;

pub trait IntoCsrDiag {
    fn into_csr(self, db: &DriverDataBase) -> cs::Diagnostic<InputFile>;
}

impl<T> IntoCsrDiag for T
where
    T: DiagnosticVoucher,
{
    fn into_csr(self, db: &DriverDataBase) -> cs::Diagnostic<InputFile> {
        let complete = self.to_complete(db);

        let severity = convert_severity(complete.severity);
        let code = Some(complete.error_code.to_string());
        let message = complete.message;

        let labels = complete
            .sub_diagnostics
            .into_iter()
            .filter_map(|sub_diag| {
                let span = sub_diag.span?;
                match sub_diag.style {
                    LabelStyle::Primary => {
                        cs::Label::new(cs::LabelStyle::Primary, span.file, span.range)
                    }
                    LabelStyle::Secondary => {
                        cs::Label::new(cs::LabelStyle::Secondary, span.file, span.range)
                    }
                }
                .with_message(sub_diag.message)
                .into()
            })
            .collect();

        cs::Diagnostic {
            severity,
            code,
            message,
            labels,
            notes: vec![],
        }
    }
}

fn convert_severity(severity: Severity) -> cs::Severity {
    match severity {
        Severity::Error => cs::Severity::Error,
        Severity::Warning => cs::Severity::Warning,
        Severity::Note => cs::Severity::Note,
    }
}
