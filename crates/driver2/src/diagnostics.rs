use codespan_reporting::diagnostic::{self as cs};
use common::{
    diagnostics::{CompleteDiagnostic, Severity},
    InputFile,
};

use crate::DriverDb;

pub trait ToCsrDiag {
    fn to_csr_diag(&self, db: &dyn DriverDb) -> cs::Diagnostic<InputFile>;
}

impl ToCsrDiag for CompleteDiagnostic {
    fn to_csr_diag(&self, _db: &dyn DriverDb) -> cs::Diagnostic<InputFile> {
        let severity = convert_severity(self.severity);
        let code = Some(self.code.to_string());
        let message = self.message;
        let span = self.span.expect("primary diagnostic must have a span");

        let mut labels = vec![
            cs::Label::new(cs::LabelStyle::Primary, span.file, span.range).with_message(message),
        ];
        labels.extend(self.sub_diagnostics.iter().filter_map(|sub_diag| {
            let span = sub_diag.span?;
            let range = sub_diag.range;
            cs::Label::new(cs::LabelStyle::Secondary, span.file, span.range)
                .with_message(sub_diag.message)
                .into()
        }));

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
