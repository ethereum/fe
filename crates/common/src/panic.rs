use once_cell::sync::Lazy;
use std::panic;

const BUG_REPORT_URL: &str = "https://github.com/ethereum/fe/issues/new";
type PanicCallback = dyn Fn(&panic::PanicInfo<'_>) + Sync + Send + 'static;
static DEFAULT_PANIC_HOOK: Lazy<Box<PanicCallback>> = Lazy::new(|| {
    let hook = panic::take_hook();
    panic::set_hook(Box::new(report_ice));
    hook
});

pub fn install_panic_hook() {
    Lazy::force(&DEFAULT_PANIC_HOOK);
}
fn report_ice(info: &panic::PanicInfo) {
    (*DEFAULT_PANIC_HOOK)(info);

    eprintln!();
    eprintln!("You've hit an internal compiler error. This is a bug in the Fe compiler.");
    eprintln!("Fe is still under heavy development, and isn't yet ready for production use.");
    eprintln!();
    eprintln!("If you would, please report this bug at the following URL:");
    eprintln!("  {BUG_REPORT_URL}");
}
