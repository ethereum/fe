mod server;
mod state;
mod db;
mod util;
mod diagnostics;

use db::Jar;
mod handlers {
    pub(crate) mod notifications;
    pub(crate) mod request;
}

use server::run_server;

fn main() {
    let _ = run_server();
}
