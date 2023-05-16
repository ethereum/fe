mod server;
mod state;

mod handlers {
    // pub(crate) mod notification;
    pub(crate) mod request;
}

use server::run_server;

fn main() {
    let _ = run_server();
    // log "hello world" to the console
    println!("Hello, world!");
}
