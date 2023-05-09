mod server;
mod state;

use server::run_server;

fn main() {
    let _ = run_server();
}
