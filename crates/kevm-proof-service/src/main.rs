use fe_proof_service::{invariant::Invariant, serde_json};
use std::{
    io::{BufRead, BufReader, BufWriter},
    net::{TcpListener, TcpStream},
};

use server_impl::Server;

mod server_impl;

use clap::Parser;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(short, long, default_value = "127.0.0.1:7878")]
    bind_addrs: String,
    #[clap(short, long, action)]
    display: bool,
    #[clap(short, long, default_value = "8")]
    max_proofs: usize,
    #[clap(short, long, default_value = "db.yaml")]
    db_path: usize,
}

fn main() {
    let args = Args::parse();

    let listener = TcpListener::bind(args.bind_addrs).unwrap();
    let mut server = Server::new("db.yaml");

    if args.display {
        server.display();
    }

    for stream in listener.incoming() {
        let stream = stream.unwrap();
        connection_handler(&mut server, stream);
    }
}

fn connection_handler(server: &mut Server, mut stream: TcpStream) {
    let mut stream_clone = stream.try_clone().unwrap();

    let mut reader = BufReader::new(&mut stream);
    let mut writer = BufWriter::new(&mut stream_clone);

    let invariant: Invariant = {
        let mut invariant_encoded = String::new();
        reader.read_line(&mut invariant_encoded).unwrap();
        serde_json::from_str(&invariant_encoded).expect("unable to decode invariant")
    };

    let rerun: bool = {
        let mut rerun_encoded = String::new();
        reader.read_line(&mut rerun_encoded).unwrap();
        serde_json::from_str(&rerun_encoded).expect("unable to decode rerun")
    };

    if rerun {
        server.forget(invariant.id());
    }

    let status = server.verify(invariant);
    serde_json::to_writer(&mut writer, &status).expect("unable to encode invariant status");
}
