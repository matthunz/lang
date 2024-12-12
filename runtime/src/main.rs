use clap::Parser;
use serde::Deserialize;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    src: String,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
enum Expr {
    Int(i32),
}

fn main() {
    let args = Args::parse();
    let _ = dbg!(serde_json::from_str::<Expr>(&args.src));
}
