use clap::Parser;
use serde::Deserialize;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    src: String,
}

#[derive(Debug, Deserialize)]
enum Op {
    #[serde(rename = "+")]
    Add,
}

#[derive(Debug, Deserialize)]
struct LineColumn {
    line: usize,
    column: usize,
}

#[derive(Debug, Deserialize)]
struct Span {
    start: LineColumn,
    end: LineColumn,
}

#[derive(Debug, Deserialize)]
struct Spanned<T> {
    span: Span,
    value: T,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
enum Expr {
    Int(i32),
    BinaryOp {
        lhs: Box<Self>,
        op: Spanned<Op>,
        rhs: Box<Self>,
    },
}

impl Expr {
    fn run(&self) -> i32 {
        match self {
            Self::Int(n) => *n,
            Self::BinaryOp { lhs, op, rhs } => {
                let lhs = lhs.run();
                let rhs = rhs.run();
                match op.value {
                    Op::Add => lhs + rhs,
                }
            }
        }
    }
}

fn main() {
    let args = Args::parse();
    let expr: Expr = serde_json::from_str::<Expr>(&args.src).unwrap();
    println!("{}", expr.run())
}
