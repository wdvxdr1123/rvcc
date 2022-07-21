use std::env::args;

fn main() {
    if args().len() != 2 {
        eprintln!("{}: invalid number of arguments", args().nth(0).unwrap());
        return;
    }

    println!("  .global main");
    println!("main:");
    println!("  li a0, {}", args().nth(1).unwrap().parse::<i64>().unwrap());
    println!("  ret");
}
