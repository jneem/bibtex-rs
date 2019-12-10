use system_bibtex_tester::BibtexRunner;

fn main() {
    let path = std::env::args().nth(1).unwrap();
    let data = std::fs::read(&path).unwrap();
    let mut runner = BibtexRunner::new(&data);
    println!("{}", runner.check_ours());
}
