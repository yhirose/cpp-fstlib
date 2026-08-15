use fst::{Map, MapBuilder};
use std::env;
use std::fs;
use std::io::{BufRead, BufReader};
use std::time::Instant;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: fst-rust-bench <dictionary file>");
        std::process::exit(1);
    }

    let file = fs::File::open(&args[1]).expect("file is not found.");
    let reader = BufReader::new(file);

    let mut words: Vec<String> = reader.lines().map(|l| l.unwrap()).collect();
    words.sort();
    words.dedup();

    eprintln!("{} keys", words.len());
    eprintln!();

    let count = 5;
    let path = "fst_rust.bin";

    eprintln!("#### fst (Rust, burntsushi/fst) ####");

    let start = Instant::now();
    {
        let wtr = std::io::BufWriter::new(fs::File::create(path).unwrap());
        let mut builder = MapBuilder::new(wtr).unwrap();
        for (i, word) in words.iter().enumerate() {
            builder.insert(word, i as u64).unwrap();
        }
        builder.finish().unwrap();
    }
    let build_ms = start.elapsed().as_millis();
    let size = fs::metadata(path).unwrap().len();
    eprintln!("build\t{} millisec.", build_ms);
    println!(
        "size\t{:.1} mega bytes ({} bytes)",
        (size * 100 / 1024 / 1024) as f64 / 100.0,
        size
    );

    let data = fs::read(path).unwrap();
    let map = Map::new(data).unwrap();

    let mut dummy: u64 = 0;
    let start = Instant::now();
    for _ in 0..count {
        for word in &words {
            if let Some(v) = map.get(word) {
                dummy += v;
            } else {
                eprintln!("error: ({})", word);
            }
        }
    }
    let exact_ms = start.elapsed().as_millis();
    eprintln!("exact\t{} millisec.", exact_ms);

    eprintln!();
    eprintln!("{}", if dummy != 0 { " " } else { "  " });
}
