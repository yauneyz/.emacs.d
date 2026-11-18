//! git-roots: print every directory containing a `.git` entry below the given
//! roots, one per line. Stops descending at the first git root (nested repos
//! and submodules are intentionally not reported). Follows symlinks
//! (loop-safe) and skips heavy build dirs. Ignores .gitignore files.
//!
//! Usage: git-roots [--skip NAME]... DIR...

use ignore::{WalkBuilder, WalkState};
use std::collections::HashSet;
use std::io::{self, Write};
use std::sync::{mpsc, Arc};

const DEFAULT_SKIP: &[&str] = &[
    ".git", "node_modules", "target", ".venv", "venv", "__pycache__", "dist",
    "build", ".cache", ".direnv", ".stack-work", "dist-newstyle", ".cpcache",
    ".gradle", ".idea", "straight", "elpa", "eln-cache", "celeba",
];

fn main() {
    let mut skip: HashSet<String> = HashSet::new();
    let mut roots = Vec::new();
    let mut custom_skip = false;
    let mut args = std::env::args().skip(1);
    while let Some(a) = args.next() {
        match a.as_str() {
            "--skip" => {
                custom_skip = true;
                if let Some(n) = args.next() {
                    skip.insert(n);
                }
            }
            "-h" | "--help" => {
                eprintln!("usage: git-roots [--skip NAME]... DIR...");
                return;
            }
            _ => roots.push(a),
        }
    }
    if !custom_skip {
        skip.extend(DEFAULT_SKIP.iter().map(|s| s.to_string()));
    }
    // `.git` itself is never worth descending into.
    skip.insert(".git".into());
    if roots.is_empty() {
        eprintln!("usage: git-roots [--skip NAME]... DIR...");
        std::process::exit(2);
    }

    let mut builder = WalkBuilder::new(&roots[0]);
    for r in &roots[1..] {
        builder.add(r);
    }
    builder
        .standard_filters(false)
        .follow_links(true)
        .threads(std::thread::available_parallelism().map_or(4, |n| n.get().min(12)));

    let skip = Arc::new(skip);
    let (tx, rx) = mpsc::channel::<String>();
    let walker = builder
        .filter_entry({
            let skip = Arc::clone(&skip);
            move |e| {
                e.depth() == 0
                    || !e.file_type().is_some_and(|t| t.is_dir())
                    || !e.file_name().to_str().is_some_and(|n| skip.contains(n))
            }
        })
        .build_parallel();

    let printer = std::thread::spawn(move || {
        let stdout = io::stdout();
        let mut out = io::BufWriter::new(stdout.lock());
        for line in rx {
            let _ = writeln!(out, "{line}");
        }
    });

    walker.run(|| {
        let tx = tx.clone();
        Box::new(move |res| {
            if let Ok(e) = res {
                if e.file_type().is_some_and(|t| t.is_dir()) && e.path().join(".git").exists() {
                    let _ = tx.send(e.path().to_string_lossy().into_owned());
                    return WalkState::Skip;
                }
            }
            WalkState::Continue
        })
    });
    drop(tx);
    let _ = printer.join();
}
