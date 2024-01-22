use std::io::{self, stdout, BufRead, Write};

use ordo::env::Env;

const ORDO: &str = "ordo>>> ";
const ERROR: &str = "error: ";

fn print_ordo() {
    print!("{}", ORDO);
    stdout().flush().unwrap();
}

fn main() {
    let mut env = Env::default();
    let stdin = io::stdin();
    print_ordo();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let source = line.trim_end();
        match env.process(source) {
            Ok(output) => {
                println!("{}", output.ty);
                println!("{}", output.val);
            }
            Err(e) => {
                println!("{}{}", ERROR, e);
            }
        }
        print_ordo();
    }
}

#[test]
fn readme_test() {
    let file = std::fs::read_to_string("README.md").unwrap();
    let lines: Vec<&str> = file.lines().collect();
    let mut i = 0;

    let mut env = Env::default();

    while i < lines.len() {
        if lines[i].starts_with(ORDO) {
            let source = lines[i];
            let source = source.strip_prefix(ORDO).unwrap();
            let expected = lines[i + 1];
            let actual = env.process(source);
            match actual {
                Ok(actual) => {
                    if expected.starts_with(ERROR) {
                        panic!(
                            "input '{}' succeeded but expected an error: {}",
                            source, expected
                        );
                    } else {
                        let expected_ty = expected;
                        let expected_val = lines[i + 2];
                        assert_eq!(expected_ty, actual.ty, "for {}", source);
                        assert_eq!(expected_val, actual.val, "for {}", source);
                        i += 3;
                    }
                }
                Err(actual) => {
                    if expected.starts_with(ERROR) {
                        let actual = format!("{}{}", ERROR, actual);
                        assert_eq!(expected, &actual);
                        i += 2;
                    } else {
                        panic!(
                            "input '{}' error '{}' but expected a success: {}",
                            source, actual, expected
                        );
                    }
                }
            }
        } else {
            i += 1;
        }
    }
}
