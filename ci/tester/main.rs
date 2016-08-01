//! This basically parses given files and pretty-prints them back
//! Then we can compare the test results before & after, if they aren't identical, it's likely a parser bug
//!
//! most important usages:
//! pico-php-tester parse <path>            : Parse&Print the files given in the <path>
//! pico-php-tester parse <path> <check>    : Parse&Print the files given in the <path> and test using <check> (exit code = 0)
//! pico-php-tester parse -b <path> <check> : Parse&Print the files given in the <path> and try to locate the origin of the first error
//!                                           using binary search techniques
//! pico-php-tester parse-file <file>       : Parse&Print a given file
//!
extern crate pico_php_parser;
extern crate glob;
extern crate wait_timeout;
extern crate clap;

use std::collections::{HashSet, HashMap};
use std::env;
use std::fs::File;
use std::io::{Read, Write};
use std::fmt::Write as WriteFmt;
use std::path::{self, Path};
use std::process::Command;
use glob::glob;
use wait_timeout::ChildExt;
use clap::{Arg, App, SubCommand};
use pico_php_parser::{Parser, PrettyPrinter};

fn main() {
    let matches = App::new("PicoPhpParser-Tester")
        .version("0.1")
        .arg(Arg::with_name("binary-search")
            .short("b")
        )
        .subcommand(SubCommand::with_name("parse")
            .about("parses files in a directory")
            .arg(Arg::with_name("DIR").index(1).required(true))
            .arg(Arg::with_name("CHECK").required(false))
        )
        .subcommand(SubCommand::with_name("parse-file")
              .about("parses a file")
              .arg(Arg::with_name("FILE").index(1).required(true))
              .arg(Arg::with_name("CHECK").required(false))
        )
        .get_matches();

    // perform the checks with binary search / divide and conquer
    let do_binary_search = matches.occurrences_of("binary-search") > 0;

    if let Some(matches) = matches.subcommand_matches("parse") {
        let dir = matches.value_of("DIR").unwrap();
        println!("Parsing: ");
        let blacklist = [
            Path::new(dir).join("vendor"),
            Path::new(dir).join("tests")
        ];
        let path = Path::new(dir).join("**/*.php");
        let mut done_files = HashSet::new();
        let mut i = 0;

        let files: Vec<String> = glob(&path.to_string_lossy()).unwrap().filter_map(Result::ok).filter(|entry| {
            // skip vendor stuff, since we can not guarantee to test it
            // skip tests stuff, to ensure we actually pass all tests (and do not only pass wrongly generated tests)
            for bpath in &blacklist {
                if entry.as_path().starts_with(bpath) {
                    return false;
                }
            }
            return true;
        }).map(|entry| format!("{}", entry.display())).collect();
        let has_check = matches.value_of("CHECK").is_some();

        let run_parse_file = |entry: &str, no_check:bool| {
            let mut cmd = Command::new(env::current_exe().unwrap());
            cmd
                .arg("parse-file")
                .arg(entry);
            if let Some(check_arg) = matches.value_of("CHECK") {
                if !no_check {
                    cmd.arg(check_arg);
                }
            }
            let mut child = cmd.spawn().unwrap();
            match child.wait_timeout_ms(60000).unwrap() {
                Some(status) if status.success() => {
                    println!("SUCCESS");
                    return true;
                },
                Some(_) => println!("ERROR"),
                _ => {
                    println!("TIMEOUT");
                    child.kill().unwrap();
                }
            }
            return false;
        };

        // if binary search is specified:
        // - Read all files to memory (else we need to rely on git revert, which is more difficult to implement)
        // - check the first half
        // - If all tests pass, revert, check the second half
        // - If not, check the first half of the first half, and continue
        // the general goal is to find the specific file which is malformed due to a parser or printer bug
        if do_binary_search {
            assert!(has_check);
            let mut src_map = HashMap::new();
            for file in &files {
                let mut str_ = String::new();
                File::open(file).unwrap().read_to_string(&mut str_).unwrap();
                src_map.insert(file, str_);
            }
            let mut low = 0;
            let mut high = files.len();
            while high - low > 1 {
                let mid = (low + high) / 2;
                // parse the files from low -> mid
                let mut passes = true;
                println!("checking from {} to {}", low, mid);
                for (i, entry) in files[low..mid].iter().enumerate() {
                    // perform the check for the last item
                    let no_check = i != mid - low - 1;
                    if !run_parse_file(entry, no_check) {
                        passes = false;
                        break;
                    }
                }
                println!("reverting...");
                // revert the changes
                for entry in &files[low..mid] {
                    write!(File::create(entry).unwrap(), "{}", src_map[entry]).unwrap();
                }
                if passes {
                    println!("range passed.");
                    low = mid;
                } else {
                    println!("range failed.");
                    high = mid;
                }
            }
            println!("bad: {} resolves to {}", low, files[low]);
            println!("good: {} resolves to {}", high, files[high]);
        } else {
            // normal parsing
            for entry in &files {
                i += 1;
                if run_parse_file(entry, false) {
                    done_files.insert(entry.to_owned());
                }
            }
            let success = done_files.len();
            if success != i {
                panic!("Parsing stage failed!");
            }
            println!("all_file_count: {}, handled: {} ({:.2}%), left: {}", i, success, (success as f64)/(i as f64)*100f64, i-success);
        }
        return;
    }

    if let Some(matches) = matches.subcommand_matches("parse-file") {
        let file = matches.value_of("FILE").unwrap();
        parse_file(file, matches.value_of("CHECK"));
        return;
    }

    panic!("invalid arguments!")
}

fn parse_file(arg: &str, check_cmd: Option<&str>) {
    println!("parsing {}", arg);
    let mut s = String::new();
    let ast = {
        let mut f = File::open(arg).unwrap();
        f.read_to_string(&mut s).unwrap();
        match Parser::parse_str(&s) {
            Ok(ast) => ast,
            Err(err) => {
                panic!("ERROR: {}", err.error_message(Some(&s)));
            }
        }
    };
    {
        let mut f = File::create(arg).unwrap();
        let mut str_ = String::new();
        PrettyPrinter::print_statements(&mut str_, ast).unwrap();
        write!(f, "<?php\n{}\n?>", str_).unwrap();
    }
    if let Some(cmd) = check_cmd {
        let mut child = Command::new(cmd).spawn().unwrap();
        match child.wait_timeout_ms(60000).unwrap() {
            Some(status) if status.success() => {
                println!("check ok");
            },
            Some(_) => panic!("check error"),
            _ => {
                child.kill().unwrap();
                panic!("check timeout");
            }
        }
    }
}
