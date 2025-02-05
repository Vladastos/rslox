use const_gen::{const_definition, CompileConst};
use glob::glob;
use itertools::Itertools;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use serde::Deserialize;
use std::{
    env,
    ffi::OsStr,
    fs,
    os::unix::ffi::{OsStrExt, OsStringExt},
    path::{Path, PathBuf},
    sync::atomic::AtomicUsize,
};

#[derive(CompileConst)]
#[inherit_docs]
/// A test case
struct Test {
    /// The name of the test case
    name: String,
    /// Expected outcome of the test case
    outcome: TestOutcome,
    /// Source file containing the tested code
    source: Vec<u8>,
}

#[derive(Deserialize, Default)]
/// A test case description file
struct TestDescription {
    /// The name of the test case
    #[serde(default)]
    name: Option<String>,
    /// Expected outcome of the test case
    #[serde(default)]
    outcome: TestOutcome,
    /// Source file containing the tested code
    #[serde(default)]
    source: Option<PathBuf>,
}

#[derive(CompileConst, Deserialize, Default)]
#[serde(tag = "type")]
#[inherit_docs]
/// Outcome of a test
enum TestOutcome {
    /// File runs without problems
    #[default]
    Success,
    /// File fails to run
    Failure {
        /// Error message that must be produced
        message: Option<String>,
    },
}

fn main() {
    // Use the OUT_DIR environment variable to get an
    // appropriate path.
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("tests.rs");

    // Contstant declarations and type definitions
    let mut const_declarations = vec![
        const_definition!(
            #[derive(Debug)]
            Test
        ),
        const_definition!(
            #[derive(Debug)]
            TestOutcome
        ),
    ];

    for test in Iterator::chain(
        // All test description files
        glob("tests/**/*.test").unwrap().map(handle_test_file),
        // All test source file
        glob("tests/**/*.lox").unwrap().map(handle_lox_file),
    )
    // Removing all files that maps to the same source
    // In particular, this will remove `.lox` files when the `.test` one is already present
    .unique_by(|r| {
        // Mapping each value to either the canonicalized path, or a unique increasing number
        r.as_ref()
            .ok()
            .and_then(|t| Path::new(OsStr::from_bytes(&t.source)).canonicalize().ok())
            .ok_or_else(|| {
                static COUNTER: AtomicUsize = AtomicUsize::new(0);
                COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
            })
    }) {
        let test = match test {
            Ok(test) => test,
            Err(err) => {
                cargo_emit::warning!("{}", err);
                continue;
            }
        };
        let fun_name = format_ident!(
            "{}",
            test.name
                .replace(
                    |ch| !matches!(ch, 'a'..='z' | 'A'..='Z' | '0'..='9'|'_'),
                    "_"
                )
                .to_lowercase()
        );
        let test: TokenStream = test.const_val().parse().unwrap();
        const_declarations.push(
            quote! {
                #[test]
                fn #fun_name() {
                    test_impl(#test)
                }
            }
            .to_string(),
        );
    }

    fs::write(&dest_path, const_declarations.join("\n")).unwrap();
}

fn handle_test_file(descr_source: Result<PathBuf, glob::GlobError>) -> anyhow::Result<Test> {
    let descr_source = descr_source?;

    let TestDescription {
        name,
        outcome,
        source,
    } = toml::from_str(&fs::read_to_string(&descr_source)?)?;

    let source = source.unwrap_or_else(|| descr_source.with_extension("lox"));
    let name = name.unwrap_or_else(|| {
        source
            .file_stem()
            .unwrap_or_default()
            .to_string_lossy()
            .into_owned()
    });

    let source = source.into_os_string().into_vec();

    Ok(Test {
        name,
        outcome,
        source,
    })
}

fn handle_lox_file(source: Result<PathBuf, glob::GlobError>) -> anyhow::Result<Test> {
    let source = source?;

    let TestDescription {
        name,
        outcome,
        source: _,
    } = Default::default();

    let name = name.unwrap_or_else(|| {
        source
            .file_stem()
            .unwrap_or_default()
            .to_string_lossy()
            .into_owned()
    });

    let source = source.into_os_string().into_vec();

    Ok(Test {
        name,
        outcome,
        source,
    })
}
