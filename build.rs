use anyhow::bail;
use const_gen::{const_definition, CompileConst};
use glob::glob;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use serde::Deserialize;
use std::{
    env, fs, io,
    os::unix::ffi::OsStringExt,
    path::{Path, PathBuf},
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

    for test in glob("tests/**/*.lox").unwrap() {
        let test = match fun_name(test) {
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

fn fun_name(source: Result<PathBuf, glob::GlobError>) -> anyhow::Result<Test> {
    let source = source?;

    let TestDescription { name, outcome } = match fs::read_to_string(&source.with_extension("test"))
    {
        Ok(test_description) => toml::from_str(&test_description)?,
        Err(err) if err.kind() == io::ErrorKind::NotFound => TestDescription::default(),
        Err(err) => bail!(err),
    };

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
