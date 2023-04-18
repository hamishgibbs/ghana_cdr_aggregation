
rule all: 
    input: 
        "target/rust/release/main"

rule compile_rust:
    input:
        "src/rust/src/main.rs"
    output:
        "target/rust/release/main"
    shell:
        "cargo build --manifest-path 'src/rust/Cargo.toml' --bin 'main' --release --target-dir 'target/rust'"