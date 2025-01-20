
run: build
	./target/release/$(shell cat Cargo.toml | grep name | cut -d \" -f 2)

build: format
	cargo build --release

format:
	cargo fmt

.PHONY: build run format
