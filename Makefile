
run: build
	./target/release/$(shell cat Cargo.toml | grep name | cut -d \" -f 2)

build:
	cargo build --release

.PHONY: build
