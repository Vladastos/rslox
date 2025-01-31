TESTS = $(shell ls tests/* | cut -d . -f 1)

run: build
	./target/release/$(shell cat Cargo.toml | grep name | cut -d \" -f 2)

test: build
	cargo test

build: fmt
	cargo build --release

fmt:
	cargo fmt

lint:
	cargo clippy

.PHONY: build run format test lint
