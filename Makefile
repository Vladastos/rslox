TESTS = $(shell ls tests/* | cut -d . -f 1)

run: build
	./target/release/$(shell cat Cargo.toml | grep name | cut -d \" -f 2)

test: build
	bash scripts/run-tests.sh

build: format
	cargo build --release

format:
	cargo fmt

.PHONY: build run format
