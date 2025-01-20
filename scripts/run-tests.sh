#!/bin/bash
set -e

# Run tests

function run_test() {
    local file=$1
    printf "\n Now testing: %s \n \n" "$file"

    # Run the test
    ./target/release/rslox "$file"
}

for file in tests/*; do
    run_test "$file"
done