#!/bin/bash
set -e

LOG_FILE=test.log

# Run tests

function run_test() {
    local file=$1
    printf "\nNow testing: %s \n \n" "$file" >> "$LOG_FILE"

    # Run the test
    ./target/release/rslox "$file" >> "$LOG_FILE" 2>&1
}
function main(){
    rm -f "$LOG_FILE"

    for file in tests/*; do
        run_test "$file"
    done
}

main
