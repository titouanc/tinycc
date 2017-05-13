#!/bin/bash

set -e

cargo test

for f in $(find fixture/ -name '*.tiny'); do
    echo "Running $f"
    ./tinypp $f | cargo run
done
