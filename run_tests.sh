#!/bin/bash

set -e

cargo test

for f in fixture/*.tiny; do
    echo "Running $f"
    ./tinypp $f | cargo run
done
