#!/bin/bash

set -e

cargo test

for f in fixture/*.tiny; do
    echo "Running $f"
    ./tinypp $f | cargo run && echo -e "\033[32m >> PASSED\033[0m" || { echo -e "\033[31;1m !! FAILED !\033[0m"; exit -1; }
done

for f in fixture/fail/*.tiny; do
    echo "Running $f (should fail)"
    ./tinypp $f | cargo run && { echo -e "\033[31;1m !! FAILED !\033[0m"; exit -1; } || echo -e "\033[32m >> PASSED\033[0m"
done
