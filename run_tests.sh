#!/bin/bash

set -e

cargo test

for f in fixture/*.tiny; do
    cargo run < $f
done
