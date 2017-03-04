#!/bin/bash

set -e
for f in fixture/*.tiny; do
    cargo run < $f
done
