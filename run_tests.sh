#!/bin/bash

set -e

cargo test
cargo build

PASS() {
    echo -e "\033[1;32m >> PASSED\033[0m"
}

FAIL() {
    echo -e "\033[31;1m !! FAILED !\033[0m"
    ./tinypp $f
    exit -1
}

for f in fixture/*.tiny; do
    echo -e "\033[1m == Running $f ==\033[0m"
    ./tinycc $f && PASS || FAIL
done

for f in fixture/fail/*.tiny; do
    echo -e "\033[1m == Running $f (should fail) ==\033[0m"
    ./tinycc $f && FAIL || PASS
done

for p in fac HelloDummy HelloWorld; do
    make $p.prog && ./$p.prog
done
