#!/bin/bash

# 声明一个函数
assert() {
  expected="$1"
  input="$2"

  ./target/debug/rvcc "$input" > tmp.s || exit
  riscv64-unknown-linux-gnu-gcc -static -o tmp tmp.s

  qemu-riscv64 -L $RISCV/sysroot ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 0
assert 42 42
assert 34 '12-34+56'
assert 41 ' 12 + 34 - 5 '
assert 47 '5+6*7'
assert 15 '5*(9-6)'
assert 4 '(3+5)/2'

echo OK