# GraceC - Grace Compiler
NTUA Compilers Project 2023

## Team
Team 18
Members:
  - Altan Avtzi [el19241]

## About

## Project Status

## Goal and Requirements

## Key Learnings

## Dependencies
[flex-latest](https://archlinux.org/packages/core/x86_64/flex/)
[bison-latest](https://archlinux.org/packages/core/x86_64/bison/)
[llvm15-15.0.7-1](https://archlinux.org/packages/extra/x86_64/llvm15/)

## Installation
After cloning the repo use:
```sh
make
```
Also available: `make clean` `make distclean`

## Usage
For basic usage see:
```sh
./grace -h
```

To compile intermediate represantation into assembly use `llc`, e.g:
```sh
llc -o temp.asm mycode.imm
```

To generate executable, link `asm` file with default library using `clang`:
```sh
clang -o a.out temp.asm libgrc.a -no-pie
```

To run directly from source code:
```sh
./run.sh mycode1.grc mycode2.grc ...
```

## Credits

## Copyright
