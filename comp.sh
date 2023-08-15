#!/bin/zsh

llc -o temp.s $1
clang -o a.out temp.s libgrc.a -no-pie
rm temp.s $1 *.asm
