# GraceC - Grace Compiler
NTUA Compilers Project 2023

## About
The project involves designing and implementing a compiler for the language Grace by student teams of up to two members. The language for implementation can be chosen from C/C++, Java, SML, OCaml, Haskell, or Python. Tools such as flex, bison, and LLVM are recommended for use in the development process. This project is conducted under the guidance of the instructor and is part of the broader educational curriculum that focuses on compiler construction.

## Team
Team 18:
  - [Altan Avtzi](https://github.com/avtzis) - el19241

## Project Status
FINISHED

## Goal and Requirements
The primary goal of this project is to successfully develop a fully functional compiler for the Grace language that adheres to specified grammatical and syntactical rules. Each team must deliver a compiler capable of lexical analysis, parsing, semantic analysis, generating intermediate code, optimization, and final code generation.

## Key Learnings
Throughout the project, key learnings include understanding the fundamental concepts of compiler design such as lexical analysis, syntax analysis, semantic analysis, optimization, and code generation. Students will gain hands-on experience in implementing these components in their chosen programming language. The project also emphasizes the importance of using compiler construction tools effectively and understanding the intricacies of the Grace programming language.

## Dependencies
- flex
- bison
- LLVM (config, libs, llc, clang) - (last compiled with version [17.0.6](https://github.com/llvm/llvm-project/releases/tag/llvmorg-17.0.6))

## Installation
After cloning the repo, install `gracec` compiler and `libgrc.a` runtime library using `make`.

Also available: `make clean` `make distclean`.

## Usage
To get intermediate reprasentation in LLVM and assembly code:
```sh
./gracec path/to/mycode.grc
```
This generates `path/to/mycode.imm` and `path/to/mycode.asm`.

For more details see:
```sh
./gracec -h
```

To compile intermediate representation into assembly, use `llc`, e.g:
```sh
llc -o temp.asm mycode.imm
```

To generate executable, link `asm` file with default library using `clang`:
```sh
clang -o a.out temp.asm libgrc.a -no-pie
```

## Copyright
This project uses the **GNU General Public License version 3 (GPLv3)** to ensure that it remains free and open-source. This license permits anyone to freely use, modify, and redistribute the project's code, provided that all copies and derivatives also remain free and open under the same GPLv3 license. For more information, please see [LICENSE](LICENSE).
