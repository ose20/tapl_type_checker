# TAPL type-checker

### Abstract
This repository is an OCaml reimplementation of the programming language and its interpreter described in "Types and Programming Languages" by Benjamin C. Peace.

### Why do I "Re"implement ?
The reason for reimplementiong the program in OCaml by myself, even though [it is distributed
by the official](https://www.cis.upenn.edu/~bcpierce/tapl/), is simply to deepen my understanding of
programming language theory(especially type system), and its implementation, which I'm interested in.
I'm not going to write the exact same program, though; I'm thinking of changing the pretty-printing format,
adding some primitives, and so on.

There are two main reasons for this. First, the [officially distributed
programs](https://www.cis.upenn.edu/~bcpierce/tapl/) do not work with
the current version of OCaml, and have other minor bugs. Secondly, I am interested in programming language theory, especially type
system, and wanted to implement and study them.

### Required Environment
To run the interpreter, which actually evaluates the terms of the defined
programming language, you need to have OCaml running. You can install OCaml using, for example, [opam](https://ocaml.org/docs/install.html).

### Contents (now developing)
1. [arith](https://github.com/ose20/tapl_type_checker/tree/main/arith) (chapters 3-4)