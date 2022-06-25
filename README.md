# TAPL type-checker

### Abstract
This repository is an OCaml reimplementation of the programming language and its interpreter described in "Types and Programming Languages" by Benjamin C. Peace.

### Why do I "Re"implement ?
The reason for reimplementiong the program by myself, even though [it is distributed
by the official](https://www.cis.upenn.edu/~bcpierce/tapl/), is simply to deepen my understanding of
programming language theory(especially type system), and its implementation, which I'm interested in.
I'm not going to write the exact same program, though; I'm thinking of ...
- changing the pretty-printing format
- improving algorithms to speed up the program
- adding other primitives
- implementing by languages other than OCaml


### Required Environment
To run the interpreter, which actually evaluates the terms of the defined
programming language, you need to have OCaml running. You can install OCaml using, for example, [opam](https://ocaml.org/docs/install.html).

### Contents (now developing)
1. [arith](https://github.com/ose20/tapl_type_checker/tree/main/arith) (chapters 3-4)
2. [untyped](https://github.com/ose20/tapl_type_checker/tree/main/untyped)
(chapters 5-7)
3. [tyarith](https://github.com/ose20/tapl_type_checker/tree/main/tyarith)
(chapter 8)
4. [simplebool](https://github.com/ose20/tapl_type_checker/tree/main/simplebool)(chapter 9)
5. [letexercise](https://github.com/ose20/tapl_type_checker/tree/main/letexercise)(chapter 22)
6. [recon](https://github.com/ose20/tapl_type_checker/tree/main/recon)