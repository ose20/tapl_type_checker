# letexercise

### abstract
This implementation corresponds to fullrecon in TaPL chapter 22.

### Build
To build, just run `dune build`.

### Command-line option
- `-I directory`: Add the given directory to the list of directories
searched for running file

### Example
To run the following test file, named sample1.txt,
```:sample1.txt
true;;
false;;

let f = lam x:Bool. if x then false else true in f false;;

let app = lam f:Bool -> Bool. lam x:Bool. f x in
let f = lam x:Bool. if x then false else true in
let x = true in
app f x;;


lam f:Bool -> Bool. lam x:Bool. f x;;



```
Do the following
```
$ dune exec letexercise -- -I sample sample1.txt
\x.if x then true else false : Bool -> Bool
\x.x : ?X1 -> ?X1
\x:A.3 : A -> Nat
4 : Nat
5 : Nat
```
