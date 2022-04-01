# letexercise

### abstract
This implementation corresponds to TaPL exercise 11.5.1 : Extension of the
let expression to the simply typed lambda calculus.

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
- : Bool = true
- : Bool = false
- : Bool = true
- : Bool = false
- : (Bool -> Bool) -> Bool -> Bool = \f:Bool -> Bool. \x:Bool. f x
```
