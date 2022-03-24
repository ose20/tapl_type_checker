# simplebool

### abstract
This implementation corresponds to simply typed lambda-calculus that appeared in Chapter 9 of TaPL.

### Build
To build, just run `dune build`. Pass only one file containing the **arith** program to the resulting file named main.

### Command-line option
- `-I directory`: Add the given directory to the list of directories
searched for running file

### Example
To run the following test file, named sample1.txt,
```:1.txt
lam x:Bool.x;
(lam x:Bool.x) true;
(lam f:Bool->Bool. lam x:Bool. f x);
(lam f:Bool->Bool. lam x:Bool. f x)
  (lam x:Bool. if x then false else true) false;
```
Do the following
```
$ dune exec tyarith -- -I sample sample1.txt
\x:Bool. x : Bool -> Bool
true : Bool
\f:Bool->Bool. \x:Bool. f x : (Bool->Bool) -> Bool -> Bool
true : Bool
```
