# tyarith

### abstract
This implementation corresponds to the system of typed boolean and number calculations that appeared in Chapter 8 of TaPL.

### Build
To build, just run `make`. Pass only one file containing the **arith** program to the resulting file named main.

### Command-line option
- `-I directory`: Add the given directory to the list of directories
searched for running file

### Example
To run the following test file, named sample1.txt,
```:sample1.txt
0;
pred 0;
succ succ succ pred (succ 0);
if false then false else true;
if if iszero 0 then false else true then succ 2 else pred 2;
```
Do the following
```
$ ./main -I ../test sample1.txt
0 : Nat
0 : Nat
3 : Nat
true : Bool
1 : Nat
```
### Syntax
The syntax of tyarith is defined as follows.
```
<toplevel>  ::= EOF | <command> ; <toplevel>

<command>   ::= import `string` | <term>

<term>      ::= if <term> then <term> else <term>
              | <appTerm>

<appTerm>   ::= succ <appTerm> | pred <appTerm> 
              | iszero <appTerm> | <aTerm>

<aTerm>     ::= true | false | `natural number` | ( <term> )
```