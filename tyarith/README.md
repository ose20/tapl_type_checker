# tyarith

### abstract
This implementation corresponds to the system of typed boolean and number calculations that appeared in Chapter 8 of TaPL.

### Build
To build, just run `make`. Pass only one file containing the **arith** program to the resulting file named main.

### Command-line option
- `-I directory`: Add the given directory to the list of directories
searched for running file

### Example
To run the following test code,
```ruby:sample1.txt
0;
pred 0;
succ succ succ pred (succ 0);
if false then false else true;
if if iszero 0 then false else true then succ 2 else pred 2;
```
