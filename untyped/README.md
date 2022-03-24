# untyped

This is an interpreter for pure untyped lambda-calculus `untyped`.
The relevant TaPL chapters are Charpters 5-7.

### Build
Just run the `dune build`. And then, pass only one file
containing the **untyped** program to this program.

### Command-line option
- `-I directory`: Add the given directory to the list of directories
searched for running file

### Example
You can use the test combinator from Chapter 5 to emulate the if statement. One thing to note is that `untyped` uses a call-by-value strategy, so v and w need to be lambda abstractions to be values.
That is,
```
(lam l. lam m. lam n. l m n) (lam t. lam f. t) (lam v. v) (lam w. w)
``` 
reduces to 
```
lam v. v
```
To actually do it and see for yourself, just run the following command.
```
$ dune exec ./main.exe -- -I test test3

lam v. v;
```

### Fix bug(?) in official implementation 
There are things in the official implementation(https://www.cis.upenn.edu/~bcpierce/tapl/) that seem to be bugs.
The official version had a bug that the program does not work properly when importing a file that binds variables. For example, suppose you have sample4.txt 
```
y /;
```
and sample5.txt.
```
import "sample4.txt";
lambda x.x;
```
Now, when sample5.txt is executed, the lambda term is not evaluated correctly as shown below.
```
y
(lambda x. [bad index: 0/1 in { x y }])
```
In my implementation, these problems have been resolved.