# untyped

This is an interpreter for pure untyped lambda-calculus `untyped`.
The relevant TaPL chapters are Charpters 5-7.

### Build
Just run the `make` to generate the `main` program. Pass only one file
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

```
$ ./main -I test test3

lam v. v;
```