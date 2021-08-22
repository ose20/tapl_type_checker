# arith

### Build
To build, just run `make`. Pass only one file containing the **arith** program to the resulting file named main.

### Command-line option
- `-I directory`: Add the given directory to the list of directories
searched for running file

### Example
```
$ ./main -I sample_code test1.txt
17
0
9
0
```

```
$ ./main -I sample_code test2.txt
false
99
```

```
$ ./main -I sample_code test3.txt
(succ (pred false))
if
  2
then
  if
    pred 10
  then
    iszero 2
  else
    if
      true
    then
      0
    else
      4
else
  15
```

### Concrete Syntax
The **arith** interpreter can accept semicolon-delimited terms and **import** statements. Be sure to add a semicolon at the end.
For example, the following file has a valid concrete syntax.
```:test4.txt
import "test1.txt";
import "test2.txt";
import "test3.txt";
iszero (pred (succ 0));
succ (if true then 2 else 3);
```