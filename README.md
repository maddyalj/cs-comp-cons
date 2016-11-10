# cs-comp-cons
*Compiler Construction: This OCaml program is a compiler tool for my special language.*

## Usage

### Building Compiler
``` bash
$ make build
$ make build-part5 # to build Interpreter and Code Generator
```
This will install `core` and `menhir` using `opam` first if they are not already installed, then build the compiler.

### Running Tests
``` bash
$ make test
$ make test-part5 # to test Interpreter and Code Generator
```
This will run all test files existing in the `tests` directory. If the filename contains a string in the format `.e_VALUE.`, it will output the test actual value and expect it to be the same as `VALUE`.

## Syntax
My language uses c-style syntax, inspired by Javascript.

### Function Definition
``` js
main (params) {
    0;
}
```
To return a value from a function, just add it as the last expression in the function.

### Comments
``` js
/*
    This is a multiline ..
    .. comment!
*/
some_code(); /* inline comment */
```

### Local Variables
``` js
let x = 5;
```

### Operations
``` js
let x = 5;
let y = (x + 5) / 2;
```
Available Operators: `+` `-` `*` `/` `<=` `<` `>=` `>` `==` `!=` `&&` `||` `!`. You can use brackets to prioritize expressions.

### Assignment
``` js
let x = 5;
x = 10;
```

### Constants
``` js
const ENVIRONMENT = 1;
```

### Calling Functions
``` js
sum(x, y);
```

### Outputting Values
``` js
print x;
```

### Debugging
``` js
dump(x);
dump(x + 3);
dump(x, y);
```

### Reading User Input (Int Only - for now)
``` js
let x = read_int();
```

### While Loops
``` js
while (i < max) {
    do_something();
    i = i + 1;
}
```

### If Statements
``` js
if (i > 0) {
    do_something();
} else {
    do_something_else();
}
```

### Complex Expression Assignment
``` js
(let z = x + y; if (z = 0) { x } else { y }) = 7;
```
Put parentheses around expressions to ensure they are parsed into a sequence.
