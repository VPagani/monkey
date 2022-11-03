# Monkey Programming Language

A dynamic toy programming language implemented as an interpreter twice from scratch in Rust ([./src/monkey-rs](./src/monkey-rs)) and Go ([./src/monkey-go](./src/monkey-go)) based on the book [Writing an Interpreter in Go](https://interpreterbook.com/) by [Thorsten Ball](https://twitter.com/thorstenball)

## Examples

### Hello World
```js
puts("hello world");
//> "hello world"
```

### Variables and Expressions
```js
let a = 1;
puts(a + 1);
//> "2"

let b = 6;
puts(a + b);
//> "8"

let b = " foo";
puts(4 + a + b);
//> "15 foo"
```

### Data types
```js

let str = "this is a string";
puts(len(str));
//> "16"

let arr = ["this", "is", "an", "array", 1, true];
puts(len(arr));
//> "6"

let hsh = { "this": "is", "a": "hash", true: false, 1: 101 };
puts(hsh["this"] + hsh["a"])
//> "ishash"
```

### Functions

```js
let factorial = fn(n) {
  if (n <= 1) {
    return 1;
  }

  return n * factorial(n - 1);
}

puts(factorial(8));
//> "40320"

let fibonacci = fn(n) {
  if (n <= 1) {
    return 1;
  }

  return fibonacci(n - 2) + fibonacci(n -1);
}

puts(fibonacci(25))
//> "121393"
```

### Built-in Functions
```js
let arr = [0, 1, 2, 3, 5, 6, 7, 8, 9]

puts(len(arr))
//> "10"

puts(first(arr))
//> "0"

puts(last(arr))
//> "9"

puts(rest(arr))
//> "[1, 2, 3, 5, 6, 7, 8, 9]"

puts(push(arr, 10))
//> "[0, 1, 2, 3, 5, 6, 7, 8, 9, 10]"
```