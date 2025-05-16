# Yoloscript

Yoloscript is a small programming language, it is still very much a work in progress.
The language itself is inspired by [Lox](https://craftinginterpreters.com/), but it has some changes and additions.
This is an implementation in Rust.

## Usage

```bash
$ cargo run
```

## Grammar 

The complete grammar is on the [Yoloscript grammar](https://github.com/KeenanCox/yoloscript/blob/main/grammar.md) page.

Here are some example programs:

### Hello 

```yoloscript
fun main() {
    print "Hello world!";
}

main();
```

### Sum

```yoloscript
fun sum(a, b) {
    return a + b;
}

print sum(1, 2);
```

### Clojure

```yoloscript

fun outerFunction() {
    mut innerVar = 0;

    fun innerFunction() {
	    innerVar = innerVar + 1;
	    print innerVar;
    }
    return innerFunction;
}
let clojure = outerFunction();

clojure(); // prints 1

clojure(); // prints 2

```