# hal

A lisp interpreter in Haskell based on [r6rs](http://www.r6rs.org/)

By [@skyf0l](https://github.com/skyf0l) and [@TempoDev](https://github.com/TempoDev)

# Features

- Builtins
  - Basics (car, cdr, cons, void, ...)
  - Predicates (eq?, null?, number?, pair?, ...)
  - Arithmetic (+, -, \*, div, mod, min, max, even?, odd?, pow, sum, product, ...)
  - Comparisons (<, >, <=, >=)
  - String comparison (string=?, string<?, string<=?, string>?, string>=?)
  - List functions (length, append, reverse, map, filter, foldl, foldr, ...)
  - Algorithms (fibonacci, factorial, merge-sort)
  - IO (display, newline)
  - Load library (load)
- Forms
  - Cond
  - If
  - Let
  - Lambda
  - Quote
  - Set
  - Define
  - Begin
- REPL
  - Read-eval-print loop (interactive)
  - Debug commands (show expression in datums / forms)
  - Environment commands (show, enter, leave, clear)

# Usage

```
$ ./hal --help
Usage: ./hal [FILE]... [OPTION]...
  -h                --help                   Show this help and exit
  -q                --quiet                  Suppress prompt
  -i                --interactive            Enable interactive mode
  -l FILE/DIR PATH  --library=FILE/DIR PATH  Add a library path
```

# Demo

```lisp
$ ./hal
HAL - Lisp REPL
Type :h to see help
Type :q to quit

> (define add (lambda (a b) (+ a b)))
> add
#<procedure>
> (add 40 2)
42
> (set! add 42)
> add
42
> (fib 21)
10946
> (merge-sort '(39 16 22 24 17 29 18 26 27 3 34 25 10 6 7 12 8 30 2 21 13 36 14 38 32 41 40 4 35 19 5 33 23 9 15 31 28 20 42 37 11 1))
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42)
```

# Build

```
$ make
```

or

```
$ stack build --copy-bins
```

# Tests

```
$ make tests_run
```

or

```
$ ./tests.sh
```
