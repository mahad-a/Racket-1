# Racket-1: A Minimal Racket Evaluator

This project is a minimalist interpreter for a small subset of the Racket language, written in Racket itself. It implements a basic **substitution-based evaluation model** without using `define`, simulating how expressions are processed and evaluated under the hood.

## What it Does

- Evaluates Racket expressions using a simplified REPL.
- Supports:
  - `quote`
  - `if`
  - `lambda`
  - Procedure calls (including higher-order functions like `map`)
  - Custom `and` expression handling with short-circuit logic.
- Uses substitution to evaluate lambda expressions.
- Allows primitive procedures (like `+`, `first`, etc.) to be passed as arguments.

## Trace Version

For debugging and learning purposes, a second script `racket1-trace.rkt` includes `displayln` statements to trace the evaluation process step-by-step.

## Guide

Make sure you have [Racket](https://racket-lang.org/) installed.

To start the evaluator:
```bash
racket racket-1.rkt
```

Type in your expression at the prompt:
```racket
Racket-1: ((lambda (x) (* x x)) 5)
25
```

## Example Use Case

Factorial using the Y combinator:
```racket
((lambda (n)
   ((lambda (f) (f f n))
    (lambda (f n)
      (if (= n 0)
          1
          (* n (f f (- n 1))) ))))
 5)
```
Output: `120`
