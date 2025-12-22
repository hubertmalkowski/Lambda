# Lambda Calculus Examples

This directory contains example programs demonstrating various features of the lambda calculus interpreter.

## Running Examples

### Strict Evaluation (Call-by-Value)
```bash
dune exec lambda examples/01_basic_arithmetic.lambda
```

### Lazy Evaluation (Call-by-Need)
```bash
dune exec lambda -- --lazy examples/01_basic_arithmetic.lambda
```

## Examples

### 01_basic_arithmetic.lambda
Basic integer operations: successor, predecessor, and zero checking.
- **Expected output**: `5`

### 02_simple_functions.lambda
Simple function definitions and applications including identity and constant functions.
- **Expected output**: `5`

### 03_church_numerals.lambda
Introduction to Church numerals - encoding natural numbers as higher-order functions.
- **Expected output**: `3`

### 04_church_successor.lambda
Implementing the successor function for Church numerals.
- **Expected output**: `3`

### 05_church_addition.lambda
Adding Church numerals using function application.
- **Expected output**: `5`

### 06_conditionals.lambda
Boolean logic and conditional expressions.
- **Expected output**: `42`

### 07_nested_functions.lambda
Higher-order functions including function composition.
- **Expected output**: `7`

### 08_lazy_evaluation.lambda
Demonstrates the difference between strict and lazy evaluation strategies.
- **Expected output**: `103` (both modes, but lazy only computes the taken branch)

## Language Features

The lambda calculus implementation supports:

- **Integers**: `0`, `1`, `42`, etc.
- **Booleans**: `true`, `false`
- **Lambda expressions**: `fun x -> expr`
- **Function application**: `f x`
- **Let bindings**: `let x = value in body`
- **Conditionals**: `if cond then expr1 else expr2`
- **Arithmetic**: `succ n`, `pred n`
- **Predicates**: `iszero n`

## Evaluation Strategies

### Strict Evaluation (Default)
Call-by-value: Arguments are evaluated before being passed to functions.

### Lazy Evaluation (--lazy flag)
Call-by-need: Arguments are wrapped in thunks and only evaluated when their values are needed. Results are memoized to avoid repeated computation.
