# Samara

This is an interpreter for a small pure functional programming language based on the simply-typed lambda calculus with [type inference](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_inference). Syntax is loosely based on [Elm](http://elm-lang.org/).

## Setup

You will need `cargo` and `rustc` installed. You can get them [here](https://www.rust-lang.org/en-US/install.html) using the `rustup` tool.

## Tests

`cargo test` will run all tests. `cargo test --test skeptic` will only test the examples in this file.

## Usage

Clone this repository and navigate to this directory in a terminal. Start the REPL with
```
$ cargo run
```

Within the REPL, you can declare a new type or enter an expression to evaluate. Aside from this, all commands begin with `:`: `:exit` to exit the REPL, `:type` followed by an expression to ask the type of that expression, and `:help` for help.

## Examples

### Arithmetic

For now, integers are the only supported numeric type. Built-in arithmetic operations can be used with Scheme-style prefix notation:

```rust,skt-repl
> + 5 5
10
> * (// 18 3) (- (+ 4 5) (% 6 4))
42
```

### Conditionals

```rust,skt-repl
> if (> 100 0) then True else False
True
```

### Functions

Functions are expressed as lambdas. All functions are curried and take exactly one argument.

```
> (\x -> * x x) 5
25
> (\x -> \y -> - x y) 5
\y -> (- 5 y)
> (\x -> \y -> - x y) 5 10
-5
```

A value bound as the argument of a lambda must have the same concrete type throughout the body of the expression. For instance the identity function can be applied to an `Int` or a `Bool`, but not both:

```
/*
> (\f -> f 5) (\x -> x)
5
> (\f -> f True) (\x -> x)
True
> (\f -> (if f True then f 5 else f 0)) (\x -> x)
Type error: Bool != Int
*/
```

### Let expressions

Can be used to define a variable within the body of a particular expression.

```rust,skt-repl
> let x = 10 in < x 5
False
```

This is the only way to define a recursive function at present:

```rust,skt-repl
> let fact = (\x -> (if (< x 1) then 1 else * x (fact (- x 1)))) in fact 8
40320
```

A value bound in a `let` expression can be used polymorphically within the body of the expression. Here the identity function is applied to both an `Int` and a `Bool`:

```rust,skt-repl
> let id = (\x -> x) in (if id True then id 5 else id 0)
5
```

A let expression without a body will continue the binding for the rest of the REPL session:

```
/*
> let id = \x -> x
> if id True then id 5 else id 0
5
> let fact = (\x -> (if (< x 1) then 1 else * x (fact (- x 1))))
> fact 8
40320
*/
```

### Algebraic data types (aka union types)

You can define sum and product types in the REPL, and use values of these types via pattern matching. I haven't implemented proper exhaustiveness checking yet, so case expressions must start with a default value immediately following the `of`.

```
/*
> type Maybe a = Just a | Nothing
> let foo = \x -> case x of 100; Just Nothing -> 5; Just (Just y) -> (* y y);
> foo Nothing
100
> foo Just Nothing
5
> foo (Just (Just 8))
64
> type Pair a b = Pair a b
> case (Pair 7 6) of 5; Pair y z -> - y z
1
*/
```

Recursive types, eg `List a = Nil | Cons a (List a)` are not yet supported.

### Multi-line expressions

Whitespace does not affect evaluation. End a line with `\` to continue the expression on the next line.

```
/*
> if (> 100 0) \
| then True \
| else False
True
*/
```

## Upcoming features

* Exhaustiveness checking in case expressions
* Recursive types
* Record types
* Infix operators

## The name

Botanically, a [samara](https://en.wikipedia.org/wiki/Samara_(fruit)) is a seed surrounded by a papery wing allowing it to be carried by the wind. Maple, ash, and in particular elm trees all produce these.
