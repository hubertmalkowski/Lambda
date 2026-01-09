# Lambda

Various typed [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) implementations I did when reading [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/index.html) book.

## Calculus implementations

All implementations at the moment only have a typechecker. Writing interpreters isn't in scope at the moment as I want to focus on type systems.

### [System f](https://en.wikipedia.org/wiki/System_F)

System f is one of the ways to have polymorphism in typed lambda calculus. It works similar to the generics we know from languages like Java.
Basically we add new type of abstractions (functions) that are only for types.

**Run with**
```
dune exec lambda-system-f-bin
```

#### Some examples


**Church numerals**

```
> let zero [A] (f : A -> A) (x : A) = x
∀A (A->A)->A->A

> let one [A] (f : A -> A) (x : A) = f x
∀A (A->A)->A->A

> let two [A] (f : A -> A) (x : A) = f (f x)
∀A (A->A)->A->A

> two [Int]
(Int->Int)->Int->Int
```

*Note: all implementations in this repo supports function definiton shorthand*
*So basically this code:*
```
let zero [A] (f : A -> A) (x : A) = x
```

*Is idiomatic to:*
```
let zero = fun [A] -> fun (f : A -> A) -> fun (x : A) -> x
```


**Church succ**

```
> let church_succ [A] (num : (A -> A) -> A -> A) (f : A -> A) (x : A) = f (num f x)
∀A ((A->A)->A->A)->(A->A)->A->A

> let three [A] = church_succ [A] (two [A])
∀A (A->A)->A->A

> let incr (a : Int) = succ a
Int->Int

> three [Int] incr 0
Int

> three [Bool] incr 0
Type error: Wrong argument type
```

### Let-polymorhism (or [Hinldley-Millner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system))

This one is a little more easy to use for a programmer. It's type system similar to the ones found in Ocaml or Haskell.
In this system we have type reconstruction - so the type signatures in function arguments are optional.

**Run with:**
```
dune exec lambda-poly-bin
```

#### Some examples

**Church numerals**
```
> let zero f x = x
zero : 'b->'c->'c

> let one f x = f x
one : ('e->'f)->'e->'f

> let two f x = f (f x)
two : ('h->'h)->'h->'h

> let incr a = succ a
incr : int->int

> two incr
_ : int->int
```

**Church succ**
```
> let church_succ num f x = f (num f x)
church_succ : (('r->'s)->'p->'r)->('r->'s)->'p->'s

> let three = church_succ two
three : ('z->'z)->'z->'z

> three incr 0
_ : int

> three incr false
Type mismatch
```
