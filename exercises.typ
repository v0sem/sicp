= Chapter 1: Building Abstractions with Procedures

== Exercise 1.1

- `10`
- `(+ 5 3 4)` $->$ `12`
- `(- 9 1)` $->$ `8`
- `(/ 6 2)` $->$ `3`
- `(+ (* 2 4) (- 4 6))` $->$ `6`
- ```clj (define a 3) ``` $->$ Stores 3 into var _a_
- ```clj (define b (+ a 1)) ``` $->$ Stores 4 `(+ 3 1)` into var _b_
- `(+ a b (* a b))` $->$ `19`
- `(= a b)` $->$ `NIL`
- ```clj
  (if (and (> b a) (< b (* a b)))
      b
      a)``` 
  ~~~~~~~~~$arrow.r.curve$ `4`
- ```clj
  (cond ((= a 4) 6)
        ((= b 4) (+ 6 7 a)
        (else 25)))```
  ~~~~~~~~~~~~$arrow.r.curve$ `16`
- `(+ 2 (if (> b a) b a))` $->$ `6`
- ```clj
  (* (cond ((> a b) a)
           ((< a b) b)
           (else -1))
      (+ a 1))```
  ~~~~~~~~~$arrow.r.curve$ `16`

== Exercise 1.2

```clj
(/ (+ 5 4 (- 2 
             (- 3 
                (+ 6 
                   (/ 4 5))))) 
   (* 3 
      (- 6 2)
      (- 2 7)))
```
== Exercise 1.3

```clj
(define ex1.3 (x y z) 
      (cond ((> x y) 
              (if (> y z)
                  (+ (* x x) (* y y))
                  (+ (* x x) (* z z))))
            (t
              (if (> x z)
                  (+ (* y y) (* x x))
                  (+ (* y y) (* z z))))))
```

== Exercise 1.4

The function `a-plus-abs-b` utilizes the if condition to change the operation to a sum if b is positive or a substraction otherwise, acting as $|b|$. 

Mathematically:

#set math.cases(gap: 1em)
$"a-plus-abs-b"(a, b) = cases(
  a + b "if" b > 0,
  a - b "if" b < 0,
) eq.triple a + |b|$

== Exercise 1.5

With an applicative oreder evaluation, the test function will not run properly because `(p)` will loop on itself, continiously running `(test 0 (p))`. Using normal order evaluation, because $y$ is not utilized on the `test` function, the `if` clause will be executed and resolve to $0$.

== Exercise 1.6

The new if does not work in the `sqrt-iter` function, it throws a _stack overflow_ type error.

This is because the special form `if` runs in applicative order, thus evaluating the predicate and only running `then` or `else` when needed. In the case of `new-if`, because of the recursive call, it will be stuck evaluating that.

== Exercise 1.7

Trying out the newton method, on very low numbers ($0.0001$) returns not very accurate results, compared to an actual square root method, comparing it with the common lisp `sqrt`:

- ```clj (sqrt 0.0001)``` $-> 0.01$
- ```clj (newton-sqrt 0.0001)``` $-> 0.032308448$

Now, with large numbers, what happens is that the number of operations exponentially increases and gets stuck evaluating. So, if we were to try and fix the first issue with smaller numbers, making our `good-enough?` function use a lower boundary, we would eventually reach the second problem, getting stuck in recursion.