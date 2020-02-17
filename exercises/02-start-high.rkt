; TODO: talk about homework, my failure re HW, but still!
; there is homework hopefully or worst case tomorrow!
; TODO: talk about additional exercise, probably sunday

; TODO: talk about FP and higher order functions
; *extremely* convenient, all you do is some form of these

(define (apply-twice f x) (f (f x)))
; TODO: lambda for succ, explain lambda
;(== (apply-twice succ 2) 4)
; TODO: talk about how define fn is sugar for lambda + define
; show example
(define (apply-thrice f)
  (lambda (x)
    (f (f (f x)))))

; TODO: example for returning functions
(define (apply-twice-curried f)
  (lambda (x)
    (f (f x))))

; EXERCISE: const
; Generally useful function - return a function that always returns the first argument
; EXAMPLES:
; ((const 5) 10) -- 5
; ((const 0) 69) -- 0
; ((const 42) 80085) -- 42
(define (const x)
  (lambda (y) x))

; EXERCISE: add-n
; A function that takes a number n and returns a function that takes a number (m)
; and adds n to it.
; EXAMPLES:
; (define add-10 (add-n 10)
; (add-10 5) -- 15
;
; ((add-n 10) 13) -- 23
(define (add-n n)
  (lambda (m) (+ m n)))

; EXERCISE: compose
; Generally useful function - compose two functions
; i.e. first apply one and then the other
; EXAMPLES:
; (define (succ n) (+ 1 n))
; ((compose succ succ) 10) -- 12
; ((compose (lambda (x) (expt 2 x)) succ) 9) -- 1024
; ((compose succ (lambda (x) (expt 2 x))) 9) -- 513
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (id x) x)

; EXERCISE: iterate
; Let's apply a function n times. This is actually quite useful.
; EXAMPLES:
; (iterate 10 succ 5) -- 15
(define (iterate n f)
  (if (= n 0)
      id
      (compose f (iterate (- n 1) f))))

; EXERCISE: iterate-curried
; Let's create a *new* function which applies f n times to it's argument
; EXAMPLES:
; ((iterate 10 succ) 5) -- 15
(define (iterate-curried n f)
  (if (= n 0)
      id
      (compose f (iterate (- n 1) f))))

; EXERCISE: my-plus-iterate
; Use iterate/iterate-curried to define addition, as we've done before
; HINT: It might help to first define it recursively, to see what operation you are iterating
; and what value you are iterating upon.
; EXAMPLES:
; (my-plus-iterate 13 56) -- 69
(define (1+ n) (+ 1 n))

(define (my-plus-iterate x y)
  ((iterate-curried x 1+) y))


; EXERCISE: my-mult-iterate
; Use iterate/iterate-curried to define multiplication, as we've done before
; HINT: It might help to first define it recursively, to see what operation you are iterating
; and what value you are iterating upon. You will need a lambda/inner definition here.
; EXAMPLES:
; (my-plus-iterate 16 16) -- 256
(define (my-mult-iterate x y)
  ((iterate-curried x (lambda (z) (+ y z))) 0))

; EXERCISE: fixpoint?
; Define a function fixpoint? f n, which checks whether n is a fixpoint of n
; (in other words whether f n == n)
;
; EXAMPLES:
; (fixpoint? (const 10) 0) -- #f
; (fixpoint? (const 10) 10) -- #t
(define (fixpoint? f n) (= (f n) n))

; EXERCISE: fixpoint
; Define a function fixpoint f, which finds a fixpoint for f, if there is one
;
; (fixpoint (const 100)) -- 100
; (fixpoint (lambda (x) (if (< x 10) (+ 1 x) 11))) -- 11
(define (fixpoint f)
  (define (for i)
    (if (fixpont? f i)
        i
        (for (1+ i))))
  (for 0))

; EXERCISE: fold-interval
; we saw this function last time
(define (sum-interval x y)
  (if (> x y)
      0
      (+ x (sum-interval (+ 1 x) y))))

; but we can also write this one
(define (prod-interval x y)
  (if (> x y)
      1
      (* x (prod-interval (+ 1 x) y))))

; and by "write" I mean I copy-pasted it
; they are *that* similar!
; write a function that generalizes over them
; by getting a null value nv which to return in the base case
; and a function which to apply in the recursive case
; let's make it return a lambda straight away so we can reuse it for {sum,prod}-interval
; HINT: Again, the trick here is to find the "common" parts of the previous two definitions.
; You will need to return a lambda.
; EXAMPLE:
; (fold-interval + 5 20 25) -- 140
(define (fold-interval f nv a b)
  (if (> a b)
      nv
      (f a (fold-interval f nv (1+ a) b))))

; now use it to define the previous two we've already seen
(define (sum-interval-fold a b)
  (fold-interval + 0 a b))
(define (prod-interval-fold a b)
  (fold-interval * 1 a b))

; along with some new ones

; EXERCISE: max-val-interval-fold
; Using fold-interval find the largest value of a function in an interval
; HINT: use the max builtin and assume the functions we are given always return natural numbers
; EXAMPLES:
; (define (weird-fn x)
;   (if (even? x)
;       (* x x)
;       (+ 1 x)))
; ((max-interval-fold weird-fn) 5 10) -- 100
(define (max-val-interval-fold f a b)
  (fold-interval (lambda (x acc) (max (f x) acc)) 0 a b))

; Using fold-interval find whether all the numbers in an interval satisfy a predicate p?
; EXAMPLES:
; (define (>10? n) (> n 10))
; ((all >10?) 8 20) -- #f
; ((all >10?) 10 20) -- #t
(define (all p? a b)
  (fold-interval (lambda (x r) (and (p? x) r)) #t a b))

; Let's think about "repeated application of a function to a value"
; We will represent these as lambdas that take a function f and a value v
; and apply f to v some number of times (the number of f's)
; We can have a "zero times application" of a function to a value - that's just the value itself.
(define zero (lambda (f v) v))

; Here is the "one time application" and "two time application" for examples
(define one (lambda (f v) (f v)))
(define two (lambda (f v) (f (f v))))

; Let's now implement a function that takes a "n-times application" and returns a "n+1 times application"
(define (succ n)
  (lambda (f v) ; since we're returning another "application" we need to write a lambda accepting our f and v
    (f (n f v)))) ; to apply f n+1 times, we simply need to apply it n times, and then apply it once more

; Now using these tools implement the following
; Read the *IMPORTANT* below, after reading both from-num and to-num's comments
; but before implementing them

; EXERCISE: from-num
; Write a function that takes a number and then constructs the "n-times application"
; HINT: Use succ and zero.
; EXAMPLES:
; ((from-num 5) (lambda (x) (+ 1 x)) 7) -- 12
(define (from-num n)
  (fold-interval (lambda (_ r) (succ r)) zero 1 n))

; EXERCISE: to-num
; Write a function that takes the "n-times application" and returns the number n
; EXAMPLES:
; (to-num one) -- 1
; (to-num two) -- 2
; (to-num (succ (succ two))) -- 4
; (to-num (lambda (f v) (f (f (f v))))) -- 3
(define (to-num n)
  (n 1+ 0))

; IMPORTANT:
; For all naturals n it should hold that
; (to-num (from-num n)) -- n
; And for all "n-time applications" that
; (from-num (to-num n)) -- n
; Wait a second.. if these are convertible between each other
; and we don't lose any information, doesn't that mean thay are interchangable?
; So you can use them instead of natural numbers. And to boot they only use lambdas
; In conclusion - you don't need natural numbers in your language if you have lambdas!

; More exercises

; EXERCISE:
; If they really are natural numbers we should be able to define addition and multiplication.
; We can think of these as
;  If I know how to apply f to v n times
;  and I also know how to apply f to v m times,
;  I ought to be able to know how to apply f to v (n + m) times
; Same with multiplication
; You are *not* allowed to use {from,to}-num here.

; EXERCISE: plus
; EXAMPLES:
; (to-num (plus (from-num 20) (from-num 10))) -- 30
; ((plus (from-num 20) (from-num 10)) (add-n 30) 69) -- 969
(define (plus n m)
  (lambda (f v)
    (n f (m f v))))

; EXERCISE: mult
; EXAMPLES:
; (to-num (mult (from-num 10) (from-num 13))) -- 130
; (to-num (mult (succ (succ zero)) (succ (succ zero)))) -- 4
(define (mult n m)
  (m (lambda (v) (plus n v)) zero))

; Difficult bonus, give up: pred
; Write a function that takes the "n-times application" and gives back the "n-1 times application")
; (without using {from,to}-num)
;
; EXAMPLES:
; (to-num (pred one)) -- 0
; (to-num (pred two)) -- 1
(define (pred n)
  (n (lambda (v) 
       (if (eq? v '-1)
           zero
           (succ v)))
     '-1))
