#lang racket

(provide from-numeral
         to-numeral
         plus
         mult
         pred)

(define zero (lambda (f v) v))

(define (succ n)
  (lambda (f v)
    (f (n f v))))

(define (1+ n) (+ 1 n))

(define (from-numeral n)
  (n 1+ 0))

(define (to-numeral n)
  (if (= n 0)
      zero
      (succ (to-numeral (- n 1)))))

(define (plus n m)
  (n succ m))

(define (mult n m)
  (n (lambda (x) (plus m x)) zero))

(define (tt x y) x)
(define (ff x y) y)
(define (if* p x y) (p x y))

(define (zero? n)
  (n (lambda (x) ff) tt))

(define (fst xy) (xy tt))
(define (snd xy) (xy ff))
(define (mk-pair x y)
  (lambda (p)
    (p x y)))

(define (pred n)
  (define (inc-pair xy)
    (if* (zero? (snd xy))
         (mk-pair (fst xy)
                  (succ (snd xy)))
         (mk-pair (succ (fst xy))
                  (snd xy))))
  (fst (n inc-pair (mk-pair zero zero))))
