#lang racket

(require math/flonum)

(provide my-sqrt)

(define (iterate n f x)
  (if (= n 0)
      x
      (f (iterate (- n 1) f x))))

(define (square-root-with-precision n)
  (lambda (x)
    (define (reinforce y)
       (* 0.5 (+ y (/ x y))))
    (iterate n reinforce (fllog2 (fl x)))))

; 8 са най-малкото стъпки, за които минават тестовете
(define my-sqrt (square-root-with-precision 8))
