#lang racket

(require math/flonum)

(provide my-sqrt)

; This function applies f to x at least once, which may be unnecessary,
; but one more step does not really slow down the algorithm and only improves the guess.
(define (until2 p? f x)
  (let ((v (f x)))
    (if (p? x v)
        v
        (until2 p? f v))))


(define (square-root-with-accuracy delta)
  (lambda (x)
    ; as per the Babylonian method:
    ;     https://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Babylonian_method
    (define (reinforce y)
      (* 0.5 (+ y (/ x y))))
    ; Racket flonums are IEEE 754 binary double-precision floating-point numbers:
    ;     https://docs.racket-lang.org/reference/numbers.html#%28tech._flonum%29
    ; This means `flexp2` and `fllog2` should be really fast (constant) operations.
    (define initial-guess
      (flexp2 (/ (fllog2 (fl x))
                 2)))

    (until2 (lambda (old-v new-v)
              (< (abs (- old-v new-v)) delta))
            reinforce
            initial-guess)))

(define my-sqrt (square-root-with-accuracy 0.00001))
