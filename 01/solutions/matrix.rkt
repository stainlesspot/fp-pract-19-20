#lang racket

(provide all?
         any?
         concat
         rows
         cols
         matrix-ref
         set
         place
         diag
         diags
         map-matrix
         filter-matrix
         zip-with
         zip-matrix)
; the provide "exports" these functions

; 00.
(define (all? p? xs)
  (not (any? (lambda (x)
               (not (p? x)))
             xs)))

; 01.
(define (any? p? xs)
  (and (not (null? xs))
       (or (p? (car xs))
           (any? p? (cdr xs)))))

; 02.
(define (concat xss)
  (apply append xss))

; 03.
(define (rows xss)
  xss)

; 04.
(define (cols xss)
  (apply map list xss))

; 05.
(define (matrix-ref xss i j)
  (list-ref (list-ref xss i) j))

; 06.
;;; slower
;;(define (set xs i y)
;;  (append (take xs i)
;;          (list y)
;;          (drop xs (+ 1 i))))

(define (set xs i y)
  (cond ((null? xs) xs)
        ((= i 0) (cons y xs))
        (else (cons (car xs) (set (cdr xs) (- i 1) y)))))

; 07.
(define (place xss i j y)
  (set xss i (set (list-ref xss i) j y)))

; 08.
(define (diag xss)
  (if (null? xss)
      xss
      (cons (caar xss)
            (diag (map cdr (cdr xss))))))

; 09.
(define (diags xss)
  (list (diag xss) (diag (map reverse xss))))

; 10.
(define (map-matrix f xss)
  (map (lambda (xs)
         (map f xs))
       xss))

; 11.
(define (filter-matrix p? xss)
  (map (lambda (xs)
         (filter p? xs))
       xss))

; 12.
;;; more verbose than just using `map`
;; (define (zip-with f xs ys)
;;   (if (or (null? xs) (null? ys))
;;       '()
;;       (cons (f (car xs) (car ys))
;;             (zip-with f (cdr xs) (cdr ys)))))

; That's just how map works in scheme
(define zip-with map)

; 13.
(define (zip-matrix xss yss)
  (zip-with (lambda (xs ys)
              (zip-with cons xs ys))
            xss
            yss))
