#lang racket

(provide coord-matrix
         unmarked-positions
         best-outcome
         winner
         play)

(require "matrix.rkt")

(define (id x) x)

(define (all-eqv?-1 xs)
  (all? (lambda (x) (eqv? x (car xs)))
        (cdr xs)))

(define (winner board)
  (or (any? (lambda (xs)
              (and (all-eqv?-1 xs)
                   (car xs)))
            (append (rows board)
                    (cols board)
                    (diags board)))
      (and (andmap (lambda (xs) (andmap id xs)) board)
           "D")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (curry2 f)
  (lambda (x)
    (lambda (y)
      (f x y))))

(define (cartesian-product xs ys)
  (if (null? ys)
      ys
      (map (lambda (x)
             (map ((curry2 cons) x)
                  ys))
           xs)))

(define (from-to a b)
  (if (> a b)
      '()
      (cons a (from-to (+ a 1) b))))

; returns a matrix of `n` rows and `m` columns
; where each element is a pair of its row and column index (coordinates)
(define (coord-matrix n m)
  (cartesian-product (from-to 0 (- n 1)) (from-to 0 (- m 1))))

; returns a list of pairs of coordinates `x` `y`,
; which correspond to unmarked positions on the board,
; i.e. for which (matrix-ref board x y) is equal to #f
(define (unmarked-positions board)
  (map cdr (filter (lambda (p)
                     (not (car p)))
                   (concat (zip-matrix board (coord-matrix 3 3))))))

; returns the sign of the player
(define (get-sign me) ; player1 is #t, player2 is #f
  (if me "X" "O"))

(define (foldl f v xs)
  (if (null? xs)
      v
      (foldl f (f v (car xs)) (cdr xs))))

(define (extremum-on less? f xs)
  (foldl (lambda (acc x)
           (if (less? (f acc) (f x))
               acc
               x))
         (car xs) (cdr xs)))

(define (minimum xs)
  (foldl min (car xs) (cdr xs)))

(define (grade board)
  (case (winner board)
    (("X")  1)
    (("D")  0)
    (("O") -1)
    (else  #f)))

; ps are the unmarked positions on the board left to be evaluated
(define (improve alpha beta ps board me)
  (if (or (null? ps) (>= alpha beta))
      (if me alpha beta)
      (let ((value (best-outcome alpha
                                 beta
                                 (car ps)
                                 board
                                 me)))
        (if me
            (improve (max alpha value)
                     beta
                     (cdr ps)
                     board
                     me)
            (improve alpha
                     (min beta value)
                     (cdr ps)
                     board
                     me)))))

;;  the best outcome for when `me` plays his sign on position `p`
;;  in general, if `me` is #t, the best possible value on empty board is 1
;;  otherwise the best value is -1
; assumes the position `p` on `prev-board` is empty
(define (best-outcome alpha beta p prev-board me)
  (define board (place prev-board (car p) (cdr p) (get-sign me)))
  (or (grade board)
      (let ((ps (unmarked-positions board)))
        (improve alpha beta ps board (not me)))))
  

; returns a pair of indexes on the board - the next best play for that player
; assumes there is at least one unmarked position, i.e., a play could be made
(define (play board me) ; player1 "X" is #t, player2 "O" is #f
  (cdr (extremum-on (if me > <)
                    car
                    (map (lambda (p)
                           (cons (best-outcome -inf.0 +inf.0 p board me)
                                 p))
                         (unmarked-positions board)))))
