#lang racket

(provide coord-matrix
         unmarked-positions
         worst-outcome
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
(define (get-sign player) ; player1 is #t, player2 is #f
  (if player "X" "O"))

(define (negate x) (- x))

(define (maximum-by max* xs)
  (foldr max* (car xs) (cdr xs)))

(define (minimum xs) (maximum-by min xs))

(define (place2 xss ij y)
  (place xss (car ij) (cdr ij) y))

; evaluates the board after player has made his play
; always returns one of {-1, 0, 1}
; if the game is not finished, continues to play-out the game
; and finds the value of the worst possible outcome
(define (worst-outcome board player) ; player1 is #t, player2 is #f
  (let ((result (winner board)))
    (cond
      ((eqv? result (get-sign player)) 1)
      ((eqv? result "D") 0)
      (result -1)
      (else ; the game has not ended
        (minimum (map (lambda (ij)
                        (negate
                          (worst-outcome (place2 board ij (get-sign (not player)))
                                         (not player))))
                      (unmarked-positions board)))))))

; returns a pair of indexes on the board - the next best play for that player
; assumes there is at least one unmarked position, i.e., a play could be made
(define (play board player) ; player1 is #t, player2 if #f
  ; the best play is the one whose worst outcome is the biggest
  (maximum-by (lambda (ij1 ij2)
               (define (value ij)
                 (worst-outcome (place2 board ij (get-sign player))
                                player))
               (if (< (value ij1) (value ij2))
                   ij2
                   ij1))
             (unmarked-positions board)))
