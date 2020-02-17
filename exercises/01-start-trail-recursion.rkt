; TODO: givt poll for when to do stuff - https://doodle.com/poll/kiqzmckr4sz4rkmz

; TODO: implement this
(define (sum-interval a b)
  (if (> a b)
      0
      (+ a (sum-interval (+ 1 a) b))))

; TODO: also implement this:
(define (sum-interval-iter a b)
  (define (iter i result)
    (if (> i b)
	    result
	    (iter (+ i 1) (+ i result))))
  (iter a 0))

; TODO: show with time, 1 10000000 is big enough
; talk about cpu and gc time

; EXERCISE: implement a iterative factorial
(define (fact-iter n)
  (define (for i result)
    (if (= i n)
	(* result n)
	(for (+ i 1) (* result i))))
  (for 1 1))

; fibonacci - classic example of hugely ineffective recursive function
; EXERCISE:
(define (fib n)
  (if (or (= n 1) (= n 0))
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

; but do it iterative, by using an inner helper to hold and pass previous values
; EXERCISE:
(define (fib-iter n)
  (define (for i a b)
    (if (= i n)
	a
	(for (+ i 1) b (+ a b))))
  (for 0 0 1))

; EXERCISE: count the digits of a number
; (digits-num 5) -- 1
; (digits-num 123) -- 3
(define (digits-num number)
  (if (< number 10)
      1
      (+ 1 (digits-num (quotient number 10)))))

; implement this however you like
; Exercise: reversing a number
; (reverse-num 0) -- 0
; (reverse-num 123) -- 321
(define (reverse-num n)
  (define (help dc n)
    (if (= dc 1)
	n
	(+ (* (modulo n 10) (expt 10 (- dc 1))) (help (- dc 1) (quotient n 10)))))
  (help (digits-num n) n))

(define (reverse-num-iter n)
  (define (help num result)
    (if (< num 10)
	(+ (* result 10) num)
	(help (quotient num 10) (+ (* result 10) (modulo num 10)))))
  (help n 0))

; EXERCISE: recursive ackermann
; ack(m, n) = n + 1                     if m == 0
; ack(m, n) = ack(m - 1, 1)             if m > 0 and n = 0
; ack(m, n) = ack(m - 1, ack(m, n - 1)) if m > 0 and n > 0
(define (ack m n)
  (if (= m 0)
      (+ n 1)
      (if (= n 0)
	  (ack (- m 1) 1)
	  (ack (- m 1) (ack m (- n 1))))))

; EXERCISE: ackermann but iteratively
(define (ack-iter m n) void)
