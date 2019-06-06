#lang sicp

(define (is_even x) (= (remainder x 2) 0))

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (iter_mul a b intmid) (
                               cond
                               ((= b 1) (+ a intmid))
                               ((is_even b) (iter_mul (double a) (halve b) intmid))
                               (else (iter_mul a (- b 1) (+ intmid a)))
                               )
  )

(define (* a b) (iter_mul a b 0))

(* 7 6)
(* 9 7)
(* 13 21)
(* 21 43)
