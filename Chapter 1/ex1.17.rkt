#lang sicp

(define (is_even num) (= (remainder num 2) 0))

(define (double num) (+ num num))

(define (halve num) (/ num 2))

(define (* a b) (
                 cond
                 ((= b 1) a)
                 ((is_even b) (double (* a (halve b))))
                 (else (+ a (* a (- b 1))))
                 )
  )

(* 7 6)
(* 9 7)
(* 8 8)
(* 64 71)
