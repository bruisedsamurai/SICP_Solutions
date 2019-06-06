#lang sicp

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ 1 test-divisor)))
        )
  )

(define (square num) (* num num))
(define (divides? divisor num) (= (remainder num divisor) 0))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)
