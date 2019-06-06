#lang sicp

(define (square x) (* x x))

(define (is_even num) (= (remainder num 2) 0))

(define (fast_exp_iter val prepend intermid pow) (
                                         cond
                                         ((= pow 1) (* prepend intermid))
                                         ((is_even pow) (fast_exp_iter (square intermid)prepend (square intermid) (/ pow 2)))
                                         (else (fast_exp_iter val (* prepend val) intermid (- pow 1)))
                                         )
  )

(define (fast_exp val pow) (fast_exp_iter val 1 val pow))

(fast_exp 2 8)
(fast_exp 4 4)
(fast_exp 9 7)
(fast_exp 9 17)
(fast_exp 12 31)
