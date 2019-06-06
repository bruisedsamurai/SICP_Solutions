#lang scheme
(define (iterative-product term a next b)
  (define (iter a result) 
    (if (> a b)
      result
      (iter (next a) (* result (term a)))
    )
  )
  (iter a 1)
)

(define (factorial num)
  (define (term a) a)
  (define (next num) (+ 1 num))
  (iterative-product term 1 next num)
)

(define (pi)
  (define (next a) (+ a 2))
  (define (term a) a)
  (define (square a) (* a a))
  (* 4 
    (/ (* 2 (square (iterative-product term 4 next 10000))) 
            (square (iterative-product term 3 next 10000)))
  )
)

(factorial 6)