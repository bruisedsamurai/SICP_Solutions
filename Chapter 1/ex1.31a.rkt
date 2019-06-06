#lang scheme
(define (product term a next b) 
  (if (> a b)
    1
    (* (term a)
      (product term (next a) next b)
    )
  )
)

(define (factorial num)
  (define (term a) a)
  (define (next num) (+ 1 num))
  (product term 1 next num)
)

(define (pi)
  (define (next a) (+ a 2))
  (define (term a) a)
  (define (square a) (* a a))
  (* 4 
    (/ (* 2 (square (product term 4 next 10000))) (square (product term 3 next 10000)))
  )
)

(factorial 6)