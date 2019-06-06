#lang scheme
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) 
      (accumulate combiner null-value term (next a) next b)
    )
  )
)

(define (sum term a next b) (accumulate + 0 terma a next b))
(define (product term a next b) (accumulate * 1 term a next b))

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

(sum (lambda (num) num) 1 (lambda (num) (+ 1 num)) 10)

(factorial 6)
(pi)