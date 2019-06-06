#lang scheme

(define (accumulate combiner null-value term a next b)
  (define (iter_acc num result)
    (if (> num b)
      result
      (iter_acc (next num) (combiner result (term num)))
    )
  )
  (iter_acc a null-value)
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