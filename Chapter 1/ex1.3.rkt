#lang sicp
(define x 4)
(define y 6)
(define z 8)
(define (square x) (* x x))
(define (find_largest x y) (if (> x y) x y))
(define (find_smallest x y) (if (< x y) x y))
(define (sum_sq_three x y z) (+
                              (square
                               (find_largest x y)
                               )
                              (square
                               (find_largest (find_smallest x y) z)
                               )
                              ))
(sum_sq_three x y z)
