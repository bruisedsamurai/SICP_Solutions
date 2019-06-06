#lang scheme

(define (cube term) (* term term term))


(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a )
      (sum term (next a) next b)
    )
  )
)

(define (Simpson-rule f a b n)
  (define (next num) (* 2 (/ (- b a)n)))
  (+ (f a)
    (* (sum f (next a) next b) 2)
    (* 4 (sum f (/(- b a) n) next b)) 
  )
)


(Simpson-rule cube 0 1 100)

(Simpson-rule cube 0 1 1000)