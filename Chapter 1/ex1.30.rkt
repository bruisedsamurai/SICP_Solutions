#lang scheme

(define (cube term) (* term term term))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))
    )
  )
  (iter a 0)
)


(sum (lambda (num) num) 1 (lambda (num) (+ 1 num)) 10)

(define (Simpson-rule f a b n)
  (define (next num) (* 2 (/ (- b a)n)))
  (+ (f a)
    (* (sum f (next a) next b) 2)
    (* 4 (sum f (/(- b a) n) next b)) 
  )
)


(Simpson-rule cube 0 1 100)

(Simpson-rule cube 0 1 1000)