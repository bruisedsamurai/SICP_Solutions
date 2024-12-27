#lang sicp

(define (equal? a b)
  (cond
    ((and (pair? a) (pair? b))  (if (eq? (car a) (car b))
                                    (equal? (cdr a) (cdr b))
                                    false))
    ((and (not (pair? a)) (not (pair? b))) (and (eq? a '()) (and (eq? b '()))))
    (else false)
    )
  )

(equal? '(this is a list)  '(this (is a) list))
