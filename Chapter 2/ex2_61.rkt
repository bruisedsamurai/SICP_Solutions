#lang sicp

(define (element-of-set? x set)  (cond ((null? set) false)
                                       ((= x (car set)) true)
                                       ((< x (car set)) false)
                                       (else (element-of-set? x (cdr set)))))

(define (insert-rec elem set) (cond
                                ((equal? set '()) (cons elem set))
                                ((> elem (car set)) (cons (car set) (insert-rec elem (cdr set))))
                                (else (cons elem set))))

(define (adjoin-set elem set) (if (element-of-set? elem set) set (insert-rec elem set)))

(adjoin-set 1 (adjoin-set 10 (adjoin-set 9 '())))
