#lang sicp

(define (element-of-set? elem set)
  (cond ((null? set) false)
        ((equal? elem (car set)) true)
        (element-of-set? elem (cdr set))
        )) ;O(n)
 
(define (adjoin-set elem set) (cons elem set)) ; O(1)

(define (intersection-set set1 set2)
  (cond  ((or (null? set1) (null? set2)) '())
         ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) set2)))
         (else (intersection-set (cdr set1) set2)))) ;O(n)^2

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (cons (car set1) (union-set (cdr set1) set2))))) ;O(n+m)
; it can be used for situations where we need a certain bias  
