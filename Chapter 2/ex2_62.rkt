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

(define (union-set set1 set2) (cond ((null? set1) set2)
                                     ((null? set2) set1)
                                     (else (let ((x1 (car set1)) (x2 (car set2))) (cond
                                                                                    ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                                                                                    ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                                                                                    ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
                                                                                    )))
                                     ))

(union-set (adjoin-set 16 (adjoin-set 12 '())) (adjoin-set 11 '()))
