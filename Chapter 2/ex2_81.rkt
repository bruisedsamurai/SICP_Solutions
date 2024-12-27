#lang sicp
;; 1. It causes an infinite loop because after finding the result of coercion it calls `apply-generic`
;; which causes a cycle.

;; 2. The procedure works as is. If there is no procedure available for the arguments then it should fail.
(define false #f)

(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2
                          (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2
                          (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr!
                   subtable
                   (cons (cons key-2 value)
                         (cdr subtable)))))
            (set-cdr!
             local-table
             (cons (list key-1
                         (cons key-2 value))
                   (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                          TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond (proc (apply proc (map contents args)))
            ((eq? (car type-tags) (cadr type-tags)) (error "No method for these types" (list op type-tags)))
            (else (if (= (length args) 2)
                      (let ((type1 (car type-tags))
                            (type2 (cadr type-tags))
                            (a1 (car args))
                            (a2 (cadr args)))
                        (let ((t1->t2
                               (get-coercion type1
                                             type2))
                              (t2->t1
                               (get-coercion type2
                                             type1)))
                          (cond (t1->t2
                                 (apply-generic
                                  op (t1->t2 a1) a2))
                                (t2->t1
                                 (apply-generic
                                  op a1 (t2->t1 a2)))
                                (else
                                 (error
                                  "No method for 
                           these types"
                                  (list
                                   op
                                   type-tags))))))
                      (error
                       "No method for these types"
                       (list op type-tags))))))))


(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)

(put-coercion 'complex 'complex
              complex->complex)

(define (exp x y)
  (apply-generic 'exp x y))

(exp 10 1)