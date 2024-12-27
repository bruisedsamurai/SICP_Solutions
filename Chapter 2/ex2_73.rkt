#lang sicp

; Dispatching that was done based on the type of expressions is made generic over the type of expression
; (but only for product and sum)
; The reason it's not easy to do it for number or variable is because identifier in the expressions of
; number and variables. They are single values passed to the procedure from which their type cannot be
; extracted.


(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (addend s) (car s))
(define (augend s) (cadr s))
(define (multiplier p) (car p))
(define (multiplicand p) (cadr p))


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


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(define (base expr) (car expr))
(define (exponent expr) (cadr expr))
(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list '** base exp))
        ))

(define (install-derive-package)
  (define (derive-product exp var) (make-sum
                                    (make-product
                                     (multiplier exp)
                                     (deriv (multiplicand exp) var))
                                    (make-product
                                     (deriv (multiplier exp) var)
                                     (multiplicand exp))))
  (define (derive-sum exp var) (make-sum (deriv (addend exp) var)
                                         (deriv (augend exp) var)))
  (define (derive-exp exp var) (make-product (exponent exp) (make-exponentiation (base exp) (make-sum (exponent exp) (- 1)))))
  (put '* 'deriv derive-product)
  (put '+ 'deriv derive-sum)
  (put '** 'deriv derive-exp)
  )

(install-derive-package)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
             1
             0))
        (else ((get (operator exp) 'deriv)
               (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(display (deriv '(** x 9) 'x))(newline)