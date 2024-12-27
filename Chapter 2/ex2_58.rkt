#lang sicp


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

; (define (sum? expr) (and (pair? expr) (eq? (cadr expr) '+)))
(define (unpack-recursive expr op) (cond
                                     ((number? expr) expr)
                                     ((null? (cdr expr)) (unpack-recursive (car expr) op)) 
                                     (else (op expr))))

(define (sum? expr) (and (pair? expr)  (eq? (cadr expr) '+)))
(define (product? expr) (and (pair? expr) (eq? (cadr expr) '*)))

(define (addend expr) (car expr))
(define (augend expr) (cddr expr))

(define (multiplier expr) (car expr))
(define (multiplicand expr) (cddr expr))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((and (pair? exp) (null? (cdr exp))) (deriv (car exp) var))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        (else (error "unknown expression type: DERIV" exp))))

; (deriv '(x + (3 * (x + (y + 2)))) 'x)
(addend '((x + y + 2)))

(deriv '(3 * (x + y + 2) + x) 'x)
(deriv '(x + 3 * (x) + (x + y + 2)) 'x)
