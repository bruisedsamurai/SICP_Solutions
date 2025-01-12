#lang sicp
(define false #f)

(define (attach-tag type-tag contents)
  (cons type-tag contents))

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


(define raise-table (make-table))
(define get-raise (raise-table 'lookup-proc))
(define put-raise (raise-table 'insert-proc!))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ '(scheme-number scheme-number)
       (lambda (x y) (= x y)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'raise 'scheme-number
       (lambda (x) (make-rational x 1)))
  (put '=zero? '(scheme-number) (lambda (num) (= 0 num)))
  'done)

(install-scheme-number-package)
(define (make-scheme-number num) ((get 'make 'scheme-number) num))


(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ x y) (and (= (numer x) (numer y)) (= (denom x) (denom y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise 'rational
       (lambda (num) (make-real (/ (numer num) (denom num))))) ; installation of generic raise
  (put 'equ '(rational rational) equ)
  (put '=zero? '(rational) (lambda (num) (and (= (numer num) 0) (not (= (denom num) 0)))))
  'done)
(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (define (make-real num) (* num 1.0))
  (put 'make 'real (lambda (num) (tag (make-real num)))))
(install-real-package)

(define (make-real num) ((get 'make 'real) num))

(define (raise datum) (let ((get-raise (get 'raise (type-tag datum))))
                        (display datum)
                        (display "\t")
                        (display get-raise)
                        (newline)
                        (if get-raise
                            (get-raise (contents datum))
                            #f)) ) ; generic raise operation

(define (raise-recursive datum1 type2) (let ((raised (raise datum1)))
                                         (cond ((eq? raised #f) #f)
                                               ((eq? (type-tag raised) type2) raised)
                                               (else (raise-recursive raised type2)))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2
                       (raise-recursive a1
                                        type2))
                      (t2->t1
                       (raise-recursive a2
                                        type1)))
                  (cond (t1->t2
                         (apply-generic
                          op t1->t2  a2))
                        (t2->t1
                         (apply-generic
                          op a1 t2->t1))
                        (else
                         (error
                          "No method for 
                           these types"
                          (list
                           op
                           type-tags))))))
              (error
               "No method for these types"
               (list op type-tags)))))))


(define (add x y) (apply-generic 'add x y))

(display (raise (make-rational 1 2.0)))
(newline)
(display (apply-generic 'add (make-real 1.0) (make-rational 10 12)))
(newline)
