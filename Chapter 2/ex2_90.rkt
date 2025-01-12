#lang sicp
(define false #f)

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (variable? x) (symbol? x))

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
  (define (tag x) x)
  ; (attach-tag 'scheme-number x))
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
  (put 'sine '(scheme-number) (lambda (num) (sin num)))
  (put 'cosine '(scheme-number) (lambda (num) (cos num)))
  (put 'negate '(scheme-number) (lambda (num) (- num)))
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
  (define (project num) (make-scheme-number (round (/ (numer num) (denom num)))))
  (define (sine num) (sin (/ (numer num) (denom num))))
  (define (cosine num) (cos (/ (numer num) (denom num))))
  (define (negate-rational num) (make-rational (negate (numer num)) (denom num)))
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
  (put 'project '(rational) project)
  (put 'sine '(rational) sine)
  (put 'cosine '(rational) cosine)
  (put 'negate '(rational) negate-rational)
  'done)
(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (define (make-real num) (* num 1.0))
  (define (equ x y) (= x y))
  (define (project num) (make-rational num 1))
  (define (negate-real num) (- num))
  ;;Interface for outer world
  (put 'make 'real (lambda (num) (tag (make-real num))))
  (put 'equ '(real real) equ)
  (put 'project '(real) project)
  (put '=zero? '(real) (lambda (num) (= 0 num)))
  (put 'negate '(real) negate-real)
  )
(install-real-package)
(define (make-real num) ((get 'make 'real) num))

(define (square num) (mul num num))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (add (square (real-part z))
               (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))
  (define (equ x y) (and (equ? (real-part x) (real-part y)) (equ? (imag-part x) (imag-part y))))
  (define (project num) (make-real (real-part num)))
  (define (negate-rect num)
    (make-complex-from-real-imag (negate (real-part num)) (negate (imag-part num))))
  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'equ '(rectangular rectangular) equ)
  (put '=zero? '(rectangular) (lambda (num) (and (= (real-part num) 0) (= (imag-part num) 0))))
  (put 'project '(rectangular) project)
  (put 'negate '(rectangular) negate-rect)
  'done)

(install-rectangular-package)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (add (square x) (square y)))
          (atan y x)))
  (define (equ x y) (and (equ? (magnitude x) (magnitude y)) (equ? (angle x) (angle y))))
  (define (project num) (make-real (real-part num)))
  (define (negate-polar num) (make-from-mag-ang (negate (magnitude num)) (negate (angle num))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'equ '(polar polar) equ)
  (put '=zero? '(polar) (lambda (num) (and (= (real-part num) 0) (= (imag-part num) 0))))
  (put 'project '(polar) project)
  (put 'negate '(polar) negate-polar)
  'done)

(install-polar-package)

(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))

(define (install-complex-package)
  ;; imported procedures from rectangular
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag
          'rectangular)
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar)
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (add (real-part z1) (real-part z2))
     (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (sub (real-part z1) (real-part z2))
     (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (mul (magnitude z1) (magnitude z2))
     (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (div (magnitude z1) (magnitude z2))
     (sub (angle z1) (angle z2))))
  (define (equ x y) (apply-generic 'equ x y))
  (define (project num) (apply-generic 'project num))
  (define (negate-complex num) (apply-generic 'negate num))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'equ '(complex complex) equ)
  (put '=zero? '(complex) (lambda (num) (apply-generic '=zero? num)))
  (put 'project '(complex) project)
  (put 'negate '(complex) negate-complex)
  'done)
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ;; representation of terms and term lists

  (define (is-polynomial-zero? poly) (apply-generic '=zero? (term-list poly)))
  (define (add-terms L1 L2) (apply-generic 'add L1 L2))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (add-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: 
                ADD-POLY"
               (list p1 p2))))
  (define (sub-poly p1 p2) (add-poly p1 (negate-poly p2)))
  (define (mul-terms L1 L2) (apply-generic 'mul L1 L2))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly
         (variable p1)
         (mul-terms (term-list p1)
                    (term-list p2)))
        (error "Polys not in same var: 
                MUL-POLY"
               (list p1 p2))))
  (define (negate-poly num) (make-poly
                             (variable num)
                             (apply-generic 'negate (term-list num))))


  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))
  (put '=zero? '(polynomial) is-polynomial-zero?)
  (put 'negate '(polynomial) (lambda (num) (tag (negate-poly num))))
  'done)
(install-polynomial-package)

(define (install-sparse-term-list)
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (make-term order coeff)
    (list order coeff))

  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (the-empty-termlist) '())

  (define (negate-term-list term-list) (map (lambda (term) (make-term (order term) (negate (coeff term)))) term-list))
  (define (add-terms L1 L2) (cond ((empty-termlist? L1) L2)
                                  ((empty-termlist? L2) L1)
                                  (else
                                   (let ((t1 (first-term L1))
                                         (t2 (first-term L2)))
                                     (cond ((> (order t1) (order t2))
                                            (adjoin-term
                                             t1
                                             (add-terms (rest-terms L1)
                                                        L2)))
                                           ((< (order t1) (order t2))
                                            (adjoin-term
                                             t2
                                             (add-terms
                                              L1
                                              (rest-terms L2))))
                                           (else
                                            (adjoin-term
                                             (make-term
                                              (order t1)
                                              (add (coeff t1)
                                                   (coeff t2)))
                                             (add-terms
                                              (rest-terms L1)
                                              (rest-terms L2)))))))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms
            t1
            (rest-terms L))))))
  (define (are-terms-zero? term-list)
    (if (empty-termlist? term-list)
        #t
        (if (=zero? (coeff (first-term term-list)))
            (are-terms-zero? (rest-terms term-list))
            #f )))
  (define (make-sparse-term-list li) li)
  (define (multiply-sparse-term-list L1 L2) (if (empty-termlist? L1)
                                                (the-empty-termlist)
                                                (add-terms
                                                 (mul-term-by-all-terms
                                                  (first-term L1) L2)
                                                 (multiply-sparse-term-list (rest-terms L1) L2))))

  ;;interfaces
  (define (tag p) (attach-tag 'sparse p))
  (put 'make '(sparse) (lambda (term-list) (tag (make-sparse-term-list term-list))))
  (put 'mul '(sparse sparse) (lambda (L1 L2) (tag (multiply-sparse-term-list L1 L2))))
  (put 'add '(sparse sparse) (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'negate '(sparse) (lambda (term-list) (tag (negate-term-list term-list))))
  (put 'empty '(sparse) empty-termlist?)
  (put '=zero? '(sparse) are-terms-zero?)
  'done)
(install-sparse-term-list)
(define (make-sparse-term-list li) ((get 'make '(sparse))li))

(define (install-dense-term-list)
  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list)
    (null? term-list))
  (define (make-term coeff)
    coeff)

  (define (adjoin-term term-order term term-list) (cond ((= term-order (order term-list)) term-list)
                                                        ((> term-order (order term-list)) (cons term (adjoin-term (- term-order 1) 0 term-list)))))
  (define (order term-list) (if (eq? term-list '()) -1 (+ 1 (order (cdr term-list)))))
  (define (coeff term) term)
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))

  (define (are-terms-zero? term-list)
    (if (empty-termlist? term-list)
        #t
        (if (=zero? (coeff (first-term term-list)))
            (are-terms-zero? (rest-terms term-list))
            #f)))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order L1) (order L2))
                    (adjoin-term
                     (order L1)
                     t1
                     (add-terms (rest-terms L1)
                                L2)))
                   ((< (order L1) (order L2))
                    (adjoin-term
                     (order L2)
                     t2
                     (add-terms
                      L1
                      (rest-terms L2))))
                   (else
                    (adjoin-term
                     (order L1)
                     (make-term
                      (add (coeff t1)
                           (coeff t2)))
                     (add-terms
                      (rest-terms L1)
                      (rest-terms L2)))))))))
  (define (mul-term-by-all-terms L1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t1 (first-term L1)) (t2 (first-term L)))
          (adjoin-term
           (add (order L1) (order L))
           (make-term
            (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms
            L1
            (rest-terms L))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms
         (mul-term-by-all-terms
          L1 L2)
         (mul-terms (rest-terms L1) L2))
        ))

  (define (negate-term-list term-list) (map
                                        (lambda (term) (make-term (order term) (negate (coeff term))))
                                        term-list))
  ;;interfaces
  (define (tag p) (attach-tag 'dense p))
  (put 'make '(dense) (lambda (term-list) (tag term-list)))
  (put '=zero? '(dense) are-terms-zero?)
  (put 'add '(dense dense) (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'mul '(dense dense) (lambda (L1 L2) (tag (mul-terms L1 L2))))
  (put 'negate '(dense) (lambda (term-list) (tag (negate-term-list term-list))))
  'done)

(install-dense-term-list)
(define (make-dense-term-list li) ((get 'make '(dense)) li))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


(define (raise datum) (let ((get-raise (get 'raise (type-tag datum))))
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

(define (drop num)
  (let () (cond
            ((eq? (type-tag num) 'scheme-number) num)
            ((equ? (raise (project num)) num) (drop (project num)))
            (else num))))

(define (project num) (apply-generic 'project num))
(define (equ? x y) (apply-generic 'equ x y))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (negate num) (apply-generic 'negate num))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (cosine num) (apply-generic 'cosine num))
(define (sine num) (apply-generic 'sine num))
(define (=zero? num) (apply-generic '=zero? num))

(display (make-polynomial 'x (make-sparse-term-list (list 0 0 0 0))))
(newline)
(display (=zero? (make-polynomial 'x (make-sparse-term-list (list '(5 0) '(4 1) '(1 1) '(0 0))))))
(newline)
(display (negate (make-polynomial 'x (make-sparse-term-list (list '(5 0) '(4 1) (list 1 (make-rational 22 7)) '(0 0))))))
(newline)
(display (sub
          (make-polynomial 'x (make-sparse-term-list (list '(5 0) '(4 1) '(2 1) (list 0 (make-rational 10 1)))))
          (make-polynomial 'x (make-sparse-term-list (list '(5 0) '(4 1) '(1 1) '(0 1))))))
(newline)
(display (make-polynomial 'x (make-dense-term-list (list 0 0 0 0))))
(newline)
(display (mul
          (make-polynomial 'x (make-dense-term-list (list 2 2 2 1)))
          (make-polynomial 'x (make-dense-term-list (list 2 3)))))
(newline)
(display (add
          (make-polynomial 'x (make-dense-term-list (list 2 2 2 1)))
          (make-polynomial 'x (make-dense-term-list (list 2 3)))))
(newline)