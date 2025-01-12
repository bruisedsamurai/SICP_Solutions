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
  (put 'sine '(scheme-number) (lambda (num) (sin num)))
  (put 'cosine '(scheme-number) (lambda (num) (cos num)))
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
  'done)
(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (define (make-real num) (* num 1.0))
  (define (equ x y) (= x y))
  (define (project num) (make-rational num 1))
  (put 'make 'real (lambda (num) (tag (make-real num))))
  (put 'equ '(real real) equ)
  (put 'project '(real) project)
  )
(install-real-package)

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
  (put 'project '(rectangular) project)
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
  (put 'project '(polar) project)
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
  (put 'project '(complex) project)
  'done)
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


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

(define (drop num)
  (let () (cond
            ((eq? (type-tag num) 'rational) num)
            ((eq? (type-tag num) 'scheme-number) num)
            ((equ? (raise (project num)) num) (drop (project num)))
            (else num))))

(define (project num) (apply-generic 'project num))
(define (equ? x y) (apply-generic 'equ x y))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (cosine num) (apply-generic 'cosine num))
(define (sine num) (apply-generic 'sine num))

;; In this new system, we replaced primitive operators will their equivalent generic procedures that
;; further operates on types like rational, and scheme numbers. Thus allowing us to use these numbers
;; as particulars of the complex numbers.

(display (add
          (make-complex-from-real-imag (make-rational 5 5) (make-rational 5 10))
          (make-complex-from-mag-ang (make-rational 5 5) 0)))
(display (add (make-complex-from-real-imag 10 1) (make-complex-from-mag-ang 10 0)))
(newline)