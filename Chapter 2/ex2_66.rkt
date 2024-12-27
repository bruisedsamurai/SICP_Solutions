#lang sicp


(define (lookup given-key set-of-records)
  (let ((curr-key (key (car (set-of-records)))))
    (cond ((or (null? set-of-records) (> given-key curr-key)) false)
          ((equal? given-key curr-key) (car set-of-records))
          (else (lookup given-key (cdr set-of-records)))
          )))