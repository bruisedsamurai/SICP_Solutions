#lang sicp

;;;; Using recursive process
(define (f n) (
               if (< n 3) n (+
                             (f (- n 1))
                             (* 2 (f (- n 2)))
                             (* 3 (f (- n 3)))
                             )
               )
  )

(f 6)

;;;; using iterative method
(define (f_iter sum counter n frst scnd) (if (= n counter) sum
                                                  (f_iter (+ sum (* 2 frst) (* 3 scnd))
                                                          (+ counter 1) n sum frst)
                           )
  )
(define (f_i n) (f_iter 2 2 n 1 0))
(f_i 6)
