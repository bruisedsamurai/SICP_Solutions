#lang sicp


(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ 1 test-divisor)))
        )
  )

(define (square num) (* num num))
(define (divides? divisor num) (= (remainder num divisor) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
)
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time)
      )
  )
)

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes num quant)
  (if (> quant 0)
      (cond
        ((prime? num) (timed-prime-test num)
                      (search-for-primes (+ num 1) (- quant 1)))
          (else (search-for-primes (+ num 1) quant))
          )
      )
  )

(search-for-primes 1000 3)

(search-for-primes 10000 3)

(search-for-primes 100000 3)

(search-for-primes 1000000 3)
