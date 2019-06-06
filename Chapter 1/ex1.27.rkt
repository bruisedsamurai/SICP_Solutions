#lang sicp
(define (is_even num) (= (remainder num 2) 0))

(define (square num) (* num num))

(define (fast_exp num power)
    (
        cond ((= power 0) 1)
        ((= power 1) num)
        ((is_even power) (square (fast_exp num (/ power 2))))
        (else (* num (fast_exp num (- power 1))))
    )
)

(define (fermat_passed? pseudo_prime test_no) (= (remainder (fast_exp test_no pseudo_prime) pseudo_prime) test_no))

(define (fast_prime prm_num)
    (define (fermat_test prm_num test_no)
        (= (remainder (fast_exp test_no prm_num) prm_num) test_no)
    )
    (fermat_test prm_num (+ 1 (random (- prm_num 1))))
)

(define (carmichael_test prm_num iters) ;true says yes it's prime
    (cond ((= iters 0) true)
        ((fast_prime prm_num) (carmichael_test prm_num (- iters 1)))
        (else false)
    )
)


(carmichael_test 6601 100)
(carmichael_test 561 100)
(carmichael_test 1105 100)
(carmichael_test 1729 100)
(carmichael_test 2465 100)