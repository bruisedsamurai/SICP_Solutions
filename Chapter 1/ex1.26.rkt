(define (is_even num) (= (remainder num 2) 0))

(define (square num) (* num num))

(def (fast_exp num power)
    (
        (cond ((= power 0) 1)
        ((= power 1) num)
        ((is_even num) (square (fast_exp num (/ power 2))))
        (else (* num (fast_exp num (- power 1))))
        )
    )
)

(define (carmichael_test num) (
    (define (fermat_test num test_no) 
        (cond (if (> num test_no)
            (if (= (remainder (fast_exp test_no num) num) test_no) 
                (fermat_test num (+ test_no 1))
                (test_no)
            )
            (test_no)
            )
        )
    )
    (fermat_test num 2)
)