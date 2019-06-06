#lang sicp

(define (is_root numer deno)
  (cond ((= numer (- deno 1)) false)
      ((= (remainder (* numer numer) deno) 1) true)
      (else false)
  )
)

(define (mod_square base num)  (* base base))
(define (is_even base) (= (remainder base 2) 0))

(define (mod_fast_exp base pow num)
  (cond ((= pow 0) 1)
        ((is_even pow) (mod_square (mod_fast_exp base (/ pow 2) num) num)) ;Mod square checks for mod congruency too
        (else (* base (mod_fast_exp base (- pow 1) num)))
        )
  )


(define (fast_prime prm_num)
      (define (miller_passed? base n)
            (= (remainder (mod_fast_exp base (- n 1) n) n) 1)
      )
      (miller_passed? (+ (random (- prm_num 1)) 1) prm_num)
)

(define (miller_rabin num iters)
      (cond ((= iters 0) true)
            ((fast_prime num) (miller_rabin num (- iters 1)))
            (else false)
      )
)

(miller_rabin 6601 1000)
(miller_rabin 2465 1000)
(miller_rabin 2821 1000)
(miller_rabin 561 1000)
(miller_rabin 7 1000)
(miller_rabin 5 1000)


