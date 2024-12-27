#lang sicp

(define (make-from-mag-ang mag angle)
  (define (dispatch op)
    (cond
      ((eq? op 'real-part) (* mag (cos angle)))
      ((eq? op 'imag-part) (* mag (sin angle)))
      ((eq? op 'magnitude) mag)
      ((eq? op 'angle) angle)
      (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))
      )
    )
  dispatch)

(display ((make-from-mag-ang 10 30) 'real-part))
(newline)
(display ((make-from-mag-ang 10 30) 'wub-luba))