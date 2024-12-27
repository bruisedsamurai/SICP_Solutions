#lang sicp

; Any complex number is tagged with 'complex thus when the time comes for dispatching magintude procedure
; deletgates to apply generic. Which in turn looks for procedure tagged with `magnitude `(complex) in
; the lookup table. Since, we just assigned a procedure hence it successfully finds the procedure.

; The calls go like this, #magnitude -> #apply-generic -> #magnitude -> #apply-generic -> corresponding
; -implementation
; In total, apply generic is called two times.
; At first 'magnitude procedure from complex-number package is dispatched and second time based on the
; type tag the rectangular or polar procedure for calculating magnitude is dispatched.