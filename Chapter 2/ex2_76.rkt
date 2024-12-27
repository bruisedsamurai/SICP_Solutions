#lang sicp

; As system evolves with generic operations
; 1. For new types the original procedure needs to modified to incorporate the type checking and then
; dispatching to the appropriate procedure. Whereas for new operations, new procedures have to be created
; for each type and then finally incorporated in the a procedure that will do the explicit dispatch.
; 2. In data directed style, only new procedures for new types have to be created and install
; without needing to modify the exposed procedure. In case of new operations, a new abstraction has to
; be created with implementation of each type to be installed in the the table.
; 3. Finally, in case of message passing; Adding a new type means creating implementation of all of it's
; procedures. Whereas, adding a new procedure means modifiying the implementation of the procedure of
; all types to incorporate it.

; Adding new types would be easier in message passing because it doesn't touch other code. Whereas
; adding new operations frequently be suited for data directed.
; Honestly, I don't see a black and white here. In my opinion, adding types is easier in both whereas
; adding new operations is slightly difficult.