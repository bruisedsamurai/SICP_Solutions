#lang sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define a-leaf-set (make-leaf-set (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1))))
(display a-leaf-set)
(newline)

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (make-code-tree (car leaf-set) (successive-merge (cdr leaf-set)))))

(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (encode-symbol symbol tree)
  (if (leaf? tree) '() (let ((left-subtree (left-branch tree)) (right-subtree (right-branch tree)))
                         (cond ((memq symbol (symbols left-subtree)) (cons 0 (encode-symbol symbol left-subtree)))
                               ((memq symbol (symbols right-subtree)) (cons 1 (encode-symbol symbol right-subtree)))
                               )
                         ))
  )

(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))

(define huffman-tree (generate-huffman-tree (list
                                             (list 'A 2) (list 'NA 16)
                                             (list 'BOOM 1) (list 'SHA 3)
                                             (list 'GET 2) (list 'YIP 9)
                                             (list 'JOB 2) (list 'WAH 1))))

(display (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM) huffman-tree))
(newline)
(length (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM) huffman-tree))
(newline)