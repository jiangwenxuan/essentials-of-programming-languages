#lang racket

; in 1.29.scm, I use other's answer
; Yukang's ans is good, so I only need
; to change a little in 1.30.scm

(define insert
  (lambda (pred lst elem)
    (cond
      ((null? lst) (list elem))
      ((pred elem (car lst))
       (cons elem lst))
      (else
       (cons (car lst)
             (insert pred (cdr lst) elem))))))

(define sort-rec
  (lambda (pred prev new)
    (cond
      ((null? new) prev)
      (else
       (sort-rec pred
                 (insert pred prev (car new))
                 (cdr new))))))

(define sort
  (lambda (pred lst)
    (sort-rec pred '() lst)))

(define l1 '(8 2 5 2 3))
(define l2 '(9 1 0 8 4))

(display (sort > l1))
(newline)
(display (sort < l2))