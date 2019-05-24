#lang racket

(define atom?
  (lambda (item)
    (and (not (null? item)) (not (pair? item)))))

(define append
  (lambda (s1 s2)
    (if (null? s1)
        s2
        (cons (car s1) (append (cdr s1) s2)))))

(define up
  (lambda (lst)
    (cond
      ((null? lst) lst)
      ((atom? lst) lst)
      ((atom? (car lst))
       (cons (car lst)
             (up (cdr lst))))
      (else
       (append (car lst) (up (cdr lst)))))))

(define l1 '((1 2) (3 4)))
(define l2 '((x (y)) z))

;(display (up l1))
;(newline)
;(display (up l2))