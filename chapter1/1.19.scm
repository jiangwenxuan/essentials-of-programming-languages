#lang racket

(define list-set
  (lambda (lst n x)
    (if (zero? n)
        (cons x (cdr lst))
        (cons (car lst) (list-set (cdr lst) (- n 1) x)))))

(define l '(a b c d))
(define x1 '(1 2))
(define x2 '(1 5 10))

(display (list-set l 2 x1))
(newline)
(display (list-set l 3 x2))