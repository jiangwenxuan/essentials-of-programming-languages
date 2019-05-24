#lang racket

(define detail-every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (and (pred (car lst))
             (detail-every? pred (cdr lst))))))

(define every?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (detail-every? pred lst))))

(define l1 '(a b c 3 e))
(define l2 '(1 2 3 4 5))

(display (every? number? l1))
(newline)
(display (every? number? l2))