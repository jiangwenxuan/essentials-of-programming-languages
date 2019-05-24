#lang racket

(define exist?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (detail-exist? pred lst))))

(define detail-exist?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (or (pred (car lst))
            (detail-exist? pred (cdr lst))))))

(define l1 '(a b c d e))
(define l2 '(1 2 3 4 5))

(display (exist? number? l1))
(newline)
(display (exist? number? l2))