#lang racket

(define duple
  (lambda (times x)
    (if (zero? times)
        '()
        (cons x (duple (- times 1) x)))))

(define l1 '3)
(define l2 '(ha ha))
(define l3 '(blah))