#lang racket

(define in-S?
  (lambda (n)
    (if (zero? n) #t
     (if (>= (- n 3) 0)
         (in-S? (- n 3))
         #f))))