#lang racket

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define append
  (lambda (s1 s2)
    (if (null? s1)
        s2
        (cons (car s1) (append (cdr s1) s2)))))

(provide atom? append)