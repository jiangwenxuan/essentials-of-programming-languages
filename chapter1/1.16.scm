#lang racket

(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (invert-item (car lst))
              (invert (cdr lst))))))

(define invert-item
  (lambda (item)
    (cons (cadr item)
          (cons (car item)
                '()))))

(define l '((a 1) (a 2) (1 b) (2 b)))