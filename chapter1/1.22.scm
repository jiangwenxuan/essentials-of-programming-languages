#lang racket

(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))

(define l1 '(a 2 (1 3) b 7))
(define l2 '(a (b c) 17 foo))

(display (filter-in number? l1))
(newline)
(display (filter-in symbol? l2))