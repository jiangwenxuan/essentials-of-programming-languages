#lang racket

(define list-index
  (lambda (pred lst)
    (list-index-count pred lst 0)))

(define list-index-count
  (lambda (pred lst index)
    (cond
      ((null? lst) #f)
      ((pred (car lst)) index)
      (else
       (list-index-count pred (cdr lst) (+ index 1))))))

(define l1 '(a 2 (1 3) b 7))
(define l2 '(a (b c) 17 foo))
(define l3 '(1 2 (a b) 3))

(display (list-index number? l1))
(newline)
(display (list-index symbol? l2))
(newline)
(display (list-index symbol? l3))