#lang racket

(define number-elements-from
  (lambda (lst n)
    (if (null? lst)
        '()
        (cons (list n (car lst))
              (number-elements-from (cdr lst) (+ n 1))))))

(define number-elements
  (lambda (lst)
    (if (null? lst)
        '()
        (g (list 0 (car lst))
           (number-elements (cdr lst))))))

(define g
  (lambda (li end)
    (if (null? end)
        (list li)
        (cons li
              (add (car end) (cdr end))))))

(define add
  (lambda (n l)
    (if (null? l)
        (list (list (+ (car n) 1) (cadr n)))
        (cons (list (+ (car n) 1) (cadr n))
              (add (car l) (cdr l))))))

(define x '(a b c d))

(display (number-elements x))