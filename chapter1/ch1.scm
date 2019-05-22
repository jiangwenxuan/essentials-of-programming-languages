#lang racket

(define in-S?
  (lambda (n)
    (if (zero? n) #t
     (if (>= (- n 3) 0)
         (in-S? (- n 3))
         #f))))

(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))

(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))
(define report-list-too-short
  (lambda (n)
    (eopl:error 'nth-element
                "list too short by ~s elements.~%" (+ n 1))))

