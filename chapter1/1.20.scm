#lang racket

(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (+ (count-sexp s (car slist)) 
           (count-occurrences s (cdr slist))))))

(define count-sexp
  (lambda (s sexp)
    (if (symbol? sexp)
        (if (eqv? s sexp)
            1
            0)
        (count-occurrences s sexp))))

(define s1 '((f x) y (((x z) x))))
(define s2 '((f x) y (((x z) () x))))

(display (count-occurrences 'x s1))
(newline)
(display (count-occurrences 'x s2))
(newline)
(display (count-occurrences 'w s1))