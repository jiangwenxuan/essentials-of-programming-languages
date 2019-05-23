#lang racket

(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (cons (swapper-in-s-exp s1 s2 (car slist))
              (swapper s1 s2 (cdr slist))))))

(define swapper-in-s-exp
  (lambda (s1 s2 sexp)
    (if (symbol? sexp)
        (if (eqv? sexp s1)
            s2
            (if (eqv? sexp s2)
                s1
                sexp))
        (swapper s1 s2 sexp))))

(define l1 '(a b c d))
(define l2 '(a d () c d))
(define l3 '((x) y (z (x))))
(display (swapper 'a 'd l1))
(newline)
(display (swapper 'a 'd l2))
(newline)
(display (swapper 'x 'y l3))