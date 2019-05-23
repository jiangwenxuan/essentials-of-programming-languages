#lang racket

(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (map (lambda (item) (subst-in-s-exp new old item))
             slist))))

(define subst-in-s-exp
  (lambda (new old item)
    (if (null? item)
        '()
        (if (symbol? item)
            (if (eqv? item old)
                new
                item)
            (subst new old item)))))

(define s '((b c) (b () d)))
(display (subst 'a 'b s))