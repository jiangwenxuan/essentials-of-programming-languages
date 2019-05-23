#lang racket

(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (if (symbol? (car slist))
            (if (eqv? old (car slist))
                (cons new (subst new old (cdr slist)))
                (cons (car slist) (subst new old (cdr slist))))
            (cons (subst new old (car slist))
                  (subst new old (cdr slist)))))))

(define s '((b c) (b () d)))
(display (subst 'a 'b s))