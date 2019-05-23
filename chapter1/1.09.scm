#lang racket

(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (remove s (cdr los))
            (cons (car los)
                  (remove s (cdr los)))))))

(define l '(a a b b c d e f a g h))
(display (remove 'a l))