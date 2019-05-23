#lang racket

(define down
  (lambda (l)
    (if (null? l)
        '()
        (cons (cons (car l) '())
              (down (cdr l))))))

(define l1 '(1 2 3))
(define l2 '((a) (fine) (idea)))
(define l3 '(a (more (complicated)) object))

(display (down l1))
(newline)
(display (down l2))
(newline)
(display (down l3))