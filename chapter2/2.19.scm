#lang racket

(define number->bintree
  (lambda (x)
    (list x '() '())))

(define current-element
  (lambda (bintree)
    (car bintree)))

(define move-to-left-son
  (lambda (bintree)
    (cadr bintree)))

(define move-to-right-son
  (lambda (bintree)
    (caddr bintree)))

(define insert-to-left
  (lambda (x bintree)
    (list (current-element bintree)
          (list x (move-to-left-son bintree) '())
          (move-to-right-son bintree))))

(define insert-to-right
  (lambda (x bintree)
    (list (current-element bintree)
          (move-to-left-son bintree)
          (list x '() (move-to-right-son bintree)))))

(define at-leaf?
  (lambda (bintree)
    (null? bintree)))

(display (number->bintree 13))
(newline)

(define t1 (insert-to-right 14
                            (insert-to-left 12
                                            (number->bintree 13))))

(display t1)
(newline)

(display (move-to-left-son t1))
(newline)

(display (current-element (move-to-left-son t1)))
(newline)

(display (at-leaf? (move-to-right-son (move-to-left-son t1))))
(newline)

(display (insert-to-left 15 t1))