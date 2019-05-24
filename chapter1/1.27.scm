#lang racket
(require "../libs/tools.scm")

; Yukang's answer

(define flatten
  (lambda (slist)
    (cond
      ((null? slist) '())
      ((not (pair? slist)) (list slist))
      (else (append (flatten (car slist))
                    (flatten (cdr slist)))))))

(define s1 '(a b c))
(define s2 '((a) () (b ()) () (c)))
(define s3 '((a b) c (((d)) e)))
(define s4 '(a b (() (c))))

(display (flatten s1))
(newline)
(display (flatten s2))
(newline)
(display (flatten s3))
(newline)
(display (flatten s4))