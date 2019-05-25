#lang racket

(require "../libs/tools.scm")

(define interior-node
  (lambda (content lnode rnode)
    (list content lnode rnode)))

(define leaf
  (lambda (content)
    content))

(define leaf?
  (lambda (bintree)
    (not (pair? bintree))))

(define lson cadr)

(define rson caddr)

(define contents-of
  (lambda (bintree)
    (if (leaf? bintree)
        bintree
        (car bintree))))

(provide interior-node leaf leaf? lson rson contents-of)