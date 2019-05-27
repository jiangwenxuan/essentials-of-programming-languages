#lang racket

(define leaf
  (lambda (leaf)
    leaf))

(define leaf?
  (lambda (bst)
    (and (null? (lson bst)) (null? (rson bst)))))

(define root car)

(define lson cadr)

(define rson caddr)

(define root-of
  (lambda (bst)
    (root bst)))

(define build-tree
  (lambda (root lnode rnode)
    (list root lnode rnode)))

(define path
  (lambda (n bst)
    (cond
      ((null? bst) 'not-find-n)
      ((eq? n (root-of bst)) '())
      ((> n (root-of bst))
       (cons 'right (path n (rson bst))))
      (else
       (cons 'left (path n (lson bst)))))))

(define tree (build-tree 14
                         (build-tree 7
                                     '()
                                     (build-tree 12 '() '()))
                         (build-tree 26
                                     (build-tree 20
                                                 (build-tree 17 '() '())
                                                 '())
                                     (build-tree 31 '() '()))))

(display (path 17 tree))