#lang eopl

(define-datatype bintree bintree?
  [leaf-node (num integer?)]
  [interior-node (key symbol?)
                 (left bintree?)
                 (right bintree?)])

(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
      [leaf-node (x) (list 'leaf-node x)]
      [interior-node (key left right)
                     (list 'interior-node
                           key
                           (bintree-to-list left)
                           (bintree-to-list right))])))

(display (bintree-to-list (interior-node 'a
                                         (leaf-node 3)
                                         (leaf-node 4))))