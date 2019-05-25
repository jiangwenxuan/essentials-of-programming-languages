#lang racket

(require "./1.31.scm")

(define double-tree
  (lambda (bintree)
    (if (leaf? bintree)
        (* 2 bintree)
        (interior-node (contents-of bintree)
                       (double-tree (lson bintree))
                       (double-tree (rson bintree))))))

(define tree (interior-node 'a (interior-node 'b 3 4) 5))

(display (double-tree tree))

(provide double-tree)