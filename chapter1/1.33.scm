#lang racket

(require "./1.31.scm")

(define make-leaves-with-red-depth
  (lambda (tree)
    (count-red tree 0)))

(define count-red
  (lambda (tree num)
    (cond
      ((leaf? tree) (leaf num))
      ((eq? (contents-of tree) 'red)
       (interior-node (contents-of tree)
                      (count-red (lson tree) (+ num 1))
                      (count-red (rson tree) (+ num 1))))
      (else
       (interior-node (contents-of tree)
                      (count-red (lson tree) num)
                      (count-red (rson tree) num))))))

(define red-tree
  (interior-node 'red
                 (interior-node 'bar
                                (leaf 26)
                                (leaf 12))
                 (interior-node 'red
                                (leaf 11)
                                (interior-node 'quux
                                               (leaf 117)
                                               (leaf 14)))))

(display (make-leaves-with-red-depth red-tree))