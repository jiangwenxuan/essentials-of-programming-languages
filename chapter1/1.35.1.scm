#lang racket

(require "./1.31.scm")

(define count 0)

(define number-leaves
  (lambda (bintree)
    (cond
      ((leaf? bintree)
       (begin
         (set! count (+ count 1))
         (leaf (- count 1))))
      (else
       (interior-node
        (contents-of bintree)
        (number-leaves (lson bintree))
        (number-leaves (rson bintree)))))))

(define tree (interior-node 'foo
                            (interior-node 'bar
                                           (leaf 26)
                                           (leaf 12))
                            (interior-node 'baz
                                           (leaf 11)
                                           (interior-node 'quux
                                                          (leaf 117)
                                                          (leaf 14)))))
(display (number-leaves tree))  