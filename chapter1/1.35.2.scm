#lang racket

(require "./1.31.scm")

; use Yukang's answer

(define number-leaves
  (lambda (btree)
    (let ((n -1))
      (define replace-leaf-with
        (lambda (btree)
          (cond
            ((leaf? btree)
             (begin
               (set! n (+ n 1))
               n))
            (else
             (interior-node (contents-of btree)
                            (replace-leaf-with (lson btree))
                            (replace-leaf-with (rson btree)))))))
      (replace-leaf-with btree))))

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