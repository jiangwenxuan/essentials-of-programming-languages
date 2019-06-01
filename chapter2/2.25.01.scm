#lang eopl

; I think Yukang's answer is a little complicated
; so I change it 

(define-datatype bintree bintree?
  [leaf-node (num integer?)]
  [interior-node (key symbol?)
                 (left bintree?)
                 (right bintree?)])

(define sum-tree
  (lambda (bt)
    (cases bintree bt
      [interior-node (key left right)
                     (let* ([sum-left (sum-tree left)]
                            [sum-right (sum-tree right)]
                            [cur-sum (+ (cdar sum-left)
                                        (cdar sum-right))])
                       (cond
                         ((and (null? (caar sum-left))
                               (null? (caar sum-right)))
                          (list (cons key cur-sum) '() '()))
                         ((null? (caar sum-left))
                          (list (cons key cur-sum) '() sum-right))
                         ((null? (caar sum-right))
                          (list (cons key cur-sum) sum-left '()))
                         (else
                          (list (cons key cur-sum) sum-left sum-right))))]
      [leaf-node (num) (list (cons '() num))])))

(define key-of-leaf car)
(define num-of-leaf cdr)
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)

(define max-tree
  (lambda (sum-bt)
    (let ([root (root-tree sum-bt)]
          [left-son (left-tree sum-bt)]
          [right-son (right-tree sum-bt)])
      (cond
        ((and (null? left-son) (null? right-son)) root)
        ((null? left-son)
         (let ([curr (max-tree right-son)])
           (if (> (num-of-leaf root) (num-of-leaf curr))
               root
               curr)))
        ((null? right-son)
         (let ([curr (max-tree left-son)])
           (if (> (num-of-leaf root) (num-of-leaf curr))
               root
               curr)))
        (else
         (let ([curr-l (max-tree left-son)]
               (curr-r (max-tree right-son)))
           (if (> (num-of-leaf curr-l) (num-of-leaf curr-r))
               (if (> (num-of-leaf root) (num-of-leaf curr-l))
                   root
                   curr-l)
               (if (> (num-of-leaf root) (num-of-leaf curr-r))
                   root
                   curr-r))))))))

(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))

(display (car (max-tree (sum-tree tree-2))))
(newline)
(display (car (max-tree (sum-tree tree-3))))