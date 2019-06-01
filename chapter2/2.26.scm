#lang eopl

; I forget s-list has an example, so I see Yukang's code,
; then I remember it

; Yukang's answer returns a tree as the datatype red-blue-tree
; my answer returns a tree as the similar shape, but it changes the constructor
; it can be easily change to the datatype red-blue-tree

(define-datatype red-blue-tree red-blue-tree?
  [leaf (num integer?)]
  [red-node (left red-blue-tree?)
            (right red-blue-tree?)]
  [blue-node (rb-trees (list-of red-blue-tree?))])

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(define change-tree
  (lambda (count)
    (lambda (rb-tree)
      (cases red-blue-tree rb-tree
        [leaf (num) count]
        [red-node (left right)
                  (list 'red
                        ((change-tree (+ count 1)) left)
                        ((change-tree (+ count 1)) right))]
        [blue-node (rb-trees)
                   (cons 'blue ((deal-lst (change-tree count)) rb-trees))]))))

(define deal-lst
  (lambda (deal)
    (lambda (lst)
      (if (null? lst)
          '()
          (cons (deal (car lst))
                ((deal-lst deal) (cdr lst)))))))

(define count-depth-of-rb-tree
  (lambda (rbt)
    ((change-tree 0) rbt)))

(define rbt (red-node (red-node (leaf 0)
                                (blue-node (cons (leaf 0) '())))
                      (blue-node (cons (leaf 0)
                                       (cons (leaf 0) '())))))
(display (count-depth-of-rb-tree rbt))