#lang racket

; use Yukang's answer
; and I will learn how to write merge sort, fast sort
; in scheme

(define insert
  (lambda (lst elem)
    (cond
      ((null? lst) (list elem))
      ((< elem (car lst))
       (cons elem lst))
      (else (cons (car lst)
                  (insert (cdr lst) elem))))))

(define sort-rec
  (lambda (prev now)
    (if (null? now)
        prev
        (sort-rec (insert prev (car now))
                  (cdr now)))))

(define sort
  (lambda (lst)
    (sort-rec '() lst)))