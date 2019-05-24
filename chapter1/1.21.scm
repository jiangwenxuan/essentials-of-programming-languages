#lang racket

; I forget append, so use cons, but it's wrong!
; Yukang's answer

(define product
  (lambda (sos1 sos2)
    (cond ((null? sos1) '())
          (else
           (append (product-symbol (car sos1) sos2)
                   (product (cdr sos1) sos2))))))

(define product-symol
  (lambda (sym sos)
    (cond ((null? sos) '())
          (else
           (cons (list sym (car sos))
                 (product-symbol sym (cdr lst)))))))

(define append
  (lambda (list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2)))))