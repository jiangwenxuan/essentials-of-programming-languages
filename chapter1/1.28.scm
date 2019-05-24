#lang racket

(define merge
  (lambda (loi1 loi2)
    (cond
      ((null? loi1) loi2)
      ((null? loi2) loi1)
      ((> (car loi1) (car loi2))
       (cons (car loi2)
             (merge loi1 (cdr loi2))))
      (else
       (cons (car loi1)
             (merge (cdr loi1) loi2))))))

(define loi1 '(1 4))
(define loi2 '(1 2 8))
(define loi3 '(35 62 81 90 91))
(define loi4 '(3 83 85 90))

(display (merge loi1 loi2))
(newline)
(display (merge loi3 loi4))