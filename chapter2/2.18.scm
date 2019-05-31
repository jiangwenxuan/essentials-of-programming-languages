#lang racket

; maybe my answer is wrong, (-^ O ^-)

(define number->sequence
  (lambda (num)
    (list num '() '())))

(define current-element
  (lambda (seq)
    (car seq)))

(define left-seq
  (lambda (seq)
    (cadr seq)))

(define right-seq
  (lambda (seq)
    (caddr seq)))

(define move-to-left
  (lambda (seq)
    (if (null? (right-seq seq))
        (error 'move-to-left "left sequence is null")
        (list (car (left-seq seq))
              (cdr (left-seq seq))
              (cons (current-element (right-seq seq)))))))

(define move-to-right
  (lambda (seq)
    (if (null? (left-seq seq))
        (error 'move-to-right "right sequence is null")
        (list (car (right-seq seq))
              (cons (current-element seq)
                    (left-seq seq))
              (cdr (right-seq seq))))))

(define insert-to-left
  (lambda (x seq)
    (list (current-element seq)
          (cons x (left-seq seq))
          (right-seq seq))))

(define insert-to-right
  (lambda (x seq)
    (list (current-element seq)
          (left-seq seq)
          (cons x (right-seq seq)))))

