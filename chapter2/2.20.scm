#lang racket

; use EFanZh's answer:
; https://github.com/EFanZh/EOPL-Exercises/blob/master/solutions/exercise-2.20.rkt

; bintree := () | (Int bintree bintree)
; bintreeX := (bintree . listof(parent))
; parent := (Int symbol bintree)

; notice: (cons `(,num () ()) '()) is an example of quasiquote
; you can search it
; or an easy introduce is here:
; https://www.dreamxu.com/scheme-of-the-quote-quasiquote-unquote-discuss/

(define number->bintree
  (lambda (num)
    (cons `(,num () ()) '())))

(define current-element caar)

(define move-to-left-son
  (lambda (bintree)
    (let* ([current (car bintree)]
           [value (car current)]
           [left-son (cadr current)]
           [right-son (caddr current)]
           [parents (cdr bintree)])
      (cons left-son
            (cons (list value 'right right-son)
                  parents)))))

(define move-to-right-son
  (lambda (bintree)
    (let* ([current (car bintree)]
           [value (car current)]
           [left-son (cadr current)]
           [right-son (caddr current)]
           [parents (cdr bintree)])
      (cons right-son
            (cons (list value 'left left-son)
                  parents)))))

(define at-leaf?
  (lambda (bintree)
    (null? (car bintree))))

(define insert-to-left
  (lambda (num bintree)
    (let* ([current (car bintree)]
           [value (car current)]
           [left-son (cadr current)]
           [right-son (caddr current)]
           [parents (cdr bintree)])
      (cons `(,value (,num ,left-son ()) ,right-son)
            parents))))

(define insert-to-right
  (lambda (num bintree)
    (let* ([current (car bintree)]
           [value (car current)]
           [left-son (cadr current)]
           [right-son (caddr current)]
           [parents (cdr bintree)])
      (cons `(value ,left-son (,num () ,right-son))
            parents))))

(define move-up
  (lambda (bintree)
    (let* ([current (car bintree)]
           [parents (cdr bintree)]
           [parent (car parents)]
           [parent-value (car parent)]
           [parent-other-branch (cadr parent)]
           [parent-other-son (caddr parent)]
           [rest-parents (cdr parents)])
      (if (eqv? parent-other-branch 'left)
          (cons (list parent-value parent-other-son current)
                rest-parents)
          (cons (list parent-value current parent-other-son)
                rest-parents)))))

(define at-root?
  (lambda (bintree)
    (null? (cdr bintree))))