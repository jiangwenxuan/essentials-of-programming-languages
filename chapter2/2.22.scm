#lang eopl

; use some Yukang's answer, and I read some blog,
; know a little about datatype and cases

(define value?
  (lambda (x)
    #t))

(define-datatype stack stack?
  [empty-stack-inter]
  [push-stack-inter (_x value?)
                    (_stack stack?)]
  [pop-stack-inter (_stack stack?)]
  [top-stack-inter (_stack stack?)])

(define empty-stack
  (lambda () (empty-stack-inter)))

(define pop
  (lambda (s)
    (cases stack s
      [empty-stack-inter () (eopl:error "empty stack")]
      [push-stack-inter (e s) s]
      [pop-stack-inter (s) s]
      [top-stack-inter (s) s])))

(define push
  (lambda (x s)
    (push-stack-inter x s)))

(define top
  (lambda (st)
    (cases stack st
      (empty-stack-inter () (eopl:error 'top "empty-stack"))
      (push-stack-inter (x s) x)
      (pop-stack-inter (s) (top s))
      (top-stack-inter (s) (top s)))))

(define empty-stack?
  (lambda (st)
    (cases stack st
      (empty-stack-inter () #t)
      (push-stack-inter (x s) #f)
      (pop-stack-inter (s) (empty-stack? s))
      (top-stack-inter (s) (empty-stack? s)))))

(define e (empty-stack))
(define a (push 1 e))
(define b (push 2 a))
(define c (push 3 b))

(display (top c))
(newline)
(pop b)
(newline)
(display (top (pop b)))