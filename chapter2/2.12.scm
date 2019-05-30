#lang racket

(define empty-stack
  (lambda ()
    (lambda (cmd)
      (cond
        ((eqv? cmd 'top)
         (error "try top on empty stack"))
        ((eqv? cmd 'pop)
         (error "try pop on empty stack"))
        (else
         (error "unknown cmd on stack"))))))

(define push
  (lambda (saved-stack var)
    (lambda (cmd)
      (cond
        ((eqv? cmd 'top) var)
        ((eqv? cmd 'pop) saved-stack)
        (else
         (error "error cmd"))))))

(define pop
  (lambda (stack)
    (stack 'pop)))

(define top
  (lambda (stack)
    (stack 'top)))

(define e (empty-stack))
(define x1 (push e 1))
(define x2 (push x1 2))
(define x3 (push x2 3))
