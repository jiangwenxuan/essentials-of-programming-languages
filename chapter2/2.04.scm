#lang racket

; observers
(define empty-stack?
  (lambda (s)
    (and (stack? s)
         (null? (cdr s)))))

(define stack?
  (lambda (s)
    (cond
      ((null? s) #f)
      ((not (pair? s)) #f)
      (else (equal? 'stack (car s))))))

; constructors
(define empty-stack (list 'stack))

(define push
  (lambda (s x)
    (if (stack? s)
        (set-cdr! s (cons x (cdr s)))
        #f)))

(define pop
  (lambda (s)
    (if (empty-stack? s)
        #f
        (let ((curr (cadr s)))
          (begin (set-cdr! s (cddr s))
                 curr)))))
        
(define top
  (lambda (s)
    (if (stack? s)
        (cadr s)
        #f)))