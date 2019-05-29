#lang racket

; unary representation:

; four interface
;(define zero (lambda () '()))
;(define is-zero? (lambda (n) (null? n)))
;(define successor (lambda (n) (cons #t n)))
;(define predecessor (lambda (n) (cdr n)))

; scheme number representation:
; four interface
(define zero (lambda () 0))
(define is-zero? (lambda (n) (zero? n)))
(define successor (lambda (n) (+ n 1)))
(define predecessor (lambda (n) (- n 1)))

; bignum representation:
; see it at 2.01.scm

(define plus
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (plus (predecessor x) y)))))

; environment interface

; empty-env : () -> env
(define empty-env
  (lambda () (list 'empty-env)))

; extend-env : var * scheme-val * env -> env
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

; apply-env : env * var -> scheme-val
(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
       (report-no-binding-found search-var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (report-invalid-env env)))))

(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "no binding for: " search-var))) 

(define report-invalid-env
  (lambda (env)
    (error 'apply-env "bad environment: " env)))
















