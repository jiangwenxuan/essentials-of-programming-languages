#lang racket

(define var-exp
  (lambda (var)
    (cons 'var-exp var)))

(define lambda-exp
  (lambda (var lc-exp)
    (list 'lambda-exp var exp)))

(define app-exp
  (lambda (lc-exp1 lc-exp2)
    (list 'app-exp lc-exp1 lc-exp2)))

(define var-exp?
  (lambda (exp)
    (and (not (pair? exp)) (eqv? (car exp) 'var-exp))))

(define lambda-exp?
  (lambda (lc-exp)
    (and (pair? lc-exp) (eqv? (car exp) 'lambda-exp))))

(define app-exp?
  (lambda (lc-exp)
    (and (pair? lc-exp) (eqv? (car lc-exp) 'app-exp))))

(define var-exp->var
  (lambda (var-exp)
    (cdr var-exp)))

(define lambda-exp->bound-var
  (lambda (lc-exp)
    (cadr lc-exp)))

(define lambda-exp->body
  (lambda (lc-exp)
    (caddr lc-exp)))

(define app-exp->rator
  (lambda (lc-exp)
    (cadr lc-exp)))

(define app-exp->rand
  (lambda (lc-exp)
    (caddr lc-exp)))

(define occurs-free?
  (lambda (var exp)
    (cond
      ((var-exp? exp) (eqv? var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and (not (eqv? var (lambda-exp->bound-var exp)))
            (occurs-free? var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? var (app-exp->rator exp))
        (occurs-free? var (app-exp->rand exp)))))))