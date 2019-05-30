#lang racket

; some functions use Yukang's answer

; var-exp : var -> lc-exp
(define var-exp
  (lambda (var)
    (cons 'var-exp var)))

; lambda-exp : var * lc-exp -> lc-exp
(define lambda-exp
  (lambda (var lc-exp)
    (list 'lambda-exp (list var) lc-exp)))

; app-exp : lc-exp * lc-exp -> app-exp
(define app-exp
  (lambda (lc-exp1 lc-exp2)
    (list 'app-exp lc-exp1 lc-exp2)))

; var-exp? : lc-exp -> bool
(define var-exp?
  (lambda (exp)
    (and (pair? exp) (eqv? (car exp) 'var-exp))))

; lambda-exp? : lc-exp -> bool
(define lambda-exp?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'lambda-exp))))

; app-exp? : lc-exp -> bool
(define app-exp?
  (lambda (x)
    (and (pair? x) (eqv? (car x) 'app-exp))))

; var-exp->var : lc-exp -> var
(define var-exp->var
  (lambda (x)
    (cdr x)))

; lambda-exp->bound-var : lc-exp -> var
(define lambda-exp->bound-var
  (lambda (x)
    (caadr x)))

; lambda-exp->body : lc-exp -> lc-exp
(define lambda-exp->body
  (lambda (x)
    (caddr x)))

; app-exp->rator : lc-exp -> lc-exp
(define app-exp->rator
  (lambda (x)
    (cadr x)))

; app-exp->rand : lc-exp -> lc-exp
(define app-exp->rand
  (lambda (x)
    (caddr x)))

; occurs-free? : sym * lc-exp -> bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and (not (eqv? search-var (lambda-exp->bound-var exp)))
            (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp)))))))

(define exp-test (lambda-exp 'a
                             (app-exp (var-exp 'b) (var-exp 'a))))

(display (occurs-free? 'a exp-test))
(newline)
(display (occurs-free? 'b exp-test))