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

;; data structure representation
;; empty-env : () -> env
;(define empty-env
;  (lambda () (list 'empty-env)))
;
;; extend-env : var * scheme-val * env -> env
;(define extend-env
;  (lambda (var val env)
;    (list 'extend-env var val env)))
;
;; apply-env : env * var -> scheme-val
;(define apply-env
;  (lambda (env search-var)
;    (cond
;      ((eqv? (car env) 'empty-env)
;       (report-no-binding-found search-var))
;      ((eqv? (car env) 'extend-env)
;       (let ((saved-var (cadr env))
;             (saved-val (caddr env))
;             (saved-env (cadddr env)))
;         (if (eqv? search-var saved-var)
;             saved-val
;             (apply-env saved-env search-var))))
;      (else
;       (report-invalid-env env)))))
;
(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "no binding for: ~s" search-var))) 
;
;(define report-invalid-env
;  (lambda (env)
;    (error 'apply-env "bad environment: " env)))


; precedural representation
; env = var -> scheme-val

; empty-env : () -> env
(define empty-env
  (lambda ()
    (lambda (search-var)
      (report-no-binding-found search-var))))

; extend-env : var * scheme-val * env -> env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

; apply-env : env * var -> scheme-val
(define apply-env
  (lambda (env search-var)
    (env search-var)))

(define env1
  (extend-env 'a 1
              (extend-env 'b '2
                          (extend-env 'c 3
                                      (empty-env)))))
;(display (apply-env env1 'c))
;(newline)
;(display (apply-env env1 'd))


; interfaces for recursive data types
(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp)
       (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and (not (eqv? search-var (lambda-exp->bound-var exp)))
            (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or (occurs-free? search-var (app-exp->rator exp))
           (occurs-free? search-var (app-exp->rand exp)))))))

; occurs-free? : sym * lc-exp -> bool
(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
      (var-exp (var) (eqv? var search-var))
      (lambda-exp (bound-var body)
                  (and
                   (not (eqv? search-var bound))
                   (occurs-free? search-var body)))
      (app-exp (rator rand)
               (or
                (occurs-free? search-var rator)
                (occurs-free? search-var rand))))))

;(define-datatype lc-exp lc-exp?
;  (var-exp
;   (var identifier?))
;  (lambda-exp
;   (bound-var identifier?)
;   (body lc-exp?))
;  (app-exp
;   (rator lc-exp?)
;   (rand lc-exp?)))

;(define-datatype type-name type-predicate-name
;  {(variant-name {(field-name predicate)}*)}+)

(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
   (first s-exp?)
   (rest s-list?)))
(define-datatype s-exp s-exp?
  (symbol-s-exp
   (sym symbol?))
  (s-list-s-exp
   (slst s-list?)))

(define-datatype s-list s-list?
  (an-s-list
   (sexps (list-of s-exps?))))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))





































