#lang eopl

; I change a little of Yukang's code 
; I don't know how to write robust parser exactly

(define id?
  (lambda (x)
    (and (symbol? x)
         (eqv? x 'lambda))))

(define list-of
  (lambda (pred)
    (lambda (l)
      (or (null? l)
          (and (pair? l)
               (pred (car l))
               ((list-of pred) (cdr l)))))))

(define-datatype lc-exp lc-exp?
  [var-exp (var (list-of id?))]
  [lambda-exp (bound-vars (list-of id?))
              (body lc-exp?)]
  [app-exp (rator lc-exp?)
           (rands (list-of lc-exp?))])

(define parser
  (lambda (exp)
    (cond
      ((eqv? exp 'lambda)
       (eopl:error 'parser "lambda can't be a var"))
      ((id? exp)
       (var-exp exp))
      ((pair? exp)
       (if (eqv? (car exp) 'lambda)
           (cond
             ((not (eq? (length exp) 3))
              (eopl:error 'parser "lambda expression must be three parts"))
             ((not (list? (cadr exp)))
              (eopl:error 'parser "bound-vars of lambda expression must be list"))
             (else
              (lambda-exp (cadr exp)
                          (parser (caddr exp)))))
           (app-exp
            (parser (car exp))
            (map parser (cdr exp)))))
      (else
       (eopl:error 'parser "wrong concrete syntax")))))