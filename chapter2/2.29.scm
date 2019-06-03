#lang eopl

(define id?
  (lambda (x)
    (and (symbol? x)
         (not (eqv? x 'lambda)))))

(define-datatype lc-exp lc-exp?
  [var-exp (var id?)]
  [lambda-exp (bound-vars (list-of id?))
              (body lc-exp?)]
  [app-exp (rator lc-exp?)
           (rands (list-of lc-exp?))])

(define list-of
  (lambda (pred)
    (lambda (l)
      (or (null? l)
          (and (pair? l)
               (pred (car l))
               ((list-of pred) (cdr l)))))))

(define parser-lc-exp
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp
            (cadr datum)
            (parser-lc-exp (caddr datum)))
           (app-exp
            (parser-lc-exp (car datum))
            (map parser-lc-exp (cdr datum)))))
      (else (eopl:error "wrong concrete syntax datum")))))

(display (parser-lc-exp 'a))
(newline)
(display (parser-lc-exp '(lambda (a) (+ a b))))
(newline)
(display (parser-lc-exp (quote (lambda (a) (+ a b)))))