#lang eopl

(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
      [var-exp (var) (eqv? var search-var)]
      [lambda-exp (bound-var body)
                  (and (not (eqv? search-var bound-var))
                       (occurs-free? search-var body))]
      [app-exp (rator rand)
               (or (occurs-free? search-var rator)
                   (occurs-free? search-var rand))])))

(define-datatype lc-exp lc-exp?
  [var-exp (var identifier?)]
  [lambda-exp (bound-var identifier?)
              (body lc-exp?)]
  [app-exp (rator lc-exp?)
           (rand lc-exp?)])

(define identifier?
  (lambda (x)
    (not (and (eqv? x 'lambda)
              (symbol? x)))))