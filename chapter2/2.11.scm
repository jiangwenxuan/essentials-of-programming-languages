#lang racket

(define empty-env?
  (lambda (env)
    (null? env)))

(define empty-env '())

(define extend-env
  (lambda (list-var list-val env)
    (cons (cons list-var list-val)
          env)))

(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env)
       (report-no-binding-found search-var))
      (else
       (let ((curr (search (caar env) (cdar env) search-var)))
         (if (eq? curr #f)
             (apply-env (cdr env) search-var)
             curr))))))

(define search
  (lambda (list-var list-val search-var)
    (cond
      ((null? list-var) #f)
      ((eqv? (car list-var) search-var)
       (car list-val))
      (else
       (search (cdr list-var) (cdr list-val) search-var)))))

(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "no binding for: ~s" search-var)))

(define has-binding?
  (lambda (env var)
    (cond
      ((null? env)
       #f)
      (else
       (let ((lvar (caar env))
             (lval (cdar env)))
         (if (eq? (search lvar lval var) #f)
             (has-binding? (cdr env) var)
             #t))))))

(define e
  (extend-env (list 'a 'b 'c)
              (list 1 2 3)
              (extend-env (list 'd 'f 'w)
                          (list 4 5 6)
                          empty-env)))

(display (apply-env e 'd))