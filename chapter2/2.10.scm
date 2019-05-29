#lang racket

(define empty-env?
  (lambda (env)
    (null? env)))

(define empty-env '())

(define extend-env
  (lambda (list-var list-val env)
    (if (null? list-val)
        env
        (cons (cons (car list-var)
                    (car list-val))
              (extend-env (cdr list-var)
                          (cdr list-val)
                          env)))))

(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env)
       (report-no-binding-found search-var))
      (else
       (let ((saved-var (caar env))
             (saved-val (cdar env))
             (saved-env (cdr env)))
         (if (eqv? saved-var search-var)
             saved-val
             (apply-env saved-env search-var)))))))

(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "no binding for: ~s" search-var)))

(define has-binding?
  (lambda (env var)
    (cond
      ((null? env)
       (report-no-binding-found var))
      ((eqv? var (caar env))
       #t)
      (else
       (has-binding? (cdr env) var)))))