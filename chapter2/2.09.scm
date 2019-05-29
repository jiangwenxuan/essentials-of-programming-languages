#lang racket

(define empty-env?
  (lambda (env)
    (null? env)))

(define empty-env '())

(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

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
      ((eqv? var (car (car env)))
       #t)
      (else
       (has-binding? (cdr env) var)))))

(define e
  (extend-env 'a 1
              (extend-env 'b 2
                          (extend-env 'c 3
                                      empty-env))))

;(display (has-binding? e 'd))
(display (has-binding? e 'c))