#lang racket

; env = var -> scheme-val

; empty-env : () -> env
(define empty-env
  (lambda ()
    (cons (lambda (search-var)
            (error 'apply "no binding var for: ~s" search-var))
          (lambda () #t))))

; extend-env : var * scheme-val * env -> env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (cons (lambda (search-var)
            (if (eqv? search-var saved-var)
                saved-val
                (apply-env saved-env search-var)))
          (lambda () #f))))

; apply-env : env * var -> scheme-val
(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

(define empty-env?
  (lambda (env)
    ((cdr env))))

(define e
  (extend-env 'd 6
              (extend-env 'y 8
                          (extend-env 'a 1
                                      (extend-env 'b 2
                                                  (empty-env))))))