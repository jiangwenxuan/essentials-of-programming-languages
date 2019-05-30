#lang racket

(define empty-env
  (lambda ()
    (lambda (cmd)
      (cond
        ((eqv? cmd 'apply-env)
         (lambda (search-var)
           (error 'apply "no binding for: ~s" search-var)))
        ((eqv? cmd 'empty-env?)
         #t)
        ((eqv? cmd 'has-binding?)
         (lambda (search-var)
           #f))
        (else
         (error "wrong cmd"))))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (cmd)
      (cond
        ((eqv? cmd 'apply-env)
         (lambda (search-var)
           (if (eqv? search-var saved-var)
               saved-val
               (apply-env saved-env search-var))))
        ((eqv? cmd 'empty-env?)
         #f)
        ((eqv? cmd 'has-binding?)
         (lambda (search-var)
           (if (eqv? search-var saved-var)
               #t
               (has-binding? saved-env search-var))))
        (else
         (error "wrong cmd"))))))

(define apply-env
  (lambda (env search-var)
    ((env 'apply-env) search-var)))

(define has-binding?
  (lambda (env search-var)
    ((env 'has-binding?) search-var)))

(define empty-env?
  (lambda (env)
    (env 'empty-env?)))

(define e
  (extend-env 'a 1
              (extend-env 'b 2
                          (extend-env 'c 3
                                      (extend-env 'd 4
                                                  (empty-env))))))

(display (apply-env e 'c))
(newline)
(display (has-binding? e 'e))
(newline)
(display (empty-env? e))              