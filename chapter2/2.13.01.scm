#lang racket

; this is my answer, 2.13.02.scm is Yukang's answer
(define empty-env
  (lambda ()
    (lambda (cmd)
      (cond
        ((eqv? cmd 'search)
         (lambda (search-var)
           (report-no-binding-found search-var)))
        ((eqv? cmd 'empty?)
         #t)
        (else
         (error "wrong cmd"))))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (cmd)
      (cond
        ((eqv? cmd 'search)
         (lambda (search-var)
           (if (eqv? search-var saved-var)
               saved-val
               (search saved-env search-var))))
        ((eqv? cmd 'empty)
         #f)
        (else
         (error "wrong cmd"))))))

(define search
  (lambda (env search-var)
    ((env 'search) search-var)))

(define empty-env?
  (lambda (env)
    (env 'empty)))

(define env
  (extend-env 'a 1
              (extend-env 'b 2
                          (extend-env 'c 3
                                      (empty-env)))))

(define report-no-binding-found
  (lambda (search-var)
    (error 'search "no binding for: ~s" search-var)))

(display (search env 'b))
(newline)
(display (empty-env? env))
(newline)
(display (search env 'd))