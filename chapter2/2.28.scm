#lang eopl

; see EFanZh's code, and I know how to use string-append
; because I don't know how to use format correctly

(define id?
  (lambda (symbol)
    (not (and (symbol? symbol)
              (eqv? symbol 'lambda)))))

(define-datatype lc-exp lc-exp?
  [var-exp (var id?)]
  [lambda-exp (bound-var id?)
              (body lc-exp?)]
  [app-exp (rator lc-exp?)
           (rand lc-exp?)])

(define unparse
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var)
               (symbol->string var))
      (lambda-exp (bound-var body)
                  (string-append "(lambda ("
                                 (symbol->string bound-var)
                                 ") "
                                 (unparse body)
                                 ")"))
      (app-exp (rator rand)
               (string-append "("
                              (unparse rator)
                              " "
                              (unparse rand)
                              ")")))))

(define expA (var-exp 'a))
(define expB (var-exp 'b))
(define app (app-exp expA expB))
(define lexp1 (lambda-exp 'a app))

(display (unparse app))
(newline)
(display (unparse lexp1))
(newline)
(define lexp2
  (lambda-exp
   'x
   (lambda-exp
    'y
    (lambda-exp
     'x
     (app-exp
      (lambda-exp
       'x
       (app-exp
        (var-exp 'x)
        (var-exp 'y)))
      (var-exp 'x))))))
(display (unparse lexp2))