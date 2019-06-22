#lang eopl

; data structure representation
; just delete environment in procedure(vars body)
; and add multi vars and vals in let expression

(define empty-env '())

(define empty-env? null?)

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (cons (cons saved-var saved-val) saved-env)))

(define extend-env-list
  (lambda (saved-vars saved-vals saved-env)
    (if (null? saved-vars)
        saved-env
        (cons (cons (car saved-vars)
                    (car saved-vals))
              (extend-env-list (cdr saved-vars)
                               (cdr saved-vals)
                               saved-env)))))

(define extend-env-let
  (lambda (vars exps env)
    (if (null? vars)
        env
        (cons (cons (car vars)
                    (value-of (car exps) env))
              (extend-env-let (cdr vars) (cdr exps) env)))))

(define apply-env
  (lambda (env search-var)
    (if (empty-env? env)
        (eopl:error 'apply-env "no binding for: ~s" search-var)
        (if (eqv? search-var (caar env))
            (cdar env)
            (apply-env (cdr env) search-var)))))

(define init-env
  (lambda ()
    (extend-env 'a (num-val 3) empty-env)))

(define environment? pair?)


(define list-of
  (lambda (pred)
    (lambda (x)
      (or (null? x)
          (and (pair? x)
               (pred (car x))
               ((list-of pred) (cdr x)))))))


(define lex-a
  '([whitespace (whitespace) skip]
    [commit ("%" (arbno (not #\newline))) skip]
    [identifier (letter (arbno (or letter digit))) symbol]
    [number (digit (arbno digit)) number]
    [number ("-" digit (arbno digit)) number]))

(define grammar-let
  '([program (expression) a-program]
    [expression (number) const-exp]
    [expression (identifier) var-exp]
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression ("let" (arbno identifier "=" expression) "in" expression) let-exp]
    [expression ("proc" "(" (separated-list identifier ",") ")"expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]
    [expression ("letproc" identifier "=" "(" identifier ")" expression "in" expression) letproc-exp]
    [expression ("traceproc" "(" (separated-list identifier ",") ")" expression) traceproc-exp]))

(define scan&parse (sllgen:make-string-parser lex-a grammar-let))

(define-datatype program program?
  [a-program (exp1 expression?)])

(define-datatype expression expression?
  [const-exp
   (num number?)]
  [var-exp
   (var symbol?)]
  [diff-exp
   (exp1 expression?)
   (exp2 expression?)]
  [zero?-exp
   (exp1 expression?)]
  [if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?)]
  [let-exp
   (var (list-of symbol?))
   (exp1 (list-of expression?))
   (body expression?)]
  [proc-exp
   (vars (list-of symbol?))
   (body expression?)]
  [call-exp
   (rator expression?)
   (rand (list-of expression?))]
  [letproc-exp
   (proc-name symbol?)
   (args symbol?)
   (body1 expression?)
   (body2 expression?)]
  [traceproc-exp
   (vars (list-of symbol?))
   (body expression?)])

(define-datatype proc proc?
  [procedure
   (vars (list-of symbol?))
   (body expression?)
;   (env environment?)
   (trace boolean?)])

(define-datatype expval expval?
  [num-val
   (num number?)]
  [bool-val
   (bool boolean?)]
  [proc-val
   (proc proc?)])

(define expval->num
  (lambda (x)
    (cases expval x
      (num-val (num) num)
      (else (report-expval-extractor-error 'num x)))))

(define expval->bool
  (lambda (x)
    (cases expval x
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool x)))))

(define expval->proc
  (lambda (x)
    (cases expval x
      (proc-val (proc) proc)
      (else (report-expval-extractor-error 'proc x)))))

(define report-expval-extractor-error
  (lambda (type val)
    (eopl:error 'expval-extractors "looking for a ~s, find ~s" type val)))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      [const-exp (num) (num-val num)]
      [var-exp (var) (apply-env env var)]
      [diff-exp (exp1 exp2)
                (let ([val1 (value-of exp1 env)]
                      [val2 (value-of exp2 env)])
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val2)])
                    (num-val (- num1 num2))))]
      [zero?-exp (exp1)
                 (let ([val1 (value-of exp1 env)])
                   (let ([num1 (expval->num val1)])
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f))))]
      [if-exp (exp1 exp2 exp3)
              (let ([val1 (value-of exp1 env)])
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env)))]
      [let-exp (var exp1 body)
               (value-of body (extend-env-let var exp1 env))]
      [proc-exp (vars body)
                (proc-val (procedure vars body #f))]
      [traceproc-exp (vars body)
                     (proc-val (procedure vars body env #t))]
      [call-exp (rator rands)
                  (let ([proc1 (expval->proc (value-of rator env))]
                        [arg (args-cal rands env)])
                    (apply-procedure proc1 arg env))]
      [letproc-exp (proc-name id body1 body2)
                   (let ([val1 [proc-val (procedure (cons id '()) body1 env)]])
                     (value-of body2 (extend-env proc-name val1 env)))])))

(define args-cal
  (lambda (rands env)
    (if (null? rands)
        '()
        (cons (value-of (car rands) env)
              (args-cal (cdr rands) env)))))

(define apply-procedure
  (lambda (proc1 arg env)
    (cases proc proc1
      [procedure (vars body trace)
                 (when trace (begin (eopl:printf "trace enter ~a : ~a" (car vars) (car arg)) (newline)))
                 (let ((value (value-of body (extend-env-list vars arg env))))
                   (when trace (begin (display "trace exit") (newline)))
                   value)])))

;(define Y "let makerec = proc (f)
;                              let d = traceproc (x)
;                                      proc (z) ((f (x x)) z)
;                                  in proc (n) ((f (d d)) n)
;               in let maketimes4 = proc (f)
;                                        proc (x)
;                                             if zero?(x)
;                                                then 0
;                                                else -((f -(x, 1)), 4)
;                      in let times4 = (makerec maketimes4)
;                             in (times4 3)")
;
;(display (run Y))

;(define l1 "let a = 3 in let p = proc(x) -(x, a) a = 5 in -(a, (p 2))")
;(display (run l1))

(define l2 "let a = 3 in let p = proc (z) a in let f = proc (x) (p 0) in let a = 5 in (f 2)")
(define l3 "let a = 3 in let p = proc (z) a in let f = proc (a) (p 0) in let a = 5 in (f 2)")

(display (run l2))
(newline)
(display (run l3))