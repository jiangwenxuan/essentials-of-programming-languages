#lang eopl

(define list-of
  (lambda (pred)
    (lambda (x)
      (or (null? x)
          (and (pair? x)
               (pred (car x))
               ((list-of pred) (cdr x)))))))

(define lex-a
  '((whitespace (whitespace) skip)
    (commit ("%" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (identifier (letter (arbno (or letter digit))) symbol)))

(define grammar-letrec
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)
    (expression ("letrec" identifier "(" (arbno identifier) ")" "=" expression "in" expression) letrec-exp)))

(define scan&parse (sllgen:make-string-parser lex-a grammar-letrec))

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
   (var symbol?)
   (exp1 expression?)
   (exp2 expression?)]
  [proc-exp
   (var symbol?)
   (body expression?)]
  [call-exp
   (rator expression?)
   (rands (list-of expression?))]
  [letrec-exp
   (p-name symbol?)
   (b-var (list-of symbol?))
   (p-body expression?)
   (letrec-body expression?)])

(define-datatype proc proc?
  [procedure
   (vars (list-of symbol?))
   (body expression?)
   (env environment?)])

(define environment? pair?)

(define empty-env '())

(define empty-env? null?)

(define extend-env
  (lambda (saved-vars saved-vals saved-env)
    (cond
      [(null? saved-vars) saved-env]
      [(not (pair? saved-vars)) (cons (cons saved-vars saved-vals) saved-env)]
      [else
       (cons (cons (car saved-vars) (car saved-vals))
             (extend-env (cdr saved-vars) (cdr saved-vals) saved-env))])))

(define init-env
  (lambda ()
    (extend-env 'a (num-val 1) empty-env)))

(define extend-env-rec
  (lambda (p-name b-vars body saved-env)
    (let ([vec (make-vector 1)])
      (let ([new-env (extend-env p-name vec saved-env)])
        (vector-set! vec
                     0
                     (proc-val (procedure b-vars body new-env)))
        new-env))))

(define apply-env
  (lambda (env search-var)
    (cond
      [(empty-env? env) (report-no-binding-found search-var)]
      [(eqv? (caar env) search-var)
       (if (vector? (cdar env))
           (vector-ref (cdar env) 0)
           (cdar env))]
      [else
       (apply-env (cdr env) search-var)])))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error "there is not ~s in environment" search-var)))

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
    (eopl:error "expval extractor want ~s, the value is ~s" type val)))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1) (value-of exp1 (init-env))))))

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
                 (let ([num1 (expval->num (value-of exp1 env))])
                   (if (zero? num1)
                       (bool-val #t)
                       (bool-val #f)))]
      [if-exp (exp1 exp2 exp3)
              (let ([val1 (value-of exp1 env)])
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env)))]
      [let-exp (var exp1 body)
               (let ([val1 (value-of exp1 env)])
                 (value-of body (extend-env var val1 env)))]
      [proc-exp (var body)
                (proc-val (procedure var body env))]
      [call-exp (rator rands)
                (let ([proc (expval->proc (value-of rator env))]
                      [args (args-cal rands env)])
                  (apply-procedure proc args))]
      [letrec-exp (p-name b-vars p-body letrec-body)
                  (value-of letrec-body
                            (extend-env-rec p-name b-vars p-body env))])))

(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
      (procedure (var body env)
                 (value-of body (extend-env var args env))))))

(define args-cal
  (lambda (rands env)
    (if (null? rands)
        '()
        (cons (value-of (car rands) env)
              (args-cal (cdr rands) env)))))

(define l1 "letrec double (x y) = if zero?(x)
                                     then y
                                     else -((double -(x, 1) y), -2)
                   in (double 3 2)")

(display (run l1))