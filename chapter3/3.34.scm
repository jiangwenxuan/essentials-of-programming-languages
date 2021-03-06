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
    (expression ("(" expression expression ")") call-exp)
    (expression ("letrec" (arbno identifier "(" identifier ")" "=" expression) "in" expression) letrec-exp)))

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
   (rand expression?)]
  [letrec-exp
   (p-names (list-of symbol?))
   (b-vars (list-of symbol?))
   (p-bodys (list-of expression?))
   (letrec-body expression?)])

(define-datatype proc proc?
  [procedure
   (var symbol?)
   (body expression?)
   (env environment?)])

(define environment? procedure?)

(define empty-env
  (lambda ()
    (lambda (search-var)
      (report-no-binding-found search-var))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

(define extend-env-rec
  (lambda (p-names b-vars p-bodys saved-env)
    (define env-rec
      (lambda (x)
        (let ([m (find-rec p-names b-vars p-bodys x)])
          (if (pair? m)
              (proc-val (procedure (car m) (cdr m) env-rec))
              (apply-env saved-env x)))))
    (lambda (search-var)
      (env-rec search-var))))

(define find-rec
  (lambda (p-names b-vars p-bodys x)
    (cond
      [(null? p-names)
       #f]
      [(eqv? x (car p-names))
       (cons (car b-vars) (car p-bodys))]
      [else
       (find-rec (cdr p-names) (cdr b-vars) (cdr p-bodys) x)])))

(define apply-env
  (lambda (env search-var)
    (env search-var)))

(define init-env
  (lambda ()
    (empty-env)))

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
      [call-exp (rator rand)
                (let ([proc (expval->proc (value-of rator env))]
                      [args (value-of rand env)])
                  (apply-procedure proc args))]
      [letrec-exp (p-names b-vars p-bodys letrec-body)
                  (value-of letrec-body
                            (extend-env-rec p-names b-vars p-bodys env))])))

(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
      (procedure (var body env)
                 (value-of body (extend-env var arg env))))))

(define l1 "letrec double (x) = if zero?(x)
                                   then 0
                                   else -((double -(x, 1)), -2)
                   in (double 6)")

(define l2 "letrec odd (x) = if zero?(x) then 0 else (even -(x, 1))
                   even (x) = if zero?(x) then 1 else (odd -(x, 1))
                   in (odd 13)")

(display (run l1))
(newline)
(display (run l2))