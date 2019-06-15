#lang eopl

(define empty-env '())

(define empty-env? null?)

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (cons (cons saved-var saved-val) saved-env)))

(define apply-env
  (lambda (env search-var)
    (if (empty-env? env)
        (eopl:error 'apply-env "no binding for: ~s" search-var)
        (if (eqv? search-var (caar env))
            (cdar env)
            (apply-env (cdr env) search-var)))))

(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       empty-env)))))

(define environment? pair?)


; instead of symbol?, use identifier?
(define identifier?
  (lambda (x)
    (and (symbol? x)
         (not (eqv? x 'lambda))
         (not (eqv? x 'define)))))

; syntax data types for the LET language, scan&parse

(define lex-a
  '((whitespace (whitespace) skip)
    (commit ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar-let
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression expression ")") call-exp)
    (expression ("letproc" identifier "=" "(" identifier ")" expression "in" expression) letproc-exp)))

;(sllgen:show-define-datatypes lex-a grammar-let)
(define scan&parse (sllgen:make-string-parser lex-a grammar-let))


(define-datatype program program?
  [a-program (exp1 expression?)])

(define-datatype expression expression?
  [const-exp (num number?)]
  [diff-exp (exp1 expression?)
            (exp2 expression?)]
  [zero?-exp (exp1 expression?)]
  [if-exp (exp1 expression?)
          (exp2 expression?)
          (exp3 expression?)]
  [var-exp (var identifier?)]
  [let-exp (var identifier?)
           (exp1 expression?)
           (body expression?)]
  [proc-exp (var identifier?)
            (body expression?)]
  [call-exp (rator expression?)
            (rand expression?)]
  [letproc-exp (proc-name identifier?)
               (args identifier?)
               (body1 expression?)
               (body2 expression?)])

(define-datatype proc proc?
  [procedure (var identifier?)
             (body expression?)
             (saved-env environment?)])

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))

(define-datatype expval expval?
  [num-val (num number?)]
  [bool-val (bool boolean?)]
  [proc-val (proc proc?)])

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val)))))

(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else (report-expval-extractor-error 'proc val)))))

(define report-expval-extractor-error
  (lambda (type val)
    (eopl:error "expval's extractor is not suit the exp: ~s" val)))

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
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
                (let ([val1 (value-of exp1 env)]
                      [val2 (value-of exp2 env)])
                  (let ([num1 (expval->num val1)]
                        [num2 (expval->num val2)])
                    (num-val (- num1 num2)))))
      (zero?-exp (exp1)
                 (let ([val1 (value-of exp1 env)])
                   (let ([num1 (expval->num val1)])
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
              (let ([val1 (value-of exp1 env)])
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (let-exp (var exp1 body)
               (let ([val1 (value-of exp1 env)])
                 (value-of body (extend-env var val1 env))))
      (proc-exp (var body)
                (proc-val (procedure var body env)))
      (call-exp (rator rand)
                (let ([proc (expval->proc (value-of rator env))]
                      [arg (value-of rand env)])
                  (apply-procedure proc arg)))
      (letproc-exp (proc-name id body1 body2)
                   (let ([val (proc-val (procedure id body1 env))])
                     (value-of body2 (extend-env proc-name val env)))))))

(define l1 "-(6, i)")
(define l2 "let x = 200 in let f = proc (z) -(z, x) in let x = 100 in let g = proc (z) -(z, x) in -((f 1), (g 1))")
(define l3 "letproc f = (z) -(z, 1) in (f 2)")
(define l4 "let f = proc (x) proc (y) -(x, -(0, y)) in ((f 10) 20)")

(display (run l1))
(newline)
(display (run l2))
(newline)
(display (run l3))
(newline)
(display (run l4))