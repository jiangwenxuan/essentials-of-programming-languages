#lang eopl

(define identifier?
  (lambda (x)
    (and (symbol? x)
         (not (eqv? x 'lambda))
         (not (eqv? x 'define)))))

(define list-of
  (lambda (pred)
    (lambda (x)
      (or (null? x)
          (and (pair? x)
               (pred (car x))
               ((list-of pred) (cdr x)))))))

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)))
  
(define the-grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)   
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression expression ")") call-exp)
    (expression ("%nameless-var" number) nameless-var-exp)
    (expression ("%let" expression "in" expression) nameless-let-exp)
    (expression ("%lexproc" expression) nameless-proc-exp)))

(define scan&parse (sllgen:make-string-parser the-lexical-spec the-grammar))

(define-datatype program program?
  (a-program (exp1 expression?)))

(define-datatype expression expression?
  (const-exp (num number?))
  (diff-exp (exp1 expression?)
            (exp2 expression?))
  (zero?-exp (exp1 expression?))
  (if-exp (exp1 expression?)
          (exp2 expression?)
          (exp3 expression?))
  (var-exp (var identifier?))
  (let-exp (var identifier?)
           (exp1 expression?)
           (body expression?))
  (proc-exp (var identifier?)
            (body expression?))
  (call-exp (rator expression?)
            (rand expression?))
  (nameless-var-exp (num number?))
  (nameless-let-exp (exp1 expression?)
                    (body expression?))
  (nameless-proc-exp (body expression?)))


(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (proc-val (proc proc?)))

(define-datatype proc proc?
  (procedure (body expression?)
             (saved-nameless-env nameless-environment?)))

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


; translator

(define empty-senv
  (lambda ()
    '()))

(define extend-senv
  (lambda (var senv)
    (cons var senv)))

(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv)
       (report-unbound-var var))
      ((eqv? var (car senv))
       0)
      (else
       (+ 1 (apply-senv (cdr senv) var))))))

(define init-senv
  (lambda ()
    (extend-senv 'i
                 (extend-senv 'v
                              (extend-senv 'x
                                           (empty-senv))))))

(define report-unbound-var
  (lambda (x)
    (eopl:error "~s is not bound" x)))

(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (a-program (translation-of exp1 (init-senv)))))))

(define translation-of
  (lambda (exp senv)
    (cases expression exp
      (const-exp (num)
                 (const-exp num))
      (diff-exp (exp1 exp2)
                (diff-exp (translation-of exp1 senv)
                          (translation-of exp2 senv)))
      (zero?-exp (exp1)
                 (zero?-exp (translation-of exp1 senv)))
      (if-exp (exp1 exp2 exp3)
              (if-exp (translation-of exp1 senv)
                      (translation-of exp2 senv)
                      (translation-of exp3 senv)))
      (var-exp (var)
               (nameless-var-exp (apply-senv senv var)))
      (let-exp (var exp1 body)
               (nameless-let-exp (translation-of exp1 senv)
                                 (translation-of body (extend-senv var senv))))
      (proc-exp (var body)
                (nameless-proc-exp (translation-of body (extend-senv var senv))))
      (call-exp (rator rand)
                (call-exp (translation-of rator senv)
                          (translation-of rand senv)))
      (else
       (report-invalid-source-expression exp)))))

(define report-invalid-source-expression
  (lambda (x)
    (eopl:error "the expression: ~s is not invalid" x)))


; interpreter

(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)))

(define empty-nameless-env
  (lambda ()
    '()))

(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))

(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))

(define init-nameless-env
  (lambda ()
    (extend-nameless-env (num-val 1)
                         (extend-nameless-env (num-val 5)
                                              (extend-nameless-env (num-val 10)
                                                                   (empty-nameless-env))))))

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (body saved-nameless-env)
                 (value-of body
                           (extend-nameless-env val saved-nameless-env))))))

(define run
  (lambda (string)
    (value-of-program (translation-of-program (scan&parse string)))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-nameless-env))))))

(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (diff-exp (exp1 exp2)
                (let ((val1 (expval->num (value-of exp1 nameless-env)))
                      (val2 (expval->num (value-of exp2 nameless-env))))
                  (num-val (- val1 val2))))
      (zero?-exp (exp1)
                 (let ((val1 (expval->num (value-of exp1 nameless-env))))
                   (if (zero? val1)
                       (bool-val #t)
                       (bool-val #f))))
      (if-exp (exp1 exp2 exp3)
              (if (expval->bool (value-of exp1 nameless-env))
                  (value-of exp2 nameless-env)
                  (value-of exp3 nameless-env)))
      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator nameless-env)))
                      (arg (value-of rand nameless-env)))
                  (apply-procedure proc arg)))
      (nameless-var-exp (x) (apply-nameless-env nameless-env x))
      (nameless-let-exp (exp1 body)
                        (let ((val (value-of exp1 nameless-env)))
                          (value-of body (extend-nameless-env val nameless-env))))
      (nameless-proc-exp (body)
                         (proc-val (procedure body nameless-env)))
      (else (eopl:error 'value-of "illegal expression in translated code: ~s" exp)))))


(define l1 "let x = 3 in -(x,1)")
(define l2 "(proc(f)(f 30)  proc(x) -(x,1))")

(display (run l1))
(newline)
(display (run l2))