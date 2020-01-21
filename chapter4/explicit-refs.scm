#lang eopl

; this program is not correct, I will check it later

(define list-of
  (lambda (pred)
    (lambda (x)
      (or (null? x)
          (and
           (pair? x)
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
    (expression ("letrec" (arbno identifier "(" identifier ")" "=" expression) "in" expression) letrec-exp)
    (expression ("begin" expression (arbno ";" expression) "end") bend-exp)
    (expression ("newref" "(" expression ")") newref-exp)
    (expression ("deref" "(" expression ")") deref-exp)
    (expression ("setref" "(" expression "," expression ")") setref-exp)))

(define scan&parse (sllgen:make-string-parser lex-a grammar-letrec))

(define-datatype program program?
  (a-program (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (var-exp
   (var symbol?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (let-exp
   (var symbol?)
   (exp1 expression?)
   (exp2 expression?))
  (proc-exp
   (var symbol?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (letrec-exp
   (p-name (list-of symbol?))
   (b-var (list-of symbol?))
   (p-body (list-of expression?))
   (letrec-body expression?))
  (bend-exp
   (exp1 expression?)
   (exps (list-of expression?)))
  (newref-exp
   (exp1 expression?))
  (deref-exp
   (exp1 expression?))
  (setref-exp
   (exp1 expression?)
   (exp2 expression?)))

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?))
  (ref-val
   (ref reference?)))

(define-datatype proc proc?
  (procedure
   (var symbol?)
   (body expression?)
   (env environment?)))

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

(define expval->ref
  (lambda (x)
    (cases expval x
      (ref-val (ref) ref)
      (else (report-expval-extractor-error 'ref x)))))

(define report-expval-extractor-error
  (lambda (type val)
    (eopl:error "expval extractor want ~s, the value is ~s" type val)))

(define environment? pair?)

(define empty-env
  (lambda ()
    '()))

(define empty-env? null?)

(define extend-env
  (lambda (saved-vars saved-vals saved-env)
    (cond
      [(null? saved-vars) saved-env]
      [(not (pair? saved-vars)) (cons (cons saved-vars saved-vals) saved-env)]
      [else
       (cons (cons (car saved-vars) (car saved-vals))
             (extend-env (cdr saved-vars) (cdr saved-vals) saved-env))])))


(define extend-env-rec
  (lambda (p-names b-vars bodys saved-env)
    (let ([new-env (mutually-rec p-names saved-env)])
      (modify-env p-names b-vars bodys new-env new-env))))
  
(define mutually-rec
  (lambda (p-names saved-env)
    (if (null? p-names)
        saved-env
        (let ([vec (make-vector 1)])
          (cons (cons (car p-names) vec)
                (mutually-rec (cdr p-names) saved-env))))))

(define modify-env
  (lambda (p-names b-vars bodys pointer env)
    (if (null? p-names)
        pointer
        (begin
          (vector-set! (cdar env) 0 (proc-val (procedure (car b-vars) (car bodys) pointer)))
          (modify-env (cdr p-names) (cdr b-vars) (cdr bodys) pointer (cdr env))))))


(define apply-env
  (lambda (env search-var)
    (cond
      ((empty-env? env)
       (report-no-binding-found search-var))
      ((eqv? (caar env) search-var)
       (if (vector? (cdar env))
           (vector-ref (cdar env) 0)
           (cdar env)))
      (else
       (apply-env (cdr env) search-var)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error "there is not ~s in the environment" search-var)))


(define empty-store
  (lambda () '()))

; a scheme variable containing the current state
; of the store. initially set to a dummy value.

(define the-store 'uninitialized)

(define get-store
  (lambda () the-store))

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define reference?
  (lambda (v)
    (integer? v)))

(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))

(define deref
  (lambda (ref)
    (list-ref the-store ref)))

(define setref!
  (lambda (ref val)
    (set! the-store
          (letrec ((setref-inner
                    (lambda (store1 ref1)
                      (cond
                        ((null? store1)
                         (report-invalid-reference ref))
                        ((zero? ref1)
                         (cons val (cdr store1)))
                        (else
                         (cons (car store1)
                               (setref-inner (cdr store1) (- ref1 1))))))))
            (setref-inner the-store ref)))))

(define report-invalid-reference
  (lambda (ref l)
    (eopl:error "there isn't ~s" ref)))

(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
      (procedure (var body env)
                 (value-of body (extend-env var args env))))))

(define args-cal
  (lambda (rands env)
    (if (not (pair? rands))
        (cons (value-of rands env) '())
        (if (null? rands)
            '()
            (cons (value-of (car rands) env)
                  (args-cal (cdr rands) env))))))

(define value-of-exps
  (lambda (exp1 exps env)
    (if (null? exps)
        (value-of exp1 env)
        (value-of-more-exp exps env))))

(define value-of-more-exp
  (lambda (exps env)
    (if (null? (cdr exps))
        (value-of (car exps) env)
        (begin (value-of (car exps) env)
               (value-of-more-exp (cdr exps) env)))))
        

(define init-env
  (lambda ()
    (empty-env)))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num)
                 (num-val num))
      (var-exp (var)
               (apply-env env var))
      (diff-exp (exp1 exp2)
                (let ((num1 (expval->num (value-of exp1 env)))
                      (num2 (expval->num (value-of exp2 env))))
                  (num-val (- num1 num2))))
      (zero?-exp (exp1)
                 (let ((num1 (expval->num (value-of exp1 env))))
                   (if (zero? num1)
                       (bool-val #t)
                       (bool-val #f))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (let-exp (var exp body)
               (let ((val1 (value-of exp env)))
                 (value-of body (extend-env var val1 env))))
      (proc-exp (var body)
                (proc-val (procedure var body env)))
      (call-exp (rator rands)
                (let ([proc (expval->proc (value-of rator env))]
                      [args (args-cal rands env)])
                  (apply-procedure proc args)))
      (letrec-exp (p-names b-vars p-bodys letrec-body)
                  (value-of letrec-body
                            (extend-env-rec p-names b-vars p-bodys env)))
      (bend-exp (exp1 exps)
                (value-of-exps exp1 exps env))
      (newref-exp (exp1)
                  (let ((v1 (value-of exp1 env)))
                    (ref-val (newref v1))))
      (deref-exp (exp1)
                 (let ((v1 (value-of exp1 env)))
                   (let ((ref1 (expval->ref v1)))
                     (deref ref1))))
      (setref-exp (exp1 exp2)
                  (let ((ref (expval->ref (value-of exp1 env))))
                    (let ((val2 (value-of exp2 env)))
                      (begin
                        (setref! ref val2)
                        (num-val 23))))))))


(define l1 "let x = newref(0)
                in letrec even(dummy) = if zero?(deref(x))
                                        then 1
                                        else begin
                                             setref(x, -(deref(x),1));
                                             (odd 888)
                                             end
                          odd(dummy) = if zero?(deref(x))
                                       then 0
                                       else begin
                                            setref(x, -(deref(x),1));
                                            (even 888)
                                            end
                          in begin setref(x,13);
                                   (odd 888)
                             end")

(define l2 "let g = let counter = newref(0)
                        in proc (dummy)
                                begin setref(counter, -(deref(counter), -1));
                                      deref(counter)
                                end
                in let a = (g 11)
                       in let b = (g 11)
                          in -(a,b)")

(display (run l1))
(newline)
(display (run l2))