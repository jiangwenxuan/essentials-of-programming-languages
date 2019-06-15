#lang eopl

; I don't dinish 3.11's code
; first, I want to group the (+, -, * /) as operator
; second, use a define-datatype to deal operator
; third, in the func: value-of, use a cases to deal detailed operators
; you can see my solutions in my code,
; but in the scan&parse, sllgen don't run as my expect
; so, I give up doing 3.11

; environment

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

(define apply-env
  (lambda (env search-var)
    (env search-var)))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "no binding for: ~s" search-var)))

(define init-env
  (lambda ()
    (extend-env 'i (num-val 1)
                (extend-env 'j (num-val 2)
                            (extend-env 'k (num-val 0)
                                        (empty-env))))))

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

; grammatical specification

(define lex-let
  '((whitespace (whitespace) skip)
    (commit ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar-let
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("minus" "(" expression ")") minus-exp)
;    (expression ("-" "(" expression "," expression ")") diff-exp)
;    (expression ("+" "(" expression "," expression ")") add-exp)
;    (expression ("*" "(" expression "," expression ")") multi-exp)
;    (expression ("/" "(" expression "," expression ")") quotient-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("list" "(" (separated-list expression ",") ")") list-exp)
    (expression (operator "(" expression "," expression ")") operator-exp)))

(define scan&parse
  (sllgen:make-string-parser lex-let grammar-let))

(define-datatype program program?
  [a-program (exp1 expression?)])

(define choose-operator
  (lambda (x)
    (lambda (y)
      (equal? x y))))

(define-datatype operator operator?
  [add-operator
   (op (choose-operator "add"))]
  [diff-operator
   (op (choose-operator "dif"))]
  [multi-operator
   (op (choose-operator "mul"))]
  [quotient-operator
   (op (choose-operator "quo"))])

(define-datatype expression expression?
  [const-exp (exp1 number?)]
  [var-exp (var identifier?)]
  [equal?-exp (exp1 expression?)
              (exp2 expression?)]
  [greater?-exp (exp1 expression?)
                (exp2 expression?)]
  [less?-exp (exp1 expression?)
             (exp2 expression?)]
  [minus-exp (exp1 expression?)]
  [operator-exp (op operator?)
                (exp1 expression?)
                (exp2 expression?)]
;  [diff-exp (exp1 expression?)
;            (exp2 expression?)]
;  [add-exp (exp1 expression?)
;           (exp2 expression?)]
;  [multi-exp (exp1 expression?)
;             (exp2 expression?)]
;  [quotient-exp (exp1 expression?)
;                (exp2 expression?)]
  [cons-exp (exp1 expression?)
            (exp2 expression?)]
  [zero?-exp (exp1 expression?)]
  [if-exp (exp1 expression?)
          (exp2 expression?)
          (exp3 expression?)]
  [let-exp (var identifier?)
           (exp1 expression?)
           (body expression?)]
  [car-exp (exp1 expression?)]
  [cdr-exp (exp1 expression?)]
  [null?-exp (exp1 expression?)]
  [emptylist-exp]
  [list-exp (exps (list-of expression?))])

; value extractors

(define-datatype expval expval?
  [num-val (num number?)]
  [bool-val (bool boolean?)]
  [cons-val (exp1 expval?)
            (exp2 expval?)]
  [emptylist-val])

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

(define expval->cons
  (lambda (val)
    (cases expval val
      (cons-val (construct1 construct2) (cons construct1 construct2))
      (else (report-expval-extractor-error 'cons val)))))

(define expval-car
  (lambda (val)
    (cases expval val
      (cons-val (construct1 construct2) construct1)
      (else (report-expval-extractor-error 'car val)))))

(define expval-cdr
  (lambda (val)
    (cases expval val
      (cons-val (construct1 construct2) construct2)
      (else (report-expval-extractor-error 'cdr val)))))

(define expval-null?
  (lambda (val)
    (cases expval val
      (emptylist-val () (bool-val #t))
      (else (bool-val #f)))))

(define list-val
  (lambda (exps)
    (if (null? exps)
        (emptylist-val)
        (cons-val (car exps)
                  (list-val (cdr exps))))))

(define report-expval-extractor-error
  (lambda (type val)
    (eopl:error
     "looking for a ~s, find ~s" type val)))

(define apply-elm
  (lambda (env)
    (lambda (x)
      (value-of x env))))

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
      (equal?-exp (exp1 exp2)
                  (let ([val1 (value-of exp1 env)]
                        [val2 (value-of exp2 env)])
                    (let ([num1 (expval->num val1)]
                          [num2 (expval->num val2)])
                      (if (eq? num1 num2)
                          (bool-val #t)
                          (bool-val #f)))))
      (greater?-exp (exp1 exp2)
                    (let ([val1 (value-of exp1 env)]
                          [val2 (value-of exp2 env)])
                      (let ([num1 (expval->num val1)]
                            [num2 (expval->num val2)])
                        (if (> num1 num2)
                            (bool-val #t)
                            (bool-val #f)))))
      (less?-exp (exp1 exp2)
                 (let ([val1 (value-of exp1 env)]
                       [val2 (value-of exp2 env)])
                   (let ([num1 (expval->num val1)]
                         [num2 (expval->num val2)])
                     (if (< num1 num2)
                         (bool-val #t)
                         (bool-val #f)))))
      (minus-exp (num) (num-val (- (expval->num (value-of num env)))))
      (operator-exp (operator-id exp1 exp2)
                    (let ([val1 (value-of exp1 env)]
                          [val2 (value-of exp2 env)])
                      (let ([num1 (expval->num val1)]
                            [num2 (expval->num val2)])
                        (cases operator operator-id
                          (add-operator
                           (op)
                           (num-val (+ num1 num2)))
                          (diff-operator
                           (op)
                           (num-val (- num1 num2)))
                          (multi-operator
                           (op)
                           (num-val (* num1 num2)))
                          (quotient-operator
                           (op)
                           (num-val (quotient num1 num2)))))))
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
      (let-exp (id exp1 body)
               (let ([val1 (value-of exp1 env)])
                 (value-of body (extend-env id val1 env))))
      (cons-exp (exp1 exp2)
                (cons-val (value-of exp1 env)
                          (value-of exp2 env)))
      (car-exp (exp1)
               (expval-car (value-of exp1 env)))
      (cdr-exp (exp1)
               (expval-cdr (value-of exp1 env)))
      (null?-exp (exp1)
                 (let ([val1 (value-of exp1 env)])
                   (expval-null? (value-of exp env))))
      (emptylist-exp ()
                     (emptylist-val))
      (list-exp (exps)
                (list-val (map (apply-elm env) exps))))))

 
(define s1 "add (i, minus(j))")
(define s2 "let x = 0 in if zero?(0) then minus(8) else minus(i)")
(define s3 "let a = 9 in let b = 10 in if greater?(a, b) then mul (a, b) else dif (a, b)") 
(define s4 "let x = 4 in cons(x, cons(cons(dif (x, 1), emptylist), emptylist))")
(define s5 "list(1, 2, 3)")
(define s6 "let x = 4 in list(x, dif (x, 1), dif (x, 3))")

(display (run s1))
(newline)
(display (run s2))
(newline)
(display (run s3))
(newline)
(display (run s4))
(newline)
(display (run s5))
(newline)
(display (run s6))