#lang eopl

(define lex
  '((whitespace (whitespace) skip)
    (commit ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("list" "(" (separated-list expression ",") ")") list-exp)))

(define scan&parse
  (sllgen:make-string-parser lex grammar))

(define-datatype program program?
  [a-program (exp1 expression?)])

(define-datatype expression expression?
  [const-exp (exp1 number?)]
  [list-exp (exps (list-of expression?))])

(define list-of
  (lambda (pred)
    (lambda (x)
      (or (null? x)
          (and (pair? x)
               (pred (car x))
               ((list-of pred) (cdr x)))))))

(define run
  (lambda (x)
    (value-of-program (scan&parse x))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp)
                 (value-of exp)))))

(define value-of
  (lambda (exp)
    (cases expression exp
      (const-exp (x) x)
      (list-exp (exps) exps))))

(define l "list(1, 2, 3, 4, 5)")

(define a (run l))