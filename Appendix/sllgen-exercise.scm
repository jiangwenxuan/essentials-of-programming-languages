#lang eopl

;(define-datatype statement statement?
;  [compound-statement
;   (compound-statements (list-of statement?))]
;  [while-statement
;   (test expression?)
;   (body statement?)]
;  [assign-statement
;   (lhs symbol?)
;   (rhs expression?)])
;
;(define-datatype expression expression?
;  [var-exp
;   (var symbol?)]
;  [diff-exp
;   (exp1 expression?)
;   (exp2 expression?)])
;
;(define grammar-a1
;  '((statement
;     ("{" statement ";" statement "}")
;     compound-statement)
;    (statement
;     ("while" expression "do" statement)
;     while-statement)
;    (statement
;     (identifier ":=" expression)
;     assign-statement)
;    (expression
;     (identifier)
;     var-exp)
;    (expression
;     ("(" expression "-" expression ")")
;     diff-exp)))

(define scanner-spec-a
  '((white-sp (whitespace) skip)
    (commit ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define list-of
  (lambda (pred)
    (lambda (x)
      (or (null? x) 
          (and (pair? x)
               (pred (car x))
               ((list-of pred) (cdr x)))))))

;(define scan&parse2
;  (sllgen:make-string-parser scanner-spec-a grammar-a1))
;
;(display (scan&parse2 "{x := foo; y := bar; z := uu;}"))

(define grammar-a3
  '((expression (identifier) var-exp)
    (expression
     ("let" (arbno identifier "=" expression) "in" expression)
     let-exp)))

(define scan&parse3
  (sllgen:make-string-parser scanner-spec-a grammar-a3))

(define-datatype expression expression?
  (var-exp (var-exp4 symbol?))
  (let-exp (let-exp9 (list-of symbol?))
           (let-exp7 (list-of expression?))
           (let-exp8 expression?))
  (num-exp (num number?)))

(display (scan&parse3 "let x = y u = v in z"))

(define grammar-a5
  '((statement
     ("{"
      (separated-list
       (separated-list identifier ",")
       ":="
       (separated-list expression ",")
       ";")
      "}")
     compound-statement)
    (expression (number) num-exp)
    (expression (identifier) var-exp)))

(define-datatype statement statement?
  (compound-statement
   (compound-statement4 (list-of (list-of symbol?)))
   (compound-statement3 (list-of (list-of expression?)))))

(define scan&parse5
  (sllgen:make-string-parser scanner-spec-a grammar-a5))

(display (scan&parse5 "{x, y := u, v; z := 4; t1, t2 := 5, 6}"))