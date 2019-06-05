#lang eopl

(define-datatype statement statement?
  [compound-statement
   (stmt1 statement?)
   (stmt2 statement?)]
  [while-statement
   (test expression?)
   (body statement?)]
  [assign-statement
   (lhs symbol?)
   (rhs expression?)])

(define-datatype expression expression?
  [var-exp
   (var symbol?)]
  [diff-exp
   (exp1 expression?)
   (exp2 expression?)])

(define grammar-a1
  '((statement
     ("{" statement ";" statement "}")
     compound-statement)
    (statement
     ("while" expression "do" statement)
     while-statement)
    (statement
     (identifier ":=" expression)
     assign-statement)
    (expression
     (identifier)
     var-exp)
    (expression
     ("(" expression "-" expression ")")
     diff-exp)))