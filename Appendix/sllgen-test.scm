#lang eopl

(define lex0
  '((whitespace (whitespace) skip)
    (commit ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define scan0 (sllgen:make-string-scanner lex0 '()))
;(display (scan0 "foo bar %here is a comment
;begin bax"))

(define-datatype statement statement?
  (compound-statement
   (stmt1 statement?)
   (stmt2 statement?))
  (while-statement
   (test expression?)
   (body statement?))
  (assign-statement
   (lhs symbol?)
   (rhs expression?)))

(define-datatype expression expression?
  (var-exp
   (id symbol?))
  (sum-exp
   (exp1 expression?)
   (exp2 expression?)))



; first test

(define simple-while-grammar
  '([statement
     ("begin" statement ";" statement "end")
     compound-statement]
    [statement
     ("while" expression "do" statement)
     while-statement]
    [statement
     (identifier ":=" expression)
     assign-statement]
    [expression
     (identifier)
     var-exp]
    [expression
     ("(" expression "+" expression ")")
     sum-exp]))

(define scan&parse-a
  (sllgen:make-string-parser lex0 simple-while-grammar))

(define stmt1 "begin x := foo ; while x do x := (x + bar) end")

(define show-the-datatypes-a
  (lambda () (sllgen:show-define-datatypes lex0 simple-while-grammar)))



; second test

(define the-grammar-1
  '([statement
     ("begin" (arbno statement ";") "end")
     compound-statement]
    [statement
     ("while" expression "do" statement)
     while-statement]
    [statement
     (identifier ":=" expression)
     assign-statement]
    [expression
     (identifier)
     var-exp]
    [expression
     ("(" expression "+" expression ")")
     sum-exp]))

(define show-the-datatypes-b
  (lambda () (sllgen:show-define-datatypes lex0 the-grammar-1)))

;(display (show-the-datatypes-b))
    


; third test

(define the-grammar-2
  '((statement
     ("begin"
      (separated-list
       (separated-list identifier ",")
       ":="
       (separated-list expression ",")
       ";")
      "end")
     compound-statement)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)))

(define show-the-datatypes-c
  (lambda () (sllgen:show-define-datatypes lex0 the-grammar-2)))

;(display (show-the-datatypes-c))