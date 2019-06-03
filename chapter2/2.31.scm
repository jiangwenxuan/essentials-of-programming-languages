#lang eopl

(define-datatype prefix-exp prefix-exp?
  [const-exp (num integer?)]
  [diff-exp (operand1 prefix-exp?)
            (operand2 prefix-exp?)])

(define make-prefix
  (lambda (l)
    (cond
      [(null? l)
       '()]
      [(integer? (car l))
       (cons (const-exp (car l))
             (cdr l))]
      [(eqv? (car l) '-)
       (if (null? (cdr l))
           (eopl:error 'make-prefix "wrong l")
           (let* ([next (make-prefix (cdr l))]
                  [op1 (car next)]
                  [next-next (make-prefix (cdr next))]
                  [op2 (car next-next)]
                  [rest (cdr next-next)])
             (cons (diff-exp op1 op2)
                   rest)))])))

(define convert-to-prefix
  (lambda (s)
    (car (make-prefix s))))

(display (convert-to-prefix '(- - 3 4 - 4 - 2 4)))