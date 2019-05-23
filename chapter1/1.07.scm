#lang racket

; my answer, use continuation passing style
(define nth-element
  (lambda (lst n f)
    (if (null? lst)
        (f '() n 'error) 
        (if (zero? n)
            (f (cons (car lst) '()) n (car lst))
            (nth-element (cdr lst)
                         (- n 1)
                         (lambda (newlat x ans)
                           (f (cons (car lst)
                                    newlat)
                              x
                              ans)))))))

(define col
  (lambda (l n v)
    (if (zero? n)
        v
        (display (list 'error l '(l is small))))))

(define m '(a b c d e f))

; Yukang's answer
(define nth-element
  (lambda (lst n)
    (let ((ans (nth-element-rec lst n)))
      (if (not ans)
          (report-list-too-short lst n)
          ans))))

(define nth-element-rec
  (lambda (lst n)
    (if (null? lst)
        #f
        (if (zero? n)
            (car lst)
            (nth-element-rec (cdr lst) (- n 1))))))

(define report-list-too-short
  (lambda (list nth)
    (error 'nth-element
           "list ~s too short by ~s elements .~%" list nth)))