#lang eopl

(define l1 "let g = let counter = newref(0)
                        in proc (dummy)
                                begin setref(counter, -(deref(counter), -1));
                                      deref(counter)
                                end
                in let a = (g 11)
                       in let b = (g 11)
                          in -(a,b)")

(define l2 "let g = proc (dummy)
                         let counter = newref(0)
                             in begin
                                setref(counter, -(deref(counter),-1));
                                deref(counter)
                                end
                in let a = (g 11)
                       in let b = (g 11)
                              in -(a,b)")

(define l3 "let x = newref(newref(0))
                in begin
                   setref(deref(x), 11);
                   deref(deref(x))
                   end")