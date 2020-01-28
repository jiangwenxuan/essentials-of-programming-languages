#lang eopl

(define l1 "let x = 0
                in letrec even(dummy) = if zero?(x)
                                        then 1
                                        else begin
                                             set x = -(x,1);
                                             (odd 888)
                                             end
                          odd(dummy) = if zero?(x)
                                       then 0
                                       else begin
                                            set x = -(x,1);
                                            (even 888)
                                            end
                          in begin
                             set x = 13;
                             (odd -888)
                             end")

(define l2 "let g = let count = 0
                        in proc (dummy)
                                begin
                                set count = -(count,-1);
                                count
                                end
                in let a = (g 11)
                       in let b = (g 11)
                              in -(a,b)")
