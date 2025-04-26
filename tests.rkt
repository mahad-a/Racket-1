#lang racket
; Each of the test cases worth 1 grade
(quote 1)
(quote (1 2))
(quote (1 2 3))
(quote 1 2 3) ;Error
(quote (1 2 3) (4 5 6));Error

; 1 grade for procedure map-1-exp?
(map-1 + '(1 2 3 4)) ;'(1 2 3 4)
(map-1 + '(1 2 3 4) '(10 20 30 40));'(11 22 33 44)
(map-1 abs '(-1 -2 3 4)) ;'(1 2 3 4)
(map-1 (lambda (x) (* x x)) '(1 2 3 4)) ;'(1 4 9 16)
(map-1 (lambda (x y) (* x y)) '(1 2 3 4) '(10 20 30 40)) ;'(10 40 90 160)
(map-1 (lambda (x y) (* x y)) '(1 2 3 4) );error
(map-1 (lambda (x ) (* x 1 )) '(1 2 3 4) );'(1 2 3 4)
(map-1 abs '(-1 -2 3 4) '(-1 -2 3 4));error
(map-1 (lambda (x y) (* x x)) '(1 2 3 4) '(10 20 30 40)) ; '(1 4 9 16)



; 1 grade for where did the student add the and_exp? condition
(and (= 5 (+ 2 4)) (* 3 4)) ;#f  ; (* 3 4) is not evaluated
(and (= 5 (+ 2 3)) (* 3 4)) ;12
(and (= 5 (+ 2 3)) (< 6 (* 4 3))) ;#t
(and (= 5 (+ 2 4)) (< 6 (* 4 3))) ;#f  ; (< 6 (* 4 3)) is not evaluated


