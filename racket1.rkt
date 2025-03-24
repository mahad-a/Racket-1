#lang racket

;;(require (planet dyoo/simply-scheme))
;;(provide (all-defined-out))


;; Simple evaluator for Racket without DEFINE, using substitution model.
;; Version 1: No DEFINE, only primitive names are global.

;; The "read-eval-print loop" (REPL):

(define (racket-1)
  (newline)
  (display "Racket-1: ")
  (flush-output)
  (print (eval-1 (read)))
  (newline)
  (racket-1)
  )

;; Two important procedures:
;; EVAL-1 takes an expression and returns its value.
;; APPLY-1 takes a procedure and a list of actual argument values, and
;;  calls the procedure.
;; They have these names to avoid conflict with Racket's EVAL and APPLY,
;;  which have similar meanings.

;; Comments on EVAL-1:

;; There are four basic expression types in Racket:
;;    1. self-evaluating (a/k/a constant) expressions: numbers, #t, etc.
;;    2. symbols (variables)
;;    3. special forms (in this evaluator, just QUOTE, IF, and LAMBDA)
;;    4. procedure calls (can call a primitive or a LAMBDA-generated procedure)

;; 1.  The value of a constant is itself.  Unlike real Racket, an Racket
;; procedure is here considered a constant expression.  You can't type in
;; procedure values, but the value of a global variable can be a procedure,
;; and that value might get substituted for a parameter in the body of a
;; higher-order function such as MAP, so the evaluator has to be ready to
;; see a built-in procedure as an "expression."  Therefore, the procedure
;; CONSTANT? includes a check for (PROCEDURE? EXP).

;; 2.  In the substitution model, we should never actually evaluate a *local*
;; variable name, because we should have substituted the actual value for
;; the parameter name before evaluating the procedure body.

;; In this simple evaluator, there is no DEFINE, and so the only *global*
;; symbols are the ones representing primitive procedures.  We cheat a little
;; by using Racket's EVAL to get the values of these variables.

;; 3.  The value of the expression (QUOTE FOO) is FOO -- the second element of
;; the expression.

;; To evaluate the expression (IF A B C) we first evaluate A; then, if A is
;; true, we evaluate B; if A is false, we evaluate C.

;; The value of a LAMBDA expression is the expression itself.  There is no
;; work to do until we actually call the procedure.  (This won't be true
;; when we write a more realistic interpreter that handles more Racket
;; features, but it works in the substitution model.)

;; 4.  To evaluate a procedure call, we recursively evaluate all the
;; subexpressions.  We call APPLY-1 to handle the actual procedure invocation.

(define (eval-1 exp)
  (cond ((constant? exp) exp)
    ((symbol? exp) (eval exp))  ; use underlying Racket's EVAL
    ; Exercise 5
    ((quote-exp? exp)
         (if (= (length exp) 2) ; first check to ensure that there is only 2 arguments (quote and arg)
             (cadr exp) ; evaluate and return quoted value if so
             (error "quote can only have one parameter"))) ; otherwise throw error
    ; Exercise 6 
    ((map-exp? exp)
         (cond
           [(empty? (cdddr exp))
            (map (eval (cadr exp)) (eval (caddr exp)))]
           [else
            (map (eval (cadr exp)) (eval (caddr exp)) (eval (cadddr exp)))]))
     

    ; Exercise 7
    ((and-exp? exp)
         (define (eval-1-and exp-1)
           (cond
             [(null? exp-1) #t]
             [(null? (cdr exp-1)) (eval-1 (car exp-1))]
             [(equal? (eval-1 (car exp-1)) #f) #f]
             [else (eval-1-and (cdr exp-1))]))
         (eval-1-and (cdr exp)))
    
    ((if-exp? exp)
     (if (eval-1 (cadr exp))
         (eval-1 (caddr exp))
         (eval-1 (cadddr exp))))
    ((lambda-exp? exp) exp)
    ((pair? exp) (apply-1 (eval-1 (car exp))      ; eval the operator
                  (map eval-1 (cdr exp))))
    (else (error "bad expr: " exp))))


;; Comments on APPLY-1:

;; There are two kinds of procedures: primitive and LAMBDA-created.

;; We recognize a primitive procedure using the PROCEDURE? predicate in
;; the underlying Racket interpreter.

;; If the procedure isn't primitive, then it must be LAMBDA-created.
;; In this interpreter (but not in later, more realistic ones), the value
;; of a LAMBDA expression is the expression itself.  So (CADR PROC) is
;; the formal parameter list, and (CADDR PROC) is the expression in the
;; procedure body.

;; To call the procedure, we must substitute the actual arguments for
;; the formal parameters in the body; the result of this substitution is
;; an expression which we can then evaluate with EVAL-1.

(define (apply-1 proc args)
  (cond ((procedure? proc)  ; use underlying Racket's APPLY
     (apply proc args))
    ((lambda-exp? proc)
     (eval-1 (substitute (caddr proc)   ; the body
                 (cadr proc)    ; the formal parameters
                 args           ; the actual arguments
                 '())))     ; bound-vars, see below
    (else (error "bad proc: " proc))))


;; Some trivial helper procedures:

(define (constant? exp)
  (or (number? exp) (boolean? exp) (string? exp) (procedure? exp)))

(define (exp-checker type)
  (lambda (exp) (and (pair? exp) (eq? (car exp) type))))

;; Exercise 5 old solution
; (define quote-exp? (exp-checker 'quote))

;; fixed solution
(define (quote-exp? exp)
  (and (pair? exp)  ; check if the expression is a pair (quote and parameters)
       (eq? (car exp) 'quote) ; first item in list is 'quote
       (= (length exp) 2))) ; only 2 items in list (quote and parameters)

;; Exercise 6 solution
(define (map-exp? exp)
  (and (exp-checker 'map-1)
       (eq? (car exp) 'map-1)))

;; Exercise 7 solution
(define (and-exp? exp)
  (and (pair? exp)
       (eq? (car exp) 'and)))

(define if-exp? (exp-checker 'if))
(define lambda-exp? (exp-checker 'lambda))


;; SUBSTITUTE substitutes actual arguments for *free* references to the
;; corresponding formal parameters.  For example, given the expression
;;
;;  ((lambda (x y)
;;     ((lambda (x) (+ x y))
;;      (* x y)))
;;   5 8)
;;
;; the body of the procedure we're calling is
;;
;;     ((lambda (x) (+ x y))
;;      (* x y))
;;
;; and we want to substitute 5 for X and 8 for Y, but the result should be
;;
;;     ((lambda (x) (+ x 8))
;;      (* 5 8))
;;
;; and *NOT*
;;
;;     ((lambda (5) (+ 5 8))
;;      (* 5 8))
;;
;; The X in (* X Y) is a "free reference," but the X in (LAMBDA (X) (+ X Y))
;; is a "bound reference."
;;
;; To make this work, in its recursive calls, SUBSTITUTE keeps a list of
;; bound variables in the current subexpression -- ones that shouldn't be
;; substituted for -- in its argument BOUND.  This argument is the empty
;; list in the top-level call to SUBSTITUTE from APPLY-1.

;; Another complication is that when an argument value isn't a self-evaluating
;; expression, we actually want to substitute the value *quoted*.  For example,
;; consider the expression
;;
;;  ((lambda (x) (first x)) 'foo)
;;
;; The actual argument value is FOO, but we want the result of the
;; substitution to be
;;
;;  (first 'foo)
;;
;; and not
;;
;;  (first foo)
;;
;; because what we're going to do with this expression is try to evaluate
;; it, and FOO would be an unbound variable.

;; There is a strangeness in MAYBE-QUOTE, which must handle the
;; case of a primitive procedure as the actual argument value; these
;; procedures shouldn't be quoted.

(define (substitute exp params args bound)
  (cond ((constant? exp) exp)
    ((symbol? exp)
     (if (memq exp bound)
         exp
         (lookup exp params args)))
    ((quote-exp? exp) exp)
    ((lambda-exp? exp)
     (list 'lambda
           (cadr exp)
           (substitute (caddr exp) params args (append bound (cadr exp)))))
    (else (map (lambda (subexp) (substitute subexp params args bound))
           exp))))

(define (lookup name params args)
  (cond ((null? params) name)
    ((eq? name (car params)) (maybe-quote (car args)))
    (else (lookup name (cdr params) (cdr args)))))

(define (maybe-quote value)
  (cond ((lambda-exp? value) value)
    ((constant? value) value)
    ((procedure? value) value)  ; real Racket primitive procedure
    (else (list 'quote value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sample evaluation, computing factorial of 5:

; Racket-1: ((lambda (n)
;          ((lambda (f) (f f n))
;       (lambda (f n)
;          (if (= n 0)
;              1
;              (* n (f f (- n 1))) )) ))
;        5)
; 120

;; Sample evaluation, using a primitive as argument to MAP:

; Racket-1: ((lambda (f n)
;          ((lambda (map) (map map f n))
;          (lambda (map f n)
;            (if (null? n)
;                '()
;            (cons (f (car n)) (map map f (cdr n))) )) ))
;         first
;         '(the rain in spain))
; (t r i s)

;; Exercise 6 map-1
(define (map-1 f lst)
  (map f lst))
#|
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
|#