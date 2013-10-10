#lang racket

(define amt-of-columns 22)
(define row-in-table (lambda () (make-vector amt-of-columns))); lambda so it re-evaluates on each call. (so it doesn't give back same vector each time)

(define amt-of-rows 23)

(define parse-table (vector-map (lambda (x) (row-in-table)) (make-vector amt-of-rows)));fills table with vectors

(define-values 
  (program
   defintions
   defintions-prime
   def
   formals
   non-empty-formals
   non-empty-formals-prime
   formal
   body
   type
   expr
   expr-prime
   simple-expr
   simple-expr-prime
   term
   term-prime
   factor
   factor-prime
   actuals
   non-empty-actuals
   non-empty-actuals-prime
   literal
   print)
  (values 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22)); this should be done for columns as well

(define add-rule
  (lambda (row col rule)
    (vector-set! (vector-ref parse-table row) col rule))) 
(add-rule program 0 '(definitions))
(add-rule defintions 0 '(def defintions-prime))

;map over and apply and you can fill rows out really easily??? **
parse-table