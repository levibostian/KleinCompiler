#lang racket

(provide (all-defined-out))
(require "semantic-actions.rkt"
         "data-types.rkt")

(define amt-of-params-for
  (lambda (some-formals)
    (amt-of-params-for-helper some-formals 0)))

(define amt-of-params-for-helper
  (lambda (some-formals amt)
    (cond 
      ((nonemptyformals? some-formals) (amt-of-params-for-helper (nonemptyformals-nonemptyformals some-formals) (+ 1 amt)))
      ((formal? some-formals) (+ 1 amt))
      ((empty-formals? some-formals) 0)
      (else (display "PROBLEM WITH amt-of-params-for-helper")))))

(define lookup-error-param
  (lambda (param function)
    (list 'param: param 'does 'not 'exist 'for 'function: function)))

(define get-id-type-for-def
  (lambda (ident function sym-table error-type)
    (hash-ref (hash-ref (hash-ref sym-table function) 'parameters) ident error-type)))

(define get-formal-by-pos
  (lambda (function sym-table position error-type)
    (hash-ref (hash-ref (hash-ref sym-table function) 'parameters) position error-type)))

(define get-function-type
  (lambda (function sym-table error-type)
    (if (member? function (hash-keys sym-table))
        (hash-ref (hash-ref sym-table function) 'type)
        error-type)))
    
(define valid-function?
  (lambda (function sym-table error-type)
    (member? function (hash-keys sym-table))))

(define no-user-defined-print?
  (lambda (sym-table)
    (if (member? 'print (hash-keys sym-table))
        #f
        #t)))
 