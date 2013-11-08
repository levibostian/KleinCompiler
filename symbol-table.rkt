#lang racket

(provide (all-defined-out))
(require "semantic-actions.rkt"
         "parser.rkt")

(define symbol-table
  (lambda (ast)
    (cond ((program? ast) (make-hash (symbol-table (program-definitions ast))))
          ((definitions? ast) (append (symbol-table (definitions-def ast))
                                      (symbol-table (definitions-definitions ast))))
          ((def? ast) (list (cons (identifier-value (def-id ast)) (make-hash (list (cons 'parameters (make-hash (symbol-table (def-formals ast))))
                                                                                   (cons 'amt-of-params (amt-of-params-for (def-formals ast)))
                                                                                   (cons 'type (type-value (def-type ast))))))))
          ((nonemptyformals? ast) (append (symbol-table (nonemptyformals-formal ast))
                                          (symbol-table (nonemptyformals-nonemptyformals ast))))
          ((formal? ast) (list (cons (identifier-value (formal-id ast)) (type-value (formal-type ast)))))
          ((empty-formals? ast) (list (cons '**null** '**null**)))
          (else (display "ERROR WITH SYMBOL-TABLE, NOTHING MATCHED")
                (display ast)) )))

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

(define test (symbol-table (parser "klein-programs/test.kln")))

(define lookup-error-param
  (lambda (param function)
    (list 'param: param 'does 'not 'exist 'for 'function: function)))

(define get-id-type-for-def
  (lambda (ident function sym-table error-type)
    (hash-ref (hash-ref (hash-ref sym-table function) 'parameters) ident error-type)))


;{ 
;  function-name1: 
;  {
;    parameters: 
;    {
;      formal: type,
;      formal: type
;    }
;   amt-of-params: number
;   type: type
;  }
;  function-name2:
;  {...}
;  function-name3:
;  {...}
;}

;(make-hash (list (cons 'main (make-hash (list (cons "parameters" (make-hash (list (cons 'x 'int) (cons 'y 'bool)))) (cons 'num-of 2) (cons 'type 'int))))))