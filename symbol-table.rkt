#lang racket

(provide (all-defined-out))
(require "semantic-actions.rkt"
         "parser.rkt"
         "symbol-table-helpers.rkt")

(define symbol-table
  (lambda (ast)
    (symbol-table-helper ast 0)))

(define symbol-table-helper
  (lambda (ast pos-of-formal)
    (cond ((program? ast) (make-hash (symbol-table-helper (program-definitions ast) pos-of-formal)))
          ((definitions? ast) (append (symbol-table-helper (definitions-def ast) pos-of-formal)
                                      (symbol-table-helper (definitions-definitions ast) pos-of-formal)))
          ((def? ast) (list (cons (identifier-value (def-id ast)) (make-hash (list (cons 'parameters (make-hash (symbol-table-helper (def-formals ast) pos-of-formal)))
                                                                                   (cons 'amt-of-params (amt-of-params-for (def-formals ast)))
                                                                                   (cons 'type (type-value (def-type ast))))))))
          ((nonemptyformals? ast) (append (symbol-table-helper (nonemptyformals-formal ast) pos-of-formal)
                                          (symbol-table-helper (nonemptyformals-nonemptyformals ast) (+ 1 pos-of-formal))))
          ((formal? ast) (let ((type-of-formal (type-value (formal-type ast))))
                           (list (cons (identifier-value (formal-id ast)) type-of-formal)
                                 (cons pos-of-formal type-of-formal)
                                 (cons (string->symbol (string-append (symbol->string (identifier-value (formal-id ast))) "~")) pos-of-formal) )))
          ((empty-formals? ast) (list (cons '**null** '**null**)))
          (else (display "ERROR WITH symbol-table-helper, NOTHING MATCHED")
                (display ast)) )))


(define test (symbol-table (parser "klein-programs/test.kln")))

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