#lang racket

(provide (all-defined-out))
(require "semantic-actions.rkt")

(define type-err (list 'SEMANTIC 'ERROR))

(define type-check
  (lambda (ast)
    (cond ((program? ast) (make-program 
                            (type-check (program-definitions ast))))
          ((definitions? ast) (make-definitions 
                                (type-check (definitions-def ast))
                                (type-check (definitions-definitions ast))))
          ((def? ast) (make-def
                        (def-identifier ast)
                        (def-formals ast)
                        (def-type ast)
                        (type-check (def-body ast))))
          ((body? ast) (make-body (type-check (body-expr ast))))
          ((print-body? ast) (make-print-body
                               (type-check (print-body-print-expr ast))
                               (type-check (print-body-expr ast))))
          ((equals? ast) #FUNCTION TO DO COMPARISON)
          ((less-than? ast) #FUNCTION TO DO COMPARISON)
          ((addition? ast) #FUNCTION TO DO COMPARISON)
          ((subtraction? ast) #FUNCTION TO DO COMPARISON)
          ((or~? ast) #FUNCTION TO DO COMPARISON)
          ((multiplication? ast) #FUNCTION TO DO COMPARISON)
          ((division? ast) #FUNCTION TO DO COMPARISON)
          ((and~? ast) #FUNCTION TO DO COMPARISON)
          ((negative-value? ast) #FUNCTION TO DO COMPARISON)
          ((if~? ast) #FUNCTION TO DO COMPARISON)
          ((not? ast) #FUNCTION TO DO COMPARISON)
          ((boolean~? ast) #FUNCTION TO DO COMPARISON)
          ((number? ast) #FUNCTION TO DO COMPARISON) ;problem with name?
          ((nonemptyactuals? ast) #FUNCTION TO DO COMPARISON)
          ((function-call? ast) #FUNCTION TO DO COMPARISON) )))