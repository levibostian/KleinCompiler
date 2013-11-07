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
                        (def-id ast)
                        (def-formals ast)
                        (def-type ast)
                        (type-check (def-body ast))))
          ((body? ast) (make-body (type-check (body-expr ast))))
          ((print-body? ast) (make-print-body
                               (type-check (print-body-print-expr ast))
                               (type-check (print-body-expr ast))))
          ((equals? ast)  (make-equals 
                            (equals-left ast)
                            (equals-right ast)
                            (is-each-side-integer?
                             (get-type-of-exp (type-check (equals-left ast)))
                             (get-type-of-exp (type-check (equals-right ast))) )))
          ((less-than? ast) (make-less-than 
                            (less-than-left  ast)
                            (less-than-right ast)
                            (is-each-side-integer?
                             (get-type-of-exp (type-check (less-than-left  ast)))
                             (get-type-of-exp (type-check (less-than-right ast))) )))
          ((addition? ast) (make-addition
                            (addition-left  ast)
                            (addition-right ast)
                            (is-each-side-integer?
                             (get-type-of-exp (type-check (addition-left  ast)))
                             (get-type-of-exp (type-check (addition-right ast))) )))
          ((subtraction? ast) (make-subtraction
                                (subtraction-left  ast)
                                (subtraction-right ast)
                                (is-each-side-integer?
                                 (get-type-of-exp (type-check (subtraction-left  ast)))
                                 (get-type-of-exp (type-check (subtraction-right ast))) )))
          ((or~? ast) (make-or~
                        (or~-left  ast)
                        (or~-right ast)
                        (is-each-side-boolean?
                         (get-type-of-exp (type-check (or~-left  ast)))
                         (get-type-of-exp (type-check (or~-right ast))) )))
          ((multiplication? ast) (make-multiplication
                                   (multiplication-left  ast)
                                   (multiplication-right ast)
                                   (is-each-side-integer?
                                     (get-type-of-exp (type-check (multiplication-left  ast)))
                                     (get-type-of-exp (type-check (multiplication-right ast))) )))
          ((division? ast) (make-division
                            (division-left  ast)
                            (division-right ast)
                            (is-each-side-integer?
                             (get-type-of-exp (type-check (division-left  ast)))
                             (get-type-of-exp (type-check (division-right ast))) )))
          ((and~? ast) (make-and~
                            (and~-left  ast)
                            (and~-right ast)
                            (is-each-side-boolean?
                             (get-type-of-exp (type-check (and~-left  ast)))
                             (get-type-of-exp (type-check (and~-right ast))) )))
          ; ((negative-value? ast) ;DIFFERENT THAN REST)
          ; ((if~? ast) ;DIFFERENT THAN REST);add universal type because can eval to both int and boolean
          ; ((not? ast) ;DIFFERENT THAN REST)
          ((boolean~? ast) ast)
          ((number? ast)   ast)))) ;problem with name?
          ; ((nonemptyactuals? ast) #FUNCTION TO DO COMPARISON);THIS IS A PROBLEM, CHANGE DA NAME
          ; ((function-call? ast) #FUNCTION TO DO COMPARISON) )))


(define is-each-side-integer?
  (lambda (left-type right-type)
    (if (and (eq? int-type left-type ) 
             (eq? int-type right-type))
      left-type
      type-err)))

(define is-each-side-boolean?
  (lambda (left-type right-type)
    (if (and (eq? bool-type left-type)
             (eq? bool-type right-type))
      left-type
      type-err)))


(define get-type-of-exp
  (lambda (exp)
    (cond ((equals? exp)          (equals-type          exp))
          ((less-than? exp)       (less-than-type       exp))
          ((addition? exp)        (addition-type        exp))
          ((subtraction? exp)     (subtraction-type     exp))
          ((or~? exp)             (or~-type             exp))
          ((multiplication? exp)  (multiplication-type  exp))
          ((division? exp)        (division-type        exp))
          ((and~? exp)            (and~-type            exp))
          ((negative-value? exp)  (negative-value-type  exp))
          ((if~? exp)             (if~-type             exp))
          ((not? exp)             (not-type             exp))
          ((nonemptyactuals? exp) (nonemptyactuals-type exp))
          ((function-call? exp)   (function-call-type   exp))
          (else (display "ERROR WITH get-type");should never reach
                "ERROR") )))

