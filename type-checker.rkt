#lang racket

(provide (all-defined-out))
(require "semantic-actions.rkt"
         "symbol-table.rkt"
         "parser.rkt")


(define type-err (list 'SEMANTIC 'ERROR))

(define symbol-table-output (symbol-table (parser "klein-programs/test.kln")));REMEMBER TO CHANGE
(define abstract-st (parser "klein-programs/test.kln"))

(define type-check
  (lambda (ast)
    (type-check-helper ast 'nothing (symbol-table ast))))

(define type-check-helper
  (lambda (ast current-function symbol-table)
    (cond ((identifier? ast) (make-identifier 
                               (identifier-value ast)
                               (get-id-type-for-def (identifier-value ast)
                                                    current-function
                                                    symbol-table
                                                    type-err)))
          ((program? ast) (make-program 
                            (type-check-helper (program-definitions ast) current-function symbol-table)))
          ((definitions? ast) (make-definitions 
                                (type-check-helper (definitions-def ast) current-function symbol-table)
                                (type-check-helper (definitions-definitions ast) current-function symbol-table)))
          ((def? ast) (make-def
                        (def-id ast)
                        (def-formals ast)
                        (def-type ast);this will have to be checked
                        (type-check-helper (def-body ast) (def-id ast) symbol-table)))
          ((body? ast) (make-body (type-check-helper (body-expr ast) current-function symbol-table)
                                  (get-type-of-exp (type-check-helper (body-expr ast) current-function symbol-table) )))
          ((print-body? ast) (make-print-body
                               (type-check-helper (print-body-print-expr ast) current-function symbol-table)
                               (type-check-helper (print-body-expr ast) current-function symbol-table)));CHECK DIS FOR TYPE/ ADD TYPE
          ((equals? ast)  (make-equals 
                            (equals-left ast)
                            (equals-right ast)
                            (is-each-side-integer?
                             (get-type-of-exp (type-check-helper (equals-left ast) current-function symbol-table))
                             (get-type-of-exp (type-check-helper (equals-right ast) current-function symbol-table)) )))
          ((less-than? ast) (make-less-than 
                            (less-than-left  ast)
                            (less-than-right ast)
                            (is-each-side-integer?
                             (get-type-of-exp (type-check-helper (less-than-left  ast) current-function symbol-table))
                             (get-type-of-exp (type-check-helper (less-than-right ast) current-function symbol-table)) )))
          ((addition? ast) (make-addition
                            (addition-left  ast)
                            (addition-right ast)
                            (is-each-side-integer?
                             (get-type-of-exp (type-check-helper (addition-left  ast) current-function symbol-table))
                             (get-type-of-exp (type-check-helper (addition-right ast) current-function symbol-table)) )))
          ((subtraction? ast) (make-subtraction
                                (subtraction-left  ast)
                                (subtraction-right ast)
                                (is-each-side-integer?
                                 (get-type-of-exp (type-check-helper (subtraction-left  ast) current-function symbol-table))
                                 (get-type-of-exp (type-check-helper (subtraction-right ast) current-function symbol-table)) )))
          ((or~? ast) (make-or~
                        (or~-left  ast)
                        (or~-right ast)
                        (is-each-side-boolean?
                         (get-type-of-exp (type-check-helper (or~-left  ast) current-function symbol-table))
                         (get-type-of-exp (type-check-helper (or~-right ast) current-function symbol-table)) )))
          ((multiplication? ast) (make-multiplication
                                   (multiplication-left  ast)
                                   (multiplication-right ast)
                                   (is-each-side-integer?
                                     (get-type-of-exp (type-check-helper (multiplication-left  ast) current-function symbol-table))
                                     (get-type-of-exp (type-check-helper (multiplication-right ast) current-function symbol-table)) )))
          ((division? ast) (make-division
                            (division-left  ast)
                            (division-right ast)
                            (is-each-side-integer?
                             (get-type-of-exp (type-check-helper (division-left  ast) current-function symbol-table))
                             (get-type-of-exp (type-check-helper (division-right ast) current-function symbol-table)) )))
          ((and~? ast) (make-and~
                            (and~-left  ast)
                            (and~-right ast)
                            (is-each-side-boolean?
                             (get-type-of-exp (type-check-helper (and~-left  ast) current-function symbol-table))
                             (get-type-of-exp (type-check-helper (and~-right ast) current-function symbol-table)) )))
          ((negative-value? ast) (make-negative-value (negative-value-value ast);untested, run test program with pretty printer to make sure works.
                                                      (get-type-of-exp (type-check-helper (negative-value-value ast) current-function symbol-table))))
          ((if~? ast) (make-if~ (if~-test ast);untested, run test program with pretty printer to make sure works.
                                (if~-then ast)
                                (if~-else ast)
                                (let ((then-type (get-type-of-exp (type-check-helper (if~-then ast) current-function symbol-table)))
                                      (else-type (get-type-of-exp (type-check-helper (if~-else ast) current-function symbol-table))))
                                     (if (and (eq? (get-type-of-exp (type-check-helper (if~-test ast) current-function symbol-table)) bool-type)
                                            (and (or (eq? then-type bool-type) (eq? then-type int-type))
                                                 (or (eq? else-type bool-type) (eq? else-type int-type))))
                                         (if (eq? then-type else-type)
                                             then-type
                                             int/bool-type)
                                         type-err ))))

          ((not? ast) (make-not (not-value ast);untested, run test program with pretty printer to make sure works.
                                (get-type-of-exp (type-check-helper (not-value ast) current-function symbol-table))))
          ((boolean~? ast) ast)
          ((number? ast)   ast)))) ;problem with name?
          ;((nonemptyactuals? ast) #FUNCTION TO DO COMPARISON);THIS IS A PROBLEM, CHANGE DA NAME
          ;((nonemptyactuals-prime? ast) ;do something)
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
          ((number? exp)          (number-type          exp))
          ((boolean~? exp)        (boolean~-type        exp))
          (else (display "ERROR WITH get-type");should never reach
                "ERROR") )))

