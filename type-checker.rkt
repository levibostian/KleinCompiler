#lang racket

(provide (all-defined-out))
(require "semantic-actions.rkt"
         "symbol-table-helpers.rkt"
         "symbol-table.rkt"
         "print-parser.rkt"
         "parser.rkt")


(define type-err 'SEMANTICERROR)

;(define symbol-table-output (symbol-table (parser "klein-programs/test.kln")));REMEMBER TO CHANGE
;(define abstract-st (parser "klein-programs/test.kln"))

;MAKE SURE TO PASS IN SYMBOL-TABLE TO TYPE-CHECKER!

(define type-check
  (lambda (ast symbol-table)
    (type-check-helper ast 'nothing symbol-table)))

(define type-check-helper
  (lambda (ast current-function symbol-table)
    (cond ((identifier? ast)(make-identifier 
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
                       (type-check-helper (def-body ast) (identifier-value (def-id ast)) symbol-table)))
          ((body? ast) (let ((body-type (get-type-of-exp (type-check-helper (body-expr ast) current-function symbol-table) )))
                         (make-body (type-check-helper (body-expr ast) current-function symbol-table)
                                    (if (eq? (get-function-type current-function symbol-table type-err)
                                             body-type)
                                        body-type
                                        type-err))))
          ((print~? ast) (make-print~ (type-check-helper (print~-expr ast) current-function symbol-table)
                                      print-type))
          
          ((print-body? ast) (make-print-body
                              (type-check-helper (print-body-print-expr ast) current-function symbol-table)
                              (type-check-helper (print-body-expr ast) current-function symbol-table)
                              (get-type-of-exp (type-check-helper (print-body-expr ast) current-function symbol-table))));CHECK DIS FOR TYPE/ ADD TYPE
          ((equals? ast)  (make-equals 
                           (type-check-helper (equals-left ast) current-function symbol-table)
                           (type-check-helper (equals-right ast) current-function symbol-table)
                           (is-each-side-integer?
                            (get-type-of-exp (type-check-helper (equals-left ast) current-function symbol-table))
                            (get-type-of-exp (type-check-helper (equals-right ast) current-function symbol-table)) )))
          ((less-than? ast) (make-less-than 
                             (type-check-helper (less-than-left  ast) current-function symbol-table)
                             (type-check-helper (less-than-right ast) current-function symbol-table)
                             (is-each-side-integer?
                              (get-type-of-exp (type-check-helper (less-than-left  ast) current-function symbol-table))
                              (get-type-of-exp (type-check-helper (less-than-right ast) current-function symbol-table)) )))
          ((addition? ast) (make-addition
                            (type-check-helper (addition-left  ast) current-function symbol-table)
                            (type-check-helper (addition-right ast) current-function symbol-table)
                            (is-each-side-integer?
                             (get-type-of-exp (type-check-helper (addition-left  ast) current-function symbol-table))
                             (get-type-of-exp (type-check-helper (addition-right ast) current-function symbol-table)) )))
          ((subtraction? ast) (make-subtraction
                               (type-check-helper (subtraction-left  ast) current-function symbol-table)
                               (type-check-helper (subtraction-right ast) current-function symbol-table)
                               (is-each-side-integer?
                                (get-type-of-exp (type-check-helper (subtraction-left  ast) current-function symbol-table))
                                (get-type-of-exp (type-check-helper (subtraction-right ast) current-function symbol-table)) )))
          ((or~? ast) (make-or~
                       (type-check-helper (or~-left  ast) current-function symbol-table)
                       (type-check-helper (or~-right ast) current-function symbol-table)
                       (is-each-side-boolean?
                        (get-type-of-exp (type-check-helper (or~-left  ast) current-function symbol-table))
                        (get-type-of-exp (type-check-helper (or~-right ast) current-function symbol-table)) )))
          ((multiplication? ast) (make-multiplication
                                  (type-check-helper (multiplication-left  ast) current-function symbol-table)
                                  (type-check-helper (multiplication-right ast) current-function symbol-table)
                                  (is-each-side-integer?
                                   (get-type-of-exp (type-check-helper (multiplication-left  ast) current-function symbol-table))
                                   (get-type-of-exp (type-check-helper (multiplication-right ast) current-function symbol-table)) )))
          ((division? ast) (make-division
                            (type-check-helper (division-left  ast) current-function symbol-table)
                            (type-check-helper (division-right ast) current-function symbol-table)
                            (is-each-side-integer?
                             (get-type-of-exp (type-check-helper (division-left  ast) current-function symbol-table))
                             (get-type-of-exp (type-check-helper (division-right ast) current-function symbol-table)) )))
          ((and~? ast) (make-and~
                        (type-check-helper (and~-left  ast) current-function symbol-table)
                        (type-check-helper (and~-right ast) current-function symbol-table)
                        (is-each-side-boolean?
                         (get-type-of-exp (type-check-helper (and~-left  ast) current-function symbol-table))
                         (get-type-of-exp (type-check-helper (and~-right ast) current-function symbol-table)) )))
          ((negative-value? ast) (make-negative-value (type-check-helper (negative-value-value ast) current-function symbol-table);untested, run test program with pretty printer to make sure works.
                                                      (get-type-of-exp (type-check-helper (negative-value-value ast) current-function symbol-table))))
          ((if~? ast) (make-if~ (type-check-helper (if~-test ast) current-function symbol-table);untested, run test program with pretty printer to make sure works.
                                (type-check-helper(if~-then ast) current-function symbol-table)
                                (type-check-helper (if~-else ast) current-function symbol-table)
                                (let ((then-type (get-type-of-exp (type-check-helper (if~-then ast) current-function symbol-table)))
                                      (else-type (get-type-of-exp (type-check-helper (if~-else ast) current-function symbol-table))))
                                  (if (and (eq? (get-type-of-exp (type-check-helper (if~-test ast) current-function symbol-table)) bool-type)
                                           (and (or (eq? then-type bool-type) (eq? then-type int-type))
                                                (or (eq? else-type bool-type) (eq? else-type int-type))))
                                      (if (eq? then-type else-type)
                                          then-type
                                          int/bool-type)
                                      type-err ))))
          
          ((not? ast) (make-not (type-check-helper (not-value ast) current-function symbol-table);untested, run test program with pretty printer to make sure works.
                                (get-type-of-exp (type-check-helper (not-value ast) current-function symbol-table))))
          ((boolean~? ast) ast)
          ((number? ast)   ast) ;problem with name?
          ((or (nonemptyactuals-prime? ast);potentially eliminate redudant checks for actuals
               (nonemptyactuals? ast)
               (empty-actuals? ast)) (checks-type-by-formal-pos ast current-function 0 symbol-table type-err))
          ((function-call? ast) (make-function-call (function-call-name ast)
                                                    (type-check-helper (function-call-actuals ast) current-function symbol-table)
                                                    (get-function-type (identifier-value (function-call-name ast)) symbol-table type-err))) )))

(define checks-type-by-formal-pos
  (lambda (actuals current-function pos sym-table error-type)
    (cond ((nonemptyactuals? actuals) (let ((type-checked-expr (type-check-helper (nonemptyactuals-expr actuals) current-function sym-table))
                                            (looked-up-type (get-formal-by-pos current-function sym-table pos error-type)))
                                        (make-nonemptyactuals type-checked-expr
                                                              (if (eq? type-checked-expr looked-up-type)
                                                                  looked-up-type
                                                                  error-type))))
          ((nonemptyactuals-prime? actuals) (make-nonemptyactuals-prime (type-check-helper (nonemptyactuals-prime-expr actuals)
                                                                                           current-function
                                                                                           sym-table)
                                                                        (checks-type-by-formal-pos (nonemptyactuals-prime-nonemptyactuals actuals)
                                                                                                   current-function
                                                                                                   (+ 1 pos)
                                                                                                   sym-table
                                                                                                   error-type)))
          ((empty-actuals? actuals) actuals)
          (else (type-check-helper actuals current-function sym-table)))))



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
          ((body? exp)            (body-type            exp))
          ((identifier? exp)      (identifier-type      exp));added to remove error with program
          (else (display "ERROR WITH get-type")
                (display exp);should never reach
                "ERROR") )))
