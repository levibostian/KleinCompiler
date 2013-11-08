#lang racket

;sorry this isn't pretty. It will be by the next submission!


(provide (all-defined-out))
(require "semantic-actions.rkt"
         "symbol-table-helpers.rkt"
         "symbol-table.rkt"
         "print-parser.rkt"
         "parser.rkt")

(define type-err 'SEMANTIC-ERROR)
(define function-not-exist 'invalid-function)

;(define symbol-table-output (symbol-table (parser "klein-programs/test.kln")));REMEMBER TO CHANGE
;(define abstract-st (parser "klein-programs/test.kln"))

(define is-each-side-integer?
  (lambda (left-type right-type return-type)
    (if (and (eq? int-type left-type ) 
             (eq? int-type right-type))
        return-type
        type-err)))

(define is-each-side-boolean?
  (lambda (left-type right-type return-type)
    (if (and (eq? bool-type left-type)
             (eq? bool-type right-type))
        return-type
        type-err)))

(define type-check
  (lambda (ast)
    (type-check-helper ast 'nothing (symbol-table ast))))

(define type-check-helper
  (lambda (ast current-function symbol-table)
    (cond ((identifier? ast)(make-identifier 
                             (identifier-value ast)
                             (get-id-type-for-def (identifier-value ast)
                                                  current-function
                                                  symbol-table
                                                  type-err)))
          ((program? ast)        (type-check-program        ast current-function symbol-table))
          ((definitions? ast)    (type-check-definitions    ast current-function symbol-table))
          ((def? ast)            (type-check-def            ast current-function symbol-table))
          ((body? ast)           (type-check-body           ast current-function symbol-table))
          ((print~? ast)         (type-check-print          ast current-function symbol-table))
          ((print-body? ast)     (type-check-print-body     ast current-function symbol-table))
          ((equals? ast)         (type-check-equals         ast current-function symbol-table))
          ((less-than? ast)      (type-check-less-than      ast current-function symbol-table))
          ((addition? ast)       (type-check-addition       ast current-function symbol-table))
          ((subtraction? ast)    (type-check-subtraction    ast current-function symbol-table))
          ((or~? ast)            (type-check-or             ast current-function symbol-table))
          ((multiplication? ast) (type-check-multiplication ast current-function symbol-table))
          ((division? ast)       (type-check-division       ast current-function symbol-table))
          ((and~? ast)           (type-check-and            ast current-function symbol-table))
          ((negative-value? ast) (type-check-negative-value ast current-function symbol-table))
          ((if~? ast)            (type-check-if             ast current-function symbol-table))
          ((not? ast)            (type-check-not            ast current-function symbol-table))
          ((boolean~? ast)       ast)
          ((number? ast)         ast) ;problem with name?
          ((or (nonemptyactuals-prime? ast)
               (nonemptyactuals? ast)
               (empty-actuals? ast)) (checks-type-by-formal-pos ast current-function 0 symbol-table type-err))
          ((function-call? ast) (type-check-function-call ast current-function symbol-table)) )))


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
          ((identifier? exp)      (identifier-type      exp))
          (else (display "ERROR WITH get-type")
                (display exp);should never reach
                "ERROR") )))

(define type-check-program
  (lambda (program current-function symbol-table)
    (make-program 
      (type-check-helper (program-definitions program) current-function symbol-table))))

(define type-check-definitions
  (lambda (defins current-function symbol-table)
    (make-definitions 
     (type-check-helper (definitions-def defins) current-function symbol-table)
     (type-check-helper (definitions-definitions defins) current-function symbol-table))))

(define type-check-def
  (lambda (defin current-function symbol-table)
    (let ((type-checked-def-body (type-check-helper (def-body defin) (identifier-value (def-id defin)) symbol-table)))
      (make-def
       (def-id defin)
       (def-formals defin)
       (def-type defin);this will have to be checked
       type-checked-def-body))))

(define type-check-body
  (lambda (bod current-function symbol-table)
    (let ((body-type (get-type-of-exp (type-check-helper (body-expr bod) current-function symbol-table) )))
      (make-body (type-check-helper (body-expr bod) current-function symbol-table)
                 (if (eq? (get-function-type current-function symbol-table type-err)
                          body-type)
                     body-type
                     type-err)))))

(define type-check-print
  (lambda (print current-function symbol-table)
    (make-print~ (type-check-helper (print~-expr print) current-function symbol-table)
                 print-type)))

(define type-check-print-body
  (lambda (print-bod current-function symbol-table)
    (let ((type-checked-print-body-print-expr (type-check-helper (print-body-print-expr print-bod) current-function symbol-table))
          (type-checked-print-body-expr (type-check-helper (print-body-expr print-bod) current-function symbol-table)))
      (make-print-body
       type-checked-print-body-print-expr
       type-checked-print-body-expr
       (get-type-of-exp type-checked-print-body-expr)))));CHECK DIS FOR TYPE/ ADD TYPE

(define type-check-binary-exp
  (lambda (make exp left right side-check result-type current-function symbol-table)
    (let ((type-checked-left  (type-check-helper (left  exp) current-function symbol-table))
          (type-checked-right (type-check-helper (right exp) current-function symbol-table)))
      (make
       type-checked-left
       type-checked-right
       (side-check
        (get-type-of-exp type-checked-left )
        (get-type-of-exp type-checked-right)
        result-type)))))
     

(define type-check-equals
  (lambda (eq current-function symbol-table)
    (type-check-binary-exp make-equals 
                           eq 
                           equals-left
                           equals-right
                           is-each-side-integer?
                           bool-type
                           current-function
                           symbol-table)))

(define type-check-less-than
  (lambda (less current-function symbol-table)
    (type-check-binary-exp make-less-than
                           less
                           less-than-left
                           less-than-right
                           is-each-side-integer?
                           bool-type
                           current-function
                           symbol-table)))

(define type-check-addition
  (lambda (add current-function symbol-table)
    (type-check-binary-exp make-addition
                           add
                           addition-left
                           addition-right
                           is-each-side-integer?
                           int-type
                           current-function
                           symbol-table)))

(define type-check-subtraction
  (lambda (sub current-function symbol-table)
    (type-check-binary-exp make-subtraction
                           sub
                           subtraction-left
                           subtraction-right
                           is-each-side-integer?
                           int-type
                           current-function
                           symbol-table)))

(define type-check-or
  (lambda (or-exp current-function symbol-table)
    (type-check-binary-exp make-or~
                           or-exp
                           or~-left
                           or~-right
                           is-each-side-boolean?
                           bool-type
                           current-function
                           symbol-table)))

(define type-check-multiplication
  (lambda (multi current-function symbol-table)
    (type-check-binary-exp make-multiplication
                           multi
                           multiplication-left
                           multiplication-right
                           is-each-side-integer?
                           int-type
                           current-function
                           symbol-table)))

(define type-check-division
  (lambda (div current-function symbol-table)
    (type-check-binary-exp make-division
                           div
                           division-left
                           division-right
                           is-each-side-integer?
                           int-type
                           current-function
                           symbol-table)))

(define type-check-and
  (lambda (& current-function symbol-table)
    (type-check-binary-exp make-and~
                           &
                           and~-left
                           and~-right
                           is-each-side-boolean?
                           bool-type
                           current-function
                           symbol-table)))

(define type-check-negative-value
  (lambda (neg-val current-function symbol-table)
    (let ((type-checked-exp (type-check-helper (negative-value-value neg-val) current-function symbol-table)))
      (make-negative-value type-checked-exp
                           (if (eq? (get-type-of-exp type-checked-exp) int-type)
                               int-type
                               type-err)))))
    
(define type-check-if
  (lambda (if-exp current-function symbol-table)
    (let ((type-checked-test (type-check-helper (if~-test if-exp) current-function symbol-table))
          (type-checked-then (type-check-helper(if~-then if-exp) current-function symbol-table))
          (type-checked-else (type-check-helper (if~-else if-exp) current-function symbol-table)))
      (make-if~ type-checked-test
                type-checked-then
                type-checked-else
                (let ((test-type (get-type-of-exp type-checked-test))
                      (then-type (get-type-of-exp type-checked-then))
                      (else-type (get-type-of-exp type-checked-else)))
                  (if (eq? test-type bool-type)
                      (if (eq? then-type else-type)
                          then-type
                          (if (or (eq? then-type type-err)
                                  (eq? else-type type-err))
                              type-err
                              int/bool-type))
                      type-err))))))

(define type-check-not
  (lambda (not~ current-function symbol-table)
    (let ((type-checked-exp (type-check-helper (not-value not~) current-function symbol-table)))
      (make-not type-checked-exp
                (if (eq? (get-type-of-exp type-checked-exp) bool-type)
                    bool-type
                    type-err)))))

(define type-check-function-call
  (lambda (f-c current-function symbol-table)
    (let ((function-name (function-call-name f-c)))
      (make-function-call (function-call-name f-c);NEED TO MAKE SURE VALID FUNCTION
                          (checks-type-by-formal-pos (function-call-actuals f-c) (identifier-value function-name) 0 symbol-table type-err)
                          (get-function-type (identifier-value function-name) symbol-table type-err) ))))

(define checks-type-by-formal-pos
  (lambda (actuals current-function pos sym-table error-type)
    (cond ((nonemptyactuals? actuals)
           (let ((type-checked-expr (type-check-helper (nonemptyactuals-expr actuals) current-function sym-table))
                 (looked-up-type (get-formal-by-pos current-function sym-table pos error-type)))
             (make-nonemptyactuals type-checked-expr
                                   (if (eq? (get-type-of-exp type-checked-expr) looked-up-type)
                                       looked-up-type
                                       error-type))))
          ((nonemptyactuals-prime? actuals)
           (make-nonemptyactuals-prime (checks-type-by-formal-pos (nonemptyactuals-prime-expr actuals)
                                                                  current-function
                                                                  pos
                                                                  sym-table
                                                                  error-type)
                                       (checks-type-by-formal-pos (nonemptyactuals-prime-nonemptyactuals actuals)
                                                                  current-function
                                                                  (+ 1 pos)
                                                                  sym-table
                                                                  error-type)))
          ((empty-actuals? actuals) actuals)
          (else (type-check-helper actuals current-function sym-table)))))