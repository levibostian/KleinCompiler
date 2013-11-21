#lang racket

;sorry this isn't pretty. It will be by the next submission!


(provide (all-defined-out))
(require "semantic-actions.rkt"
         "symbol-table-helpers.rkt"
         "symbol-table.rkt"
         "print-parser.rkt"
         "parser.rkt")

(define type-err 'TYPE-ERROR)

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

(define display-errors
  (lambda (type error-message)
    (if (eq? type type-err)
        (display-error error-message)
        (display ""))))
(define display-error
  (lambda (error-message)
    (display error-message)
    (newline)))

(define semantic-analysis
  (lambda (ast)
    (let ((sym-table (symbol-table ast)))
      (if (no-user-defined-print? sym-table)
          (type-check-helper ast 'nothing (symbol-table ast))
          "Error: cannot overwrite print"))))
              
      

(define type-check-helper
  (lambda (ast current-function symbol-table)
    (cond ((identifier? ast)
           (let ((ident-type (get-id-type-for-def (identifier-value ast)
                                                  current-function
                                                  symbol-table
                                                  type-err))
                 (ident-val (identifier-value ast)))
             (display-errors ident-type (list ident-val 'in current-function 'referenced 'before 'assignment))
             (make-identifier 
              (identifier-value ast)
              ident-type)))
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
          ((print~? exp)          (print~-type          exp))
          ((print-body? exp)      (print-body-type      exp))
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
    (let ((type-checked-body (type-check-helper (body-expr bod) current-function symbol-table)))
      (display-errors (get-type-of-exp type-checked-body) 
                      (list 'body 'in current-function 'incompatible 'type 'with 'function 'return 'type))
      (make-body type-checked-body
                 (if (eq? (get-function-type current-function symbol-table type-err)
                          (get-type-of-exp type-checked-body))
                          (get-type-of-exp type-checked-body)
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
       (get-type-of-exp type-checked-print-body-expr)))))

(define type-check-binary-exp
  (lambda (make exp left right side-check result-type current-function symbol-table exp-symbol)
    (let ((type-checked-left  (type-check-helper (left  exp) current-function symbol-table))
          (type-checked-right (type-check-helper (right exp) current-function symbol-table)))
      (let ((check-sides-result (side-check
                               (get-type-of-exp type-checked-left )
                               (get-type-of-exp type-checked-right)
                               result-type)))
        (display-errors check-sides-result
                        (list 'expression: left exp-symbol right 'in current-function 'incompatible 'types))         
        (make type-checked-left type-checked-right check-sides-result)))))
     

(define type-check-equals
  (lambda (eq current-function symbol-table)
    (type-check-binary-exp make-equals 
                           eq 
                           equals-left
                           equals-right
                           is-each-side-integer?
                           bool-type
                           current-function
                           symbol-table
                           '=)))

(define type-check-less-than
  (lambda (less current-function symbol-table)
    (type-check-binary-exp make-less-than
                           less
                           less-than-left
                           less-than-right
                           is-each-side-integer?
                           bool-type
                           current-function
                           symbol-table
                           '<)))

(define type-check-addition
  (lambda (add current-function symbol-table)
    (type-check-binary-exp make-addition
                           add
                           addition-left
                           addition-right
                           is-each-side-integer?
                           int-type
                           current-function
                           symbol-table
                           '+)))

(define type-check-subtraction
  (lambda (sub current-function symbol-table)
    (type-check-binary-exp make-subtraction
                           sub
                           subtraction-left
                           subtraction-right
                           is-each-side-integer?
                           int-type
                           current-function
                           symbol-table
                           '-)))

(define type-check-or
  (lambda (or-exp current-function symbol-table)
    (type-check-binary-exp make-or~
                           or-exp
                           or~-left
                           or~-right
                           is-each-side-boolean?
                           bool-type
                           current-function
                           symbol-table
                           'or)))

(define type-check-multiplication
  (lambda (multi current-function symbol-table)
    (type-check-binary-exp make-multiplication
                           multi
                           multiplication-left
                           multiplication-right
                           is-each-side-integer?
                           int-type
                           current-function
                           symbol-table
                           '*)))

(define type-check-division
  (lambda (div current-function symbol-table)
    (type-check-binary-exp make-division
                           div
                           division-left
                           division-right
                           is-each-side-integer?
                           int-type
                           current-function
                           symbol-table
                           '/)))

(define type-check-and
  (lambda (& current-function symbol-table)
    (type-check-binary-exp make-and~
                           &
                           and~-left
                           and~-right
                           is-each-side-boolean?
                           bool-type
                           current-function
                           symbol-table
                           'and)))

(define type-check-negative-value
  (lambda (neg-val current-function symbol-table)
    (let ((type-checked-exp (type-check-helper (negative-value-value neg-val) current-function symbol-table)))
      (display-errors (get-type-of-exp type-checked-exp) 
                      (list '- type-checked-exp 'in current-function 'must 'be 'integer))
      (make-negative-value type-checked-exp
                           (if (eq? (get-type-of-exp type-checked-exp) int-type)
                               int-type
                               type-err)))))
    
(define type-check-if
  (lambda (if-exp current-function symbol-table)
    (let ((type-checked-test (type-check-helper (if~-test if-exp) current-function symbol-table))
          (type-checked-then (type-check-helper(if~-then if-exp) current-function symbol-table))
          (type-checked-else (type-check-helper (if~-else if-exp) current-function symbol-table)))
      (let ((test-type (get-type-of-exp type-checked-test))
            (then-type (get-type-of-exp type-checked-then))
            (else-type (get-type-of-exp type-checked-else)))
        (display-errors test-type 
                        (list 'if 'test: type-checked-test '_in current-function 'must 'be 'boolean))
        (display-errors then-type 
                        (list 'if 'then: type-checked-then '_in current-function 'type 'error))
        (display-errors else-type
                        (list 'if 'then: type-checked-else '_in current-function 'type 'error))
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
                        type-err)))))))

(define type-check-not
  (lambda (not~ current-function symbol-table)
    (let ((type-checked-exp (type-check-helper (not-value not~) current-function symbol-table)))
      (display-errors (get-type-of-exp type-checked-exp) 
                      (list 'not type-checked-exp 'in current-function 'must 'be 'integer))
      (make-not type-checked-exp
                (if (eq? (get-type-of-exp type-checked-exp) bool-type)
                    bool-type
                    type-err)))))

(define type-check-function-call
  (lambda (f-c current-function symbol-table)
    (let ((function-name (function-call-name f-c)))
      (let ((function-type (get-function-type (identifier-value function-name) symbol-table type-err)))
        (display-errors function-type
                        (list 'function 'call 'in current-function 'does 'not 'exist))
        (make-function-call (function-call-name f-c)
                            (checks-type-by-formal-pos (function-call-actuals f-c) (identifier-value function-name) current-function 0 symbol-table type-err)
                            (get-function-type (identifier-value function-name) symbol-table type-err) )))))

(define checks-type-by-formal-pos
  (lambda (actuals function-caller current-function pos sym-table error-type)
    (cond ((nonemptyactuals? actuals)
           (let ((type-checked-expr (type-check-helper (nonemptyactuals-expr actuals) current-function sym-table))
                 (looked-up-type (get-formal-by-pos function-caller sym-table pos error-type)))
             (make-nonemptyactuals type-checked-expr
                                   (if (eq? (get-type-of-exp type-checked-expr) looked-up-type)
                                       looked-up-type
                                       error-type))))
          ((nonemptyactuals-prime? actuals)
           (make-nonemptyactuals-prime (checks-type-by-formal-pos (nonemptyactuals-prime-expr actuals)
                                                                  function-caller
                                                                  current-function
                                                                  pos
                                                                  sym-table
                                                                  error-type)
                                       (checks-type-by-formal-pos (nonemptyactuals-prime-nonemptyactuals actuals)
                                                                  function-caller
                                                                  current-function
                                                                  (+ 1 pos)
                                                                  sym-table
                                                                  error-type)))
          ((empty-actuals? actuals) actuals)
          (else (type-check-helper actuals current-function sym-table)))))