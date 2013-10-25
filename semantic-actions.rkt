#lang racket

(provide (all-defined-out))
(require "parser-extra.rkt")

(define pop-2 (compose pop pop  ))
(define pop-3 (compose pop pop-2))
(define pop-4 (compose pop pop-3))

(define 1st-on-stack get-top-of-stack)
(define 2nd-on-stack (compose 1st-on-stack pop  ))
(define 3rd-on-stack (compose 1st-on-stack pop-2))
(define 4th-on-stack (compose 1st-on-stack pop-3))

(define 1-off-top
  (lambda (stack)
    (list (1st-on-stack stack))))
(define 2-off-top
  (lambda (stack)
    (list (2nd-on-stack stack)
          (1st-on-stack stack))))
(define 3-off-top
  (lambda (stack)
    (list (3rd-on-stack stack)
          (2nd-on-stack stack)
          (1st-on-stack stack))))
(define 4-off-top
  (lambda (stack)
    (list (4th-on-stack stack)
          (3rd-on-stack stack)
          (2nd-on-stack stack)
          (1st-on-stack stack))))

(define semantic-action
  (lambda (make pop-amt amt-off-top)
    (lambda (stack value)
      (push (pop-amt stack)
            (list (apply make 
                         (amt-off-top stack)))))))
(define semantic-action-1
  (lambda (make)
    (semantic-action make pop 1-off-top)))
(define semantic-action-2
  (lambda (make) 
    (semantic-action make pop-2 2-off-top)))
(define semantic-action-3
  (lambda (make) 
    (semantic-action make pop-3 3-off-top)))
(define semantic-action-4
  (lambda (make) 
    (semantic-action make pop-4 4-off-top)))
(define semantic-no-pops
  (lambda (make)
    (lambda (stack value)
      (push stack (list (make value))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Semantic Actions;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;Body;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct identifier (value))
(define make/identifier (semantic-no-pops make-identifier))

(define-struct program (definitions))
(define make/program (semantic-action-1 make-program))

(define-struct definitions (def definitions))
(define make/definitions (semantic-action-2 make-definitions))

(define-struct def (id formals type body))
(define make/def (semantic-action-4 make-def))

(define-struct empty-formals ())
(define make/empty-formals (semantic-no-pops make-empty-formals))

(define-struct nonemptyformals (formal nonemptyformals))
(define make/nonemptyformals (semantic-action-2 make-nonemptyformals))

(define-struct formal (id type))
(define make/formal (semantic-action-2 make-formal))

(define-struct type (value))
(define make/type (semantic-no-pops make-type))

(define-struct print~ (expr))
(define make/print~ (semantic-action-1 make-print~)) 

(define-struct body (expr))
(define make/body (semantic-action-1 make-body)) 

(define-struct print-body (print-expr expr))
(define make/print-body (semantic-action-2 make-print-body)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Expr;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct equals (left right))
(define make/equals (semantic-action-2 make-equals))

(define-struct less-than (left right))
(define make/less-than (semantic-action-2 make-less-than))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;Simple-Expr;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct addition (left right))
(define make/addition (semantic-action-2 make-addition))

(define-struct subtraction (left right))
(define make/subtraction (semantic-action-2 make-subtraction))

(define-struct or~ (left right))
(define make/or (semantic-action-2 make-or~))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Term;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct multiplication (left right))
(define make/multiplication (semantic-action-2 make-multiplication))

(define-struct division (left right))
(define make/division (semantic-action-2 make-division))

(define-struct and~ (left right))
(define make/and~ (semantic-action-2 make-and~))
;;;

(define-struct negative-value (value))
(define make/negative-value (semantic-action-1 make-negative-value))

(define-struct if~ (test then else))
(define make/if~ (semantic-action-3 make-if~))

(define-struct not (value))
(define make/not (semantic-action-1 make-not))

(define-struct boolean~ (value))
(define make/boolean~ (semantic-no-pops make-boolean~))

(define-struct number (value))
(define make/number (semantic-no-pops make-number))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;Actuals;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct empty-actuals ())
(define make/empty-actuals (semantic-no-pops make-empty-actuals))

(define-struct nonemptyactuals (expr))
(define make/nonemptyactuals (semantic-action-1 make-nonemptyactuals))

(define-struct nonemptyactuals-prime (expr nonemptyactuals))
(define make/nonemptyactuals-prime (semantic-action-2 make-nonemptyactuals-prime))

(define-struct function-call (name actuals))
(define make/function-call (semantic-action-2 make-function-call))
