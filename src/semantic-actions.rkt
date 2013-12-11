#lang racket

(provide (all-defined-out))
(require "parser-extra.rkt")

(define empty-type 'void)
(define print-type 'null)
(define bool-type 'boolean)
(define int-type  'integer)
(define int/bool-type 'integer/boolean)

(define op (open-output-string))
(define get-op get-output-string)

(define pop-2 (compose pop pop  ))
(define pop-3 (compose pop pop-2))
(define pop-4 (compose pop pop-3))

(define 1st-on-stack get-top-of-stack)
(define 2nd-on-stack (compose 1st-on-stack pop  ))
(define 3rd-on-stack (compose 1st-on-stack pop-2))
(define 4th-on-stack (compose 1st-on-stack pop-3))

(define 1-off-top
  (lambda (stack)
    (list (first stack))))
(define 2-off-top
  (lambda (stack)
    (list (second stack)
          (first stack))))
(define 3-off-top
  (lambda (stack)
    (list (third stack)
          (second stack)
          (first stack))))
(define 4-off-top
  (lambda (stack)
    (list (fourth stack)
          (third stack)
          (second stack)
          (first stack))))


(define semantic-action-with
  (lambda (make pop-amt amt-off-top make-check)
    (lambda (stack value)
      (push (pop-amt stack)
            (list (apply (need-type? make make-check)
                         (amt-off-top stack)))))))

(define curry-the-make-1
  (lambda (make)
    (lambda (value1)
      (make value1 empty-type))))
(define curry-the-make-2
  (lambda (make)
    (lambda (value1 value2)
      (make value1 value2 empty-type))))
(define curry-the-make-3
  (lambda (make)
    (lambda (value1 value2 value3)
      (make value1 value2 value3 empty-type))))
(define curry-the-make-4
  (lambda (make)
    (lambda (value1 value2 value3 value4)
      (make value1 value2 value3 value4 empty-type))))

(define semantic-action-1
  (lambda (make)
    (semantic-action-with make pop 1-off-top curry-the-make-1)))
(define semantic-action-2
  (lambda (make) 
    (semantic-action-with make pop-2 2-off-top curry-the-make-2)))
(define semantic-action-3
  (lambda (make) 
    (semantic-action-with make pop-3 3-off-top curry-the-make-3)))
(define semantic-action-4
  (lambda (make) 
    (semantic-action-with make pop-4 4-off-top curry-the-make-4)))

(define semantic-no-pops
  (lambda (make)
    (lambda (stack value)
      (push stack (list ((choose-check make) value))))))
(define choose-check
  (lambda (make)
    (if (or (eq? make-boolean~ make)
            (eq? make-number make))
        make
        (need-type? make curry-the-make-1))))
(define need-type?
  (lambda (make make-check)
    (if (or (eq? make-program make)
          (eq? make-definitions make)
          (eq? make-def make)
          (eq? make-nonemptyformals make)
          (eq? make-empty-formals make)
          (eq? make-boolean~ make)
          (eq? make-number make)
          (eq? make-nonemptyactuals-prime make)
          (eq? make-empty-actuals make)
          (eq? make-formal make))
        make
        (make-check make))))
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;Semantic Actions;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;Body;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct identifier (value type))
(define make/identifier (semantic-no-pops make-identifier))

(define-struct program (definitions))
(define make/program (semantic-action-1 make-program))

(define-struct definitions (def definitions))
(define make/definitions (semantic-action-2 make-definitions))

(define-struct def (id formals type body))
(define make/def (semantic-action-4 make-def))

(define-struct empty-formals ())
(define make/empty-formals
  (lambda (stack value)
    (push stack (list (make-empty-formals)))))

(define-struct nonemptyformals (formal nonemptyformals))
(define make/nonemptyformals (semantic-action-2 make-nonemptyformals))

(define-struct formal (id type))
(define make/formal (semantic-action-2 make-formal))

(define-struct type (value type))
(define make/type (semantic-no-pops make-type))

(define-struct print~ (expr type))
(define make/print~ (semantic-action-1 make-print~))

(define-struct body (expr type))
(define make/body (semantic-action-1 make-body)) 

(define-struct print-body (print-expr expr type))
(define make/print-body (semantic-action-2 make-print-body)) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Expr;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct equals (left right type))
(define make/equals (semantic-action-2 make-equals))

(define-struct less-than (left right type))
(define make/less-than (semantic-action-2 make-less-than))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;Simple-Expr;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct addition (left right type))
(define make/addition (semantic-action-2 make-addition))

(define-struct subtraction (left right type))
(define make/subtraction (semantic-action-2 make-subtraction))

(define-struct or~ (left right type))
(define make/or (semantic-action-2 make-or~))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Term;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct multiplication (left right type))
(define make/multiplication (semantic-action-2 make-multiplication))

(define-struct division (left right type))
(define make/division (semantic-action-2 make-division))

(define-struct and~ (left right type))
(define make/and~ (semantic-action-2 make-and~))
;;;

(define-struct negative-value (value type))
(define make/negative-value (semantic-action-1 make-negative-value))

(define-struct if~ (test then else type))
(define make/if~ (semantic-action-3 make-if~))

(define-struct not (value type))
(define make/not (semantic-action-1 make-not))

(define-struct boolean~ (value (type #:auto))
#:auto-value bool-type)
(define make/boolean~ (semantic-no-pops make-boolean~))

;POTENTIAL PROBLEM WITH THIS NAME
(define-struct number (value (type #:auto))
#:auto-value int-type)
(define make/number (semantic-no-pops make-number))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;Actuals;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct empty-actuals ())
(define make/empty-actuals
  (lambda (stack value)
    (push stack (list (make-empty-actuals)))))

(define-struct nonemptyactuals (expr type))
(define make/nonemptyactuals (semantic-action-1 make-nonemptyactuals))

(define-struct nonemptyactuals-prime (expr nonemptyactuals))
(define make/nonemptyactuals-prime (semantic-action-2 make-nonemptyactuals-prime))

(define-struct function-call (name actuals type))
(define make/function-call (semantic-action-2 make-function-call))
