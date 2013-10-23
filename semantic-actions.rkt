#lang racket


;;;Maybe changes these from list-ifying everything to have the 
;;;PUSH check if the value is a list, and turn it into one if it's not


(provide (all-defined-out))
(require "parser-extra.rkt")

(define pop-2 (compose pop pop  ))
(define pop-3 (compose pop pop-2))
(define pop-4 (compose pop pop-3))

(define 1st-on-stack get-top-of-stack)
(define 2nd-on-stack (compose 1st-on-stack
                              pop))
(define 3rd-on-stack (compose 1st-on-stack
                              pop-2))
(define 4th-on-stack (compose 1st-on-stack
                              pop-3))
(define semantic-with-1
  (lambda (stack make)
    (push (pop stack)
          (list 
           (make (1st-on-stack stack))))))
(define semantic-with-2
  (lambda (stack make)
    (push (pop-2 stack)
          (list 
           (make (2nd-on-stack stack)
                 (1st-on-stack stack))))))
(define semantic-with-3
  (lambda (stack make)
    (push (pop-3 stack)
          (list 
           (make (3rd-on-stack stack)
                 (2nd-on-stack stack)
                 (1st-on-stack stack))))))

;Semantic Actions

(define-struct identifier (value))
(define make/identifier
  (lambda (stack value)
    (push stack (list (make-identifier value)))))

(define-struct program (definitions))
(define make/program
  (lambda (stack value)
    (semantic-with-1 stack make-program)))

(define-struct definitions (def definitions))
(define make/defintions
  (lambda (stack value)
    (semantic-with-2 stack make-definitions)))

(define-struct def (id formals type body))
(define make/def
  (lambda (stack value)
    (push (pop-4 stack)
          (list 
           (make-def (4th-on-stack stack)
                     (3rd-on-stack stack)
                     (2nd-on-stack stack)
                     (1st-on-stack stack))))))

(define-struct nonemptyformals (formal nonemptyformals))
(define make/nonemptyformals
  (lambda (stack value)
    (semantic-with-2 stack make-nonemptyformals)))

(define-struct formal (id type))
(define make/formal
  (lambda (stack value)
    (semantic-with-2 stack make-formal)))

(define-struct body (expr))
(define make/body
  (lambda (stack value)
    (semantic-with-1 stack make-body)))

(define-struct print-body (print-expr expr))
(define make/print-body
  (lambda (stack value)
    (semantic-with-2 make-print-body stack)))

(define-struct type (value))
(define make/type
  (lambda (stack value)
    (push stack (list (make-type value)))))
;;;EXPR;;;
;(define-struct expressions (expr expressions))
;(define make/expressions
;  (lambda (stack value)
;    (semantic-with-2 stack make-expressions)))
(define-struct equals (left right))
(define make/equals
  (lambda (stack value)
    (semantic-with-2 stack make-equals)))

(define-struct less-than (left right))
(define make/less-than
  (lambda (stack value)
    (semantic-with-2 stack make-less-than)))
;;;
;;;SIMPLE EXPR;;;
(define-struct addition (left right))
(define make/addition
  (lambda (stack value)
    (semantic-with-2 stack make-addition)))

(define-struct subtraction (left right))
(define make/subtraction
  (lambda (stack value)
    (semantic-with-2 stack make-addition)))

(define-struct or (left right))
(define make/or
  (lambda (stack value)
    (semantic-with-2 stack make-or)))
;;;
;;;TERM;;;
(define-struct multiplication (left right))
(define make/multiplication
  (lambda (stack value)
    (semantic-with-2 stack make-multiplication)))

(define-struct division (left right))
(define make/division
  (lambda (stack value)
    (semantic-with-2 stack make-division)))

(define-struct and~ (left right))
(define make/and~
  (lambda (stack value)
    (semantic-with-2 stack make-and~)))
;;;

(define-struct negative-value (value))
(define make/negative-value
  (lambda (stack value)
    (semantic-with-1 stack make-negative-value)))

(define-struct if~ (test then else))
(define make/if~
  (lambda (stack value)
    (semantic-with-3 stack make-if~)))

(define-struct not (value))
(define make/not
  (lambda (stack value)
    (semantic-with-1 stack make-not)))

(define-struct boolean~ (value))
(define make/boolean~
  (lambda (stack value)
    (push stack (list (make-boolean~ value)))))

(define-struct number (value))
(define make/number
  (lambda (stack value)
    (push stack (list (make-number value)))))

;;;
