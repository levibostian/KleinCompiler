#lang racket
; Klein compiler
; cs4550 - Fall 2013
; 
; Team RackAttack

;(require rackunit 
;         "scanner.rkt")
(require "parser-extra.rkt")
(require rackunit
         "parse-table.rkt"
         "scanner.rkt")

(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;Parser;;;;;;;;;;;;;;;
(define parser
  (lambda (source-code-path)
    (token-reader (scanner source-code-path)) ))

(define token-reader
  (lambda (token-list)
    (token-reader-helper '() (list 'program '$) token-list '() (token-value (get-current-token token-list))) ))

(define token-reader-helper
  (lambda (parser-accum stack token-list semantic-stack previous-terminal)
    (let ((top-of-stack (get-top-of-stack stack))
          (current-token (get-current-token token-list)))
      (cond ((end? stack current-token) (get-top-of-stack semantic-stack));these parameters will be cleaned up at some point
            ((terminal? top-of-stack) 
             (terminal-action parser-accum 
                              top-of-stack 
                              stack 
                              current-token 
                              token-list 
                              semantic-stack
                              previous-terminal))
            ((non-terminal? top-of-stack)
             (non-terminal-action parser-accum 
                                  top-of-stack 
                                  stack 
                                  current-token 
                                  token-list
                                  semantic-stack
                                  previous-terminal))
            (else (semantic-action parser-accum 
                                   top-of-stack 
                                   stack 
                                   current-token 
                                   token-list
                                   semantic-stack
                                   previous-terminal))))))

(define terminal-action
  (lambda (parser-accum top-of-stack stack current-token token-list semantic-stack previous-terminal)

    (if (top=token? top-of-stack current-token)
        (token-reader-helper parser-accum (pop stack) (consume token-list) semantic-stack current-token)
        (print-error current-token stack))))

(define non-terminal-action
  (lambda (parser-accum top-of-stack stack current-token token-list semantic-stack previous-terminal)
    (let ((grammar-rule (rule-for top-of-stack (terminal-for current-token))))
      (if (transition-error? grammar-rule) 
          (print-error (get-current-token token-list) grammar-rule)
          (token-reader-helper (gather-parser-output parser-accum top-of-stack current-token)
                               (check-for-push (pop stack) grammar-rule)
                               token-list
                               semantic-stack
                               previous-terminal)))))
(define semantic-action
  (lambda (parser-accum top-of-stack stack current-token token-list semantic-stack previous-terminal)
    (token-reader-helper parser-accum
                         (pop stack)
                         token-list
                         (top-of-stack semantic-stack (token-value previous-terminal))
                         previous-terminal)))

;(parser "klein-programs/test.kln")
;(parser "klein-programs/euclid.kln")
;(parser "klein-programs/horner.kln")
;(parser "klein-programs/circular-prime.kln")
;(parser "klein-programs/farey.kln")
;(parser "klein-programs/fibonacci.kln")
;(parser "klein-programs/horner-parameterized.kln")

