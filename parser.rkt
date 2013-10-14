#lang racket
; Klein compiler
; cs4550 - Fall 2013
; 
; Team RackAttack

(require "scanner.rkt")
(require "parse-table.rkt")

(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;Parser;;;;;;;;;;;;;;;
(define parser
  (lambda (source-code-path)
    (token-reader (scanner source-code-path)) ))

(define token-reader
  (lambda (token-list)
    (token-reader-helper '() (list 'program '$) token-list) ))

(define token-reader-helper
  (lambda (parser-accum stack token-list)
    (let ((top-of-stack (get-top-of-stack stack))
          (current-token (get-current-token token-list)))
      (cond ((end? stack current-token) #t);changed back to true, so we can test. The output is no longer needed. Functions were left in though.
            ((terminal? top-of-stack) 
             (terminal-action parser-accum top-of-stack stack current-token token-list))
            (else 
             (non-terminal-action parser-accum top-of-stack stack current-token token-list))))))

(define terminal-action
  (lambda (parser-accum top-of-stack stack current-token token-list)
    (if (top=token? top-of-stack current-token)
        (token-reader-helper parser-accum (pop stack) (consume token-list))
        (print-error current-token stack))))
(define non-terminal-action
  (lambda (parser-accum top-of-stack stack current-token token-list)
    (let ((grammar-rule (rule-for top-of-stack (terminal-for current-token))))
      (if (transition-error? grammar-rule) 
          (print-error (get-current-token token-list) grammar-rule)
          (token-reader-helper (gather-parser-output parser-accum top-of-stack current-token)
                               (check-for-push (pop stack) grammar-rule)
                               token-list )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Edit list of functions to be displayed per line;;;
(define convert-to-display
  (lambda (parser-accum)
    (string-join 
     (reverse 
      (map (compose (lambda (x) (string-join x " "))
                    (compose reverse (lambda (x) (map symbol->string x))))
           parser-accum))
     "\n")))
(define gather-parser-output
  (lambda (parser-accum top-of-stack token)
    (cond ((eq? top-of-stack 'def)    (cons (list (token-value token)) parser-accum))
          ((eq? top-of-stack 'formal) (cons (cons (token-value token) (car parser-accum)) 
                                            (cdr parser-accum)))
          (else parser-accum ))))
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Misc helper function;;;
(define end? 
  (lambda (stack token-list)
    (and (end-of-stream? (get-top-of-stack stack)) (end-input-stream? token-list))))
(define top=token?
  (lambda (top-of-stack token)
    (or (equal? top-of-stack (token-value token))
        (eq? top-of-stack (terminal-for token)))))
(define end-of-file-token?
  (lambda (token)
    (equal? (token-type token) '<end-of-file>) ))
(define compose
  (lambda (f g)
    (lambda (lst)
      (f (g lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;Stack;;;;;;;;;;;;;;
(define get-top-of-stack car)
(define pop cdr)
(define push
  (lambda (stack value)
    (append value stack) ))
(define end-of-stream?
  (lambda (item)
    (equal? '$ item)) )
(define check-for-push
  (lambda (stack rule)
    (if (equal? rule '(epsilon))
        stack
        (push stack rule))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;Tokens;;;;;;;;;;;;;;;
(define main-check?
  (lambda (token)
    (eq? (token-value token) 'main) ))
(define boolean?
  (lambda (token)
    (and (or (eq? (token-value token) 'true)
             (eq? (token-value token) 'false))
         (eq? (token-type token) '<keyword>)) ))

(define ident-terminal   'identifier        )
(define main-terminal    'identifier        )
(define int-terminal     'number            )
(define invalid-terminal 'invalid-identifier)
(define boolean-terminal 'boolean           )

(define terminal-for
  (lambda (token)
    (cond ((<invalid-ident>-token? token) invalid-terminal)
          ((<identifier>-token?    token) ident-terminal  )
          ((main-check?            token) main-terminal   )
          ((<integer>-token?       token) int-terminal    )
          ((boolean?               token) boolean-terminal)
          (else (token-value token)) )))

;;;token types;;;
(define <identifier>-token?    (lambda (token) (check-type? '<identifier> token)))
(define <integer>-token?       (lambda (token) (check-type? '<integer> token)))
(define <invalid-ident>-token? (lambda (token) (check-type? '<invalid-identifier> token)))
(define <keyword>-token?       (lambda (token) (check-type? '<keyword> token)))
(define <separator>-token?     (lambda (token) (check-type? '<separator> token)))
(define <operator>-token?      (lambda (token) (check-type? '<operator> token)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;Error-check;;;;;;;;;;;;;;;;;;;;
(define print-error
  (lambda (token stack)
    (if (end-of-file-token? token)
        (list 'Compile 'abort. 'Source 'code 'file 'empty.)
        (list  'ERROR: 'with 
               (symbol->string (token-value token)) 
               'on 'column:  (token-col token) 
               'on 'row: (token-row token)
               'stack: stack )) ))
(define transition-error?
  (lambda (grammar-rule)
    (eq? grammar-rule err)) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(parser "klein-programs/euclid.kln")
(parser "klein-programs/horner.kln")
(parser "klein-programs/circular-prime.kln")
(parser "klein-programs/farey.kln")
(parser "klein-programs/fibonacci.kln")
(parser "klein-programs/horner-parameterized.kln")

