#lang racket
; Klein compiler
; cs4550 - Fall 2013
; 
; Team RackAttack

(require "scanner.rkt")
(require "parse-table.rkt")

(provide (all-defined-out))

;;;Parser;;;
(define parser
  (lambda (source-code-path)
    (token-reader (scanner source-code-path)) ))

(define token-reader
  (lambda (token-list)
    (token-reader-helper "" (list 'program '$) token-list) ))

(define token-reader-helper
  (lambda (parser-accum stack token-list)
    (let ((top-of-stack (get-top-of-stack stack))
          (current-token (get-current-token token-list)))
      (cond ((end? stack current-token) (printf parser-accum))
            ((terminal? top-of-stack) 
             (terminal-action parser-accum top-of-stack stack current-token token-list))
             (else
              (let ((grammar-rule (rule-for top-of-stack (terminal-for current-token))))
                (if (transition-error? grammar-rule) 
                    (print-error (get-current-token token-list) grammar-rule)
                    (token-reader-helper (parser-output parser-accum top-of-stack current-token)
                                         (check-for-push (pop stack) grammar-rule)
                                         token-list ))))))))

(define parser-output
  (lambda (parser-accum top-of-stack token)
    (cond ((eq? top-of-stack 'def) (string-append parser-accum
                                                  (symbol->string (token-value token))
                                                  " "))
          ((eq? top-of-stack 'formal) (string-append parser-accum
                                                    (symbol->string (token-value token))
                                                    " "))
          ((eq? top-of-stack 'body) (string-append parser-accum
                                                   "\n"))
          (else parser-accum ))))

(define terminal-action
  (lambda (parser-accum top-of-stack stack current-token token-list)
    (if (top=token? top-of-stack current-token)
        (token-reader-helper parser-accum (pop stack) (consume token-list))
        (print-error current-token stack))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;Stack;;;
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
;;;;;;;;;;;
;;;Token-stream;;;
(define next-token cadr)
(define get-current-token car)
(define consume cdr)
(define end-input-stream?
  (lambda (token)
    (equal? '$ (token-value token))))
;;;;;;;;;;;;;;;;;;
;;;Tokens;;;
(define token-type car)
(define token-value cadr)
(define token-col caddr)
(define token-row cadddr)
(define check-type?
  (lambda (type token)
    (eq? (token-type token) type)))
(define main-check?
  (lambda (token)
    (eq? (token-value token) 'main) ))
(define boolean?
  (lambda (token)
    (and (or (eq? (token-value token) 'true)
             (eq? (token-value token) 'false))
         (eq? (token-type token) '<keyword>)) ))
(define terminal-for
  (lambda (token)
    (cond ((or (check-type? '<identifier> token)
               (main-check? token)) 'identifier)
          ((check-type? '<integer> token) 'number)
          ((boolean? token) 'boolean)
          (else (token-value token))) ))
;;;;;;;;;;;;
;;;Error-check;;;
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
;;;;;;;;;;;;;;;;;

(parser "klein-programs/euclid.kln")
(parser "klein-programs/horner.kln")
(parser "klein-programs/circular-prime.kln")
(parser "klein-programs/farey.kln")
(parser "klein-programs/fibonacci.kln")
(parser "klein-programs/horner-parameterized.kln")