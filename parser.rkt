#lang racket
; Klein compiler
; cs4550 - Fall 2013
; 
; Team RackAttack

(define pop cdr)
(define top-of-stack car)
(define next-token car)
(define current-token car)
(define rest-of-tokens cdr)
(define token-type car)
(define token-value cadr)
(define token-col caddr)
(define token-row cadddr)
(define end-of-tokens?
  (lambda (stack)
    (eq? '$ (top-of-stack stack)) ))
(define identifier? 
  (lambda (token)
    (eq? (token-type token) '<identifier>) ))
(define numb?
  (lambda (token)
    (eq? (token-type token) '<integer>) ))
(define main?
  (lambda (token)
    (eq? (token-value token) 'main) ))
(define boolean?
  (lambda (token)
    (and (or (eq? (token-value token) 'true)
             (eq? (token-value token) 'false))
         (eq? (token-type token) '<keyword>)) ))
(define not-error?
  (lambda (grammer-rule)
    (not (eq? grammer-rule err)) ))
(define push
  (lambda (stack value)
    (append value stack) ))

(require "scanner.rkt")
(require "parse-table.rkt")

(provide (all-defined-out))

(define parser
  (lambda (source-code-path)
      (token-reader (scanner source-code-path)) ))

(define token-reader
  (lambda (token-list)
    (token-reader-helper '() (list 'program '$) token-list) ))

(define token-reader-helper
  (lambda (parser-accum stack token-list)
    (if (and (end-of-tokens? stack) (eq? 1 (length token-list)))
        #t
        (cond ((terminal? (top-of-stack stack)) 
               (if (eq? (top-of-stack stack) (terminal-look-up (current-token token-list)))
                   (token-reader-helper parser-accum (pop stack) (rest-of-tokens token-list))
                   stack))
              (else 
               (let ((grammer-rule (table-look-up (top-of-stack stack) (terminal-look-up (current-token token-list)))))
                 (if (not-error? grammer-rule)
                     (if (equal? grammer-rule '(epsilon))
                         (token-reader-helper parser-accum (pop stack) token-list)
                         (token-reader-helper parser-accum (push (pop stack) grammer-rule) token-list))
                     (current-token token-list))))) )))

(define terminal-look-up
  (lambda (token)
    (cond ((or (identifier? token)
               (main? token)) 'identifier)
          ((numb? token) 'number)
          ((boolean? token) 'boolean)
          (else
           (token-value token))) ))
