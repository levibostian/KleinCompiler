#lang racket
; Klein compiler
; cs4550 - Fall 2013
; 
; Team RackAttack

(require "scanner.rkt")
(require "parse-table.rkt")

(provide (all-defined-out))

(define parser
  (lambda (source-code-path)
      (token-reader (scanner source-code-path)) ))

(define token-reader
  (lambda (token-list)
    (token-reader-helper '() '('$) token-list) ))

(define token-reader-helper
  (lambda (parser-accum stack token-list)
    
    ))