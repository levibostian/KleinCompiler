#lang racket
; Klein compiler
; cs4550 - Fall 2013
; 
; Team RackAttack

(provide scanner
         line-reader
         char-reader
         token-factory)

(define scanner
  (lambda (source-code-path)
    (line-reader "" (open-input-file source-code-path)) ))

(define line-reader
  (lambda (current-code-line input-port)
    ; send line of code to function that reads character by character
    ))

;(define char-reader
;  (lambda (char accum-string)
;    (if (or (eq? char " ")
;            (eq? char "(")
;            (eq? char ")")
;            (eq? char ":")
;            (eq? char ","))
;        ;run accum through function for token creation
;        (token-factory accum-string)
;        "else")
;        ))

(define token-factory
  (lambda (accum)
    (cond ((or (eq? accum "integer")
               (eq? accum "boolean")) (string-append "<type> " accum))
          ((or (eq? accum "(")
               (eq? accum ")")
               (eq? accum ":")
               (eq? accum ",")) (string-append "<punctuation> " accum))
          (else (string-append "<identifier> " accum))) ))

