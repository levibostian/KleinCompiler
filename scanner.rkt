#lang racket
; Klein compiler
; cs4550 - Fall 2013
; 
; Team RackAttack

(require "scanner-helper-functions.rkt")
(require "data-types.rkt")

(provide (all-defined-out))

(define check-for/add-tokens
  (lambda (current-char tokens char-accum column-num row-num)
    (cond ((stopping-char? current-char) (token-additions current-char tokens char-accum column-num row-num))
          (else tokens) )))

(define token-additions
  (lambda (current-char tokens char-accum column-num row-num)
    (if (whitespace? current-char)
        (add-char-accum-to-token char-accum tokens (get-column-num char-accum column-num) row-num)
        (combine-tokens (generate-token current-char (get-column-num current-char column-num) row-num)
                        (add-char-accum-to-token char-accum tokens (get-column-num char-accum column-num) row-num)) )))

(define add-char-accum-to-token
  (lambda (chars tokens column-num row-num)
    (if (eq? chars empty-char)
        tokens
        (combine-tokens (generate-token chars column-num row-num) tokens))))

(define reset-or-accum-chars
  (lambda (current-char chars)
    (if (stopping-char? current-char)
        empty-char
        (string-append chars current-char)) ))


(define scanner
  (lambda (source-code-path)
    (let ((port (open-input-file source-code-path)))
      (reverse (file-reader port))) ))

(define file-reader
  (lambda (port)
    (file-reader-helper port '() 1) ))

(define file-reader-helper
  (lambda (port token-list row-in-file)
    (let ((next-line-code (read-line port)))
      (if (eof-object? next-line-code)
          token-list
          (let ((token-result 
                 (line-reader next-line-code token-list empty-char 1 row-in-file)))
            (file-reader-helper port token-result (+ 1 row-in-file))))) ))

(define line-reader
  (lambda (line token-accum char-accum  column-in-file row-in-file)
    (cond ((end-of-line? line) (add-char-accum-to-token char-accum token-accum (get-column-num char-accum column-in-file) row-in-file))
          ((comment? line) token-accum)
          (else 
           (let ((current-char (get-next-char line)))
             (line-reader (rest-of line)
                          (check-for/add-tokens current-char token-accum char-accum column-in-file row-in-file)
                          (reset-or-accum-chars current-char char-accum)
                          (+ 1 column-in-file)
                          row-in-file)))) ))

(define generate-token
  (lambda (char-or-accum column-num row-num)
    (cond ((keyword? char-or-accum)     (build-token "<keyword>"     char-or-accum column-num row-num))
          ((num? char-or-accum)         (build-token "<integer>"     char-or-accum column-num row-num))
          ((operator? char-or-accum)    (build-token "<operator>"    char-or-accum column-num row-num))
          ((separator? char-or-accum)   (build-token "<separator>"   char-or-accum column-num row-num))
          ((punctuation? char-or-accum) (build-token "<punctuation>" char-or-accum column-num row-num))
          (else (build-token "<identifier>" char-or-accum column-num row-num))) ))

(define build-token
  (lambda (token-name char-or-accum column-num row-num)
    (list (string->symbol token-name) char-or-accum column-num (number->string row-num)) ))

(define get-column-num ;instead of column num being end of char-or-accum, make it beginning
  (lambda (char-or-accum column-num)
    (if (stopping-char? char-or-accum)
        (number->string column-num)
        (number->string (- column-num (string-length char-or-accum)))) ))
