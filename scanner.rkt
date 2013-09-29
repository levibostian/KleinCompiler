#lang racket
; Klein compiler
; cs4550 - Fall 2013
; 
; Team RackAttack

(require "data-types.rkt")

(provide scanner
         file-reader
         file-reader-helper
         line-reader
         rest-of
         member?
         num?
         keyword?
         separator?
         punctuation?
         whitespace?
         operator?
         comment?
         keyword?
         stopping-char?
         separator?
         generate-token
         check-for/add-tokens
         reset-or-accum-chars)

(define end-of-line? 
  (lambda (char) 
    (eq? (string-length char) 0) ))
(define get-next-char (lambda (code-line) (substring code-line 0 1) ))
(define rest-of (lambda (line) (substring line 1) ))

(define combine-tokens cons)

(define check-for/add-tokens
  (lambda (current-char tokens chars)
    (cond ((stopping-char? current-char) (token-additions current-char tokens chars))
          (else tokens) )))

(define token-additions
  (lambda (current-char tokens chars)
    (if (whitespace? current-char)
        (add-chars-token chars tokens)
        (combine-tokens (generate-token current-char)
                        (add-chars-token chars tokens)) )))

(define add-chars-token
  (lambda (chars tokens)
    (if (eq? chars empty-char)
        tokens
        (combine-tokens (generate-token chars) tokens))))

(define reset-or-accum-chars
  (lambda (current-char chars)
    (if (stopping-char? current-char)
        empty-char
        (string-append chars current-char)) ))


(define run-scanner
  (lambda (path-name)
    (scanner path-name)))

(define scanner
  (lambda (source-code-path)
    (let ((port (open-input-file source-code-path)))
      (reverse (file-reader port))) ))

(define file-reader
  (lambda (port)
    (file-reader-helper port '() 0) ))

(define file-reader-helper
  (lambda (port token-list row-in-file)
    (let ((next-line-code (read-line port)))
      (if (eof-object? next-line-code)
          token-list
          (let ((token-result 
                 (line-reader next-line-code token-list empty-char row-in-file 0)))
            (file-reader-helper port token-result (+ 1 row-in-file))))) ))

(define line-reader
  (lambda (line token-accum char-accum row-in-file column-in-file)
    (cond ((end-of-line? line) (add-chars-token char-accum token-accum))
          ((comment? line) token-accum)
          (else 
           (let ((current-char (get-next-char line)))
             (line-reader (rest-of line)
                          (check-for/add-tokens current-char token-accum char-accum)
                          (reset-or-accum-chars current-char char-accum)
                          row-in-file
                          (+ 1 column-in-file))))) ))

(define generate-token
  (lambda (char-or-accum)
    (cond ((keyword? char-or-accum)     (string-append "<keyword> "     char-or-accum))
          ((num? char-or-accum)      (string-append "<integer> "     char-or-accum))
          ((operator? char-or-accum)    (string-append "<operator> "    char-or-accum))
          ((separator? char-or-accum)   (string-append "<separator> "   char-or-accum))
          ((punctuation? char-or-accum) (string-append "<punctuation> " char-or-accum))
          (else (string-append "<identifier> " char-or-accum))) ))
