#lang racket
; Klein compiler
; cs4550 - Fall 2013
; 
; Team RackAttack

(provide scanner
         file-reader
         file-reader-helper
         line-reader
         next-char
         member?
         punctuation?
         whitespace?
         operator?
         token-factory)

(define empty-char-accum "")
(define operators-list (list "+" "-" "/" "*" "<" ">" "="))
(define punctuations-list (list "(" ")" ":" ","))
(define whitespaces-list (list " "))

(define scanner
  (lambda (source-code-path)
    (let ((port (open-input-file source-code-path)))
      (reverse (file-reader port))) ))

(define file-reader
  (lambda (port)
    (file-reader-helper port '()) ))

(define file-reader-helper
  (lambda (port token-list)
    (let ((next-line-code (read-line port)))
      (if (eof-object? next-line-code)
          token-list
          (let ((token-result 
                 (line-reader next-line-code token-list empty-char-accum)))
            (file-reader-helper port token-result)))) ))

(define line-reader
  (lambda (line token-accum char-accum)
    (let ((current-char (string-ref line 0)))
      (cond (punctuation? current-char)
              (line-reader (next-char line) (punctuation-token-construct current-char char-accum token-accum) empty-char-accum)
            (whitespace? current-char)
              (line-reader (next-char line) (whitespace-token-construct char-accum token-accum) empty-char-accum)
            (operator? current-char)
              (line-reader (next-char line) (operator-token-construct current-char char-accum token-accum) empty-char-accum)
            (else ;char, so add to accum
              (line-reader (next-char line) (string-append char-accum current-char)))))))

(define whitespace-token-construct
  (lambda (char-accum token-accum)
    (cons (token-factory char-accum) token-accum) ))

(define punctuation-token-construct
  (lambda (punctuation-char char-accum token-accum)
    (cons (token-factory punctuation-char) (cons (token-factory char-accum) token-accum)) ))

(define operator-token-construct
  (lambda (operator-char char-accum token-accum)
    (cons (token-factory operator-char) (cons (token-factory char-accum) token-accum)) ))

(define next-char
  (lambda (line)
    (substring line 1) ))
                           
(define member?
  (lambda (item lyst)
    (or (member lyst item)) ))

(define punctuation? 
  (lambda (char)
    (member? char punctuations-list) ))

(define whitespace?
  (lambda (char)
    (member? char whitespaces-list) ))

(define operator?
  (lambda (char)
    (member? char operators-list) ))

(define token-factory
  (lambda (accum)
    (cond ((or (eq? accum "integer")
               (eq? accum "boolean")) (string-append "<type> " accum))
          ((or (eq? accum "(")
               (eq? accum ")")
               (eq? accum ":")
               (eq? accum ",")) (string-append "<punctuation> " accum))
          (else (string-append "<identifier> " accum))) ))