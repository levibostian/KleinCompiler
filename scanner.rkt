#lang racket
; Klein compiler
; cs4550 - Fall 2013
; 
; Team RackAttack

(provide scanner
         file-reader
         file-reader-helper
         line-reader
         rest-of
         member?
         punctuation?
         whitespace?
         operator?
         token-factory
         check-for/add-tokens
         reset/accum-chars)

(define empty-char-accum "")
(define operators   (list "+" "-" "/" "*" "<" ">" "="))
(define punctuation (list "(" ")" ":" ","))
(define whitespace  (list " "));needs to be expanded, I think. Maybe not?

(define member? (lambda (item lyst) (if (member item lyst) #t #f)))

(define punctuation? 
  (lambda (char)
    (member? char punctuation) ))

(define whitespace?
  (lambda (char)
    (member? char whitespace) ))

(define end-of-line?
  (lambda (char)
    (eq? (string-length char) 0) ));did not make member? because this is ONLY option.

(define operator?
  (lambda (char)
    (member? char operators) ))


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

(define line-reader;do these lines include \n characters?
  (lambda (line token-accum char-accum)
    (let ((current-char (get-current-char line))) ;this was changed so that it grabs a String instead of a Char. Will make it easier
      (line-reader (rest-of line)
                   (check-for/add-tokens current-char token-accum char-accum)
                   (reset/accum-chars current-char char-accum)
                   ))))

(define get-current-char
  (lambda (code-line)
    (if (end-of-line? code-line)
        ""
        (substring code-line 0 1)) ))

(define check-for/add-tokens
  (lambda (current-char tokens chars)
    (cond ((or (punctuation? current-char);this will be cleaned up
               (operator?    current-char)) (cons (token-factory current-char) (cons (token-factory chars) tokens)))
          ((whitespace?  current-char) (whitespace->token chars tokens))
          ((end-of-line? current-char) (whitespace->token chars tokens))
          (else tokens) )))

(define reset/accum-chars
  (lambda (current-char chars)
    (if (or (punctuation? current-char)
            (operator?    current-char)
            (whitespace?  current-char))
        ""
        chars)))

(define whitespace->token
  (lambda (char-accum token-accum)
    (cons (token-factory char-accum) token-accum) ))

(define rest-of
  (lambda (line)
    (substring line 1) ))
                           
(define token-factory
  (lambda (char-or-accum)
    (cond ((or (eq? accum "integer")
               (eq? accum "boolean")) (string-append "<type> " accum))
          ((or (eq? accum "(")
               (eq? accum ")")
               (eq? accum ":")
               (eq? accum ",")) (string-append "<punctuation> " accum))
          (else (string-append "<identifier> " accum))) ))


(scanner "klein-programs/euclid.kln")