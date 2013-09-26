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

(define get-next-char
  (lambda (code-line)
    (substring code-line 0 1) ))

(define char-only-has-value
  (lambda (char char-accum)
    (and (= (string-length char) 0)
         (> (string-length char) 0)) ))

(define char-accum-only-has-value
  (lambda (char char-accum)
    (and (> (string-length char-accum) 0)
         (= (string-length char) 0)) ))

(define both-accums-have-values
  (lambda (char char-accum)
    (and (> (string-length char-accum) 0)
         (> (string-length char) 0)) ))

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
    (if (end-of-line? line)
        (add-to-token-accum line token-accum char-accum)
        (let ((current-char (get-next-char line)))
          (line-reader (rest-of line)
                       (check-for/add-tokens current-char token-accum char-accum)
                       (reset/accum-chars current-char char-accum)
                       ))) ))

(define check-for/add-tokens
  (lambda (current-char tokens chars)
    (cond ((or (punctuation? current-char);this will be cleaned up
               (operator?    current-char)) (add-to-token-accum current-char tokens chars))
          ((whitespace?  current-char) (whitespace->token chars tokens))
          (else tokens) )))

(define add-to-token-accum
  (lambda (char tokens-list char-accum)
    (cond ((both-accums-have-values char char-accum) (cons (token-factory char) (cons (token-factory char-accum) tokens-list)))
          ((char-accum-only-has-value char char-accum) (cons (token-factory char-accum) tokens-list))
          ((char-only-has-value char char-accum) (cons (token-factory char) tokens-list))
          (else 
           tokens-list)) ))

(define reset/accum-chars
  (lambda (current-char chars)
    (if (or (punctuation? current-char)
            (operator?    current-char)
            (whitespace?  current-char))
        ""
        (string-append chars current-char)) ))

(define whitespace->token
  (lambda (char-accum token-accum)
    (if (> (string-length char-accum) 0)
        (cons (token-factory char-accum) token-accum)
        token-accum) ))

(define rest-of
  (lambda (line)
    (substring line 1) ))

(define token-factory
  (lambda (char-or-accum)
    (cond ((or (eq? char-or-accum "integer")
               (eq? char-or-accum "boolean")) (string-append "<type> " char-or-accum))
          ((or (eq? char-or-accum "(")
               (eq? char-or-accum ")")
               (eq? char-or-accum ":")
               (eq? char-or-accum ",")) (string-append "<punctuation> " char-or-accum))
          (else (string-append "<identifier> " char-or-accum))) ))

(scanner "klein-programs/euclid.kln")