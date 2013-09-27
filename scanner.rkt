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
         generate-token
         check-for/add-tokens
         reset/accum-chars)

(define empty-char "")
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
    (if (end-of-line? line)
        (add-chars-token char-accum token-accum)
        (let ((current-char (get-next-char line)))
          (line-reader (rest-of line)
                       (check-for/add-tokens current-char token-accum char-accum)
                       (reset/accum-chars current-char char-accum)
                       row-in-file
                       (+ 1 column-in-file)))) ))

(define check-for/add-tokens
  (lambda (current-char tokens chars)
    (cond ((or (punctuation? current-char)
               (operator?    current-char)) (combine-tokens (generate-token current-char) 
                                                            (add-chars-token chars tokens)))
          ((whitespace?  current-char) (whitespace->token chars tokens))
          (else tokens) )))

(define combine-tokens cons)

(define add-chars-token
  (lambda (chars tokens)
    (if (eq? chars "")
        tokens
        (cons (generate-token chars) tokens))))

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
        (combine-tokens (generate-token char-accum) token-accum)
        token-accum) ))

(define rest-of
  (lambda (line)
    (substring line 1) ))

;(define add-row/column 
;  (lambda (token-value row column)
;    ;finish this

(define generate-token
  (lambda (char-or-accum)
    (cond ((or (equal? char-or-accum "integer")
               (equal? char-or-accum "boolean")) (string-append "<type> " char-or-accum))
          ((or (equal? char-or-accum "(")
               (equal? char-or-accum ")")
               (equal? char-or-accum ":")
               (equal? char-or-accum ",")) (string-append "<punctuation> " char-or-accum))
          (else (string-append "<identifier> " char-or-accum))) ))

(scanner "klein-programs/euclid.kln")