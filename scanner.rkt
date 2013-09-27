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

(define empty-char "")
(define operators          (list "+" "-" "/" "*" "<" "="))
(define whitespace         (list " " "\r" "\n" "\r\n" "\t"))
(define type               (list "integer" "boolean"))
(define boolean            (list "true" "false"))
(define math-operator      (list "+" "-" "*" "/"))
(define comparator         (list "<" "="))
(define separator          (list "," ":"))
(define punctuation        (list "(" ")"))
(define conditional        (list "if" "then" "else" "endif"))
(define primitive          (list "main" "print"))
(define boolean-connective (list "or" "and" "not"))
(define comment            (list "//"))

(define comment?
  (lambda (line)
    (cond ((< (string-length line) 2) #f)
          ((member? (substring line 0 2) comment) #t)
          (else #f)) ))

(define keyword?
  (lambda (item)
    (or (member? item type)
        (member? item boolean)
        (member? item conditional)
        (member? item boolean-connective)
        (member? item primitive))))

(define operator?
  (lambda (sym)
    (or (member? sym math-operator)
        (member? sym comparator)) ))

(define num? (lambda (num) (integer? (string->number num)) ))
(define member? (lambda (item lyst) (if (member item lyst) #t #f)))
(define separator?   (lambda (sym ) (member? sym separator) ))
(define punctuation? (lambda (char) (member? char punctuation) ))
(define whitespace?  (lambda (char) (member? char whitespace) ))
(define end-of-line? (lambda (char) (eq? (string-length char) 0) ))

(define stopping-char?
  (lambda (char)
    (or (punctuation? char)
        (separator?   char)
        (operator?    char)
        (whitespace?  char) )))

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
