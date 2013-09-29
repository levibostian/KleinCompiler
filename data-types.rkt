#lang racket
; Klein compiler
; cs4550 - Fall 2013
; 
; Team RackAttack
; Description: Helper functions for data types of Klein language
;              returning #t or #f if input belongs to data type 
;              or not

(provide (all-defined-out))

;; Define data types
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

;; Check if input belongs to data type
(define member? 
  (lambda (item lyst) 
    (if (member item lyst) #t #f)))

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

(define num? 
  (lambda (num) 
    (integer? (string->number num)) ))

(define separator?   
  (lambda (sym ) 
    (member? sym separator) ))

(define punctuation? 
  (lambda (char) 
    (member? char punctuation) ))

(define whitespace?  
  (lambda (char) 
    (member? char whitespace) ))

(define stopping-char?
  (lambda (char)
    (or (punctuation? char)
        (separator?   char)
        (operator?    char)
        (whitespace?  char) )))
