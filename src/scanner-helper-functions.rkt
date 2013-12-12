#lang racket
; Klein compiler
; cs4550 - Fall 2013
; 
; Team RackAttack
; Description: Helper functions used exclusively for scanner. 

(provide (all-defined-out))

(define end-of-line? 
  (lambda (char) 
    (eq? (string-length char) 0) ))

(define get-next-char 
  (lambda (code-line) 
    (substring code-line 0 1) ))

(define rest-of 
  (lambda (line) 
    (substring line 1) ))

(define combine-tokens cons)