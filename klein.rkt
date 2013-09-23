#lang racket
; Klein compiler
; cs4550 - Fall 2013
; 
; Team RackAttack

(define klein
  (lambda (source-code-path)
    (line-reader "" (open-input-file source-code-path)) ))

(define line-reader
  (lambda (current-code-line input-port)
    ; send line of code to function that reads character by character
    ))

