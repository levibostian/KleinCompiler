#lang racket

(require "../print-parser.rkt"
         "../semantic-actions.rkt"
         "../parser.rkt")

(define file-name
  (vector-ref (current-command-line-arguments) 0))

(display (print/program (parser file-name))) 