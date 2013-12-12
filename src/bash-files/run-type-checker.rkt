#lang racket

(require "../type-checker.rkt"
         "../parser.rkt"
         "../print-parser.rkt")

(define file-name
  (vector-ref (current-command-line-arguments) 0))

(display (print/program (semantic-analysis (parser file-name))))