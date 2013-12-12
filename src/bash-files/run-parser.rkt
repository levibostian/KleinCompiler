#lang racket

(require "../parser.rkt")

(define file-name
  (vector-ref (current-command-line-arguments) 0))

(define print-parser
  (parser file-name))