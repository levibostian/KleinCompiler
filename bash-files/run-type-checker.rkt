#lang racket

(require "../type-checker.rkt"
         "../parser.rkt")

(define file-name
  (vector-ref (current-command-line-arguments) 0))

(semantic-analysis (parser file-name))