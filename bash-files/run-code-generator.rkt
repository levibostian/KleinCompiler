#lang racket

(require "../type-checker.rkt"
         "../parser.rkt"
         "../run-time-generator.rkt")

(define file-name
  (vector-ref (current-command-line-arguments) 0))

(generate (semantic-actions (parser file-name)))