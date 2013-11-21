#lang racket

(require "../type-checker.rkt"
         "../parser.rkt"
         "../run-time-generator.rkt")

(define file-name
  (vector-ref (current-command-line-arguments) 0))
(define tm-file-name
  (vector-ref (current-command-line-arguments) 1))

(generate (semantic-actions (parser file-name)) tm-file-name)
