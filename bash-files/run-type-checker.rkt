#lang racket

(require "../type-checker.rkt")

(define file-name
  (vector-ref (current-command-line-arguments) 0))

(type-checker file-name)