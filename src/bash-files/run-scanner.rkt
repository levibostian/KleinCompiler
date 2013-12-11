#lang racket

(require "../scanner.rkt")

(define file-name
  (vector-ref (current-command-line-arguments) 0))

(scanner file-name)