#lang racket

(require "../run-time-generator.rkt")

(define file-name
  (vector-ref (current-command-line-arguments) 0))
(define tm-file-name
  (vector-ref (current-command-line-arguments) 1))

(write-out file-name tm-file-name)
