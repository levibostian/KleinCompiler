#lang racket

(require "type-checker.rkt"
         "parser.rkt")

(semantic-analysis (parser "klein-programs/08-print.kln"))








(define generator 
  (lambda (ast line-num top-of-call-stack)
    (cond
      ((program? ast) (list 
                            (generator