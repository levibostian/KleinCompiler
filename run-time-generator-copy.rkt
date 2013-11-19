#lang racket

(require "type-checker.rkt"
         "parser.rkt")

(semantic-analysis (parser "klein-programs/08-print.kln"))

(define generate-runtime
  (lambda (line-num)
    (let ((generated-args (generate-function-args line-num)))
      (let ((line-num       (length   (generate-function-args line-num)))
            (append (generate-function-args line-num)
                    (list (string-append (number->string line-num)       ": LDA 6,2(7)\n")
                          (string-append (number->string (+ 1 line-num)) ":  ST 6,1(0)\n")
                          (string-append (number->string (+ 2 line-num)) ": LDA 7,9(0)\n")
                          (string-append (number->string (+ 3 line-num)) ":  LD 2,2(0)\n")
                          (string-append (number->string (+ 4 line-num)) ": OUT 4,0,0\n" )
                          (string-append (number->string (+ 5 line-num)) ":HALT 0,0,0\n"))))))
                   

           
;(define generate-def 
;  (lambda ()))

(define generate
  (lambda (ast top-of-call-stack line-num)
    (cond 
      ((program? ast)     (append (list 
                                  (generate (program-definitions             ast top-of-call-stack line-num)))
      ((definitions? ast) (append (generate (definitions-def         ast top-of-call-stack line-num))
                                  (generate (definitions-definitions ast top-of-call-stack line-num))))
      ((def? ast)         (