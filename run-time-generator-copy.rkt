#lang racket

(require "type-checker.rkt"
         "parser.rkt"
         "semantic-actions.rkt"
         "symbol-table.rkt"
         "symbol-table-helpers.rkt")

(semantic-analysis (parser "klein-programs/08-print.kln"))

(define generate-function-args 
  (lambda (line-num)
    '()))

(define generate-runtime
  (lambda (line-num)
    (let ((generated-args (generate-function-args line-num)))
      (let ((line-num (length (generate-function-args line-num))))
            (append (generate-function-args line-num)
                    (list (string-append (number->string line-num)       ": LDA 6,2(7)\n")
                          (string-append (number->string (+ 1 line-num)) ":  ST 6,1(0)\n")
                          (list (string-append (number->string (+ 2 line-num)) ": LDA 7,~a(0)\n")
                                (symbol-table-lookup 'main)) ; note this for future reference for functions
                          (string-append (number->string (+ 3 line-num)) ":  LD 2,2(0)\n")
                          (string-append (number->string (+ 4 line-num)) ": OUT 2,0,0\n" )
                          (string-append (number->string (+ 5 line-num)) ":HALT 0,0,0\n" ) ))))))
                   
(define generate-number
  (lambda (value top-of-call-stack line-num)
    (append (list (string-append (number->string line-num)       (format ": LDC 4,~a(0)\n" value))
                  (string-append (number->string (+ 1 line-num)) (format ":  ST 4,~a(0)\n" top-of-call-stack)) ))))
           
;(define generate-def 
;  (lambda ()))

(define generate
  (lambda (ast)
    (let ((symbol-table (symbol-table ast)))
      (letrec ((generate-everything
                (lambda (ast top-of-call-stack line-num)
                  (cond 
                    ((program? ast)     (let ((generated-runtime (generate-runtime line-num)))
                                          (let ((line-num (length generated-runtime)))
                                            (append generated-runtime
                                                    (generate-everything (program-definitions ast) (+ top-of-call-stack 1) line-num)))))
                    ((definitions? ast) (let ((generated-def (generate-everything (definitions-def ast) top-of-call-stack line-num)))
                                          (let ((line-num (length generated-def)))
                                            (append generated-def
                                                    (generate-everything (definitions-definitions ast) top-of-call-stack line-num)))))
                    ((def? ast)         (hash-set! (hash-ref symbol-table (identifier-value (def-id ast))) 'tm-line line-num)
                                        (append (list "* MAIN STARTS HERE\n") ; add def to symbol table, branch number
                                                (generate-everything (def-body ast) top-of-call-stack line-num))) ; use the let implementation to find out length of whole function
                    ((body? ast)        (append (generate-everything (body-expr ast) top-of-call-stack line-num)))
                    ((number? ast)      (append (generate-number (number-value ast) top-of-call-stack line-num)))
                    ;(else (list "NOTHING MATCHED IN generate FUNCTION" ast)) 
                    ))))
        (printf (create-tm-string (generate-everything ast 1 0) symbol-table)) ))));check for parser error

(define eval-tm-lines
  (lambda (symbol-table)
    (lambda (line)
      (if (and (list? line) (eqv? (length line) 2))
          (format (car line) (hash-ref ((cadr line) symbol-table) 'tm-line))
          line))))
  
(define create-tm-string
  (lambda (list-of-tm-lines symbol-table)
    (string-join (map (eval-tm-lines symbol-table)
                      list-of-tm-lines))))
  
  
  
  