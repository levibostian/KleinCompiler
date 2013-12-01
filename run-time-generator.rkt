#lang racket

(provide (all-defined-out))
(require "type-checker.rkt"
         "parser.rkt"
         "semantic-actions.rkt"
         "symbol-table.rkt"
         "symbol-table-helpers.rkt")

(define generate-function-args 
  (lambda (line-num)
    '()))

(define generate-runtime
  (lambda (line-num)
    (let ((generated-args (generate-function-args line-num)))
      (let ((line-num (length (generate-function-args line-num))))
            (append (generate-function-args line-num)
                    (list (string-append (number->string line-num)       ": LDA 6,3(7)\n")
                          (string-append (number->string (+ 1 line-num)) ": LDC 5,0(0)\n")
                          (string-append (number->string (+ 2 line-num)) ": LDC 4,1(0)\n")
                          (list (string-append (number->string (+ 3 line-num)) ": LDA 7,~a(0)\n")
                                (symbol-table-lookup 'main)) ; note this for future reference for functions
                          (string-append (number->string (+ 4 line-num)) ":  LD 1,0(0)\n")
                          (string-append (number->string (+ 5 line-num)) ": OUT 1,0,0\n" )
                          (string-append (number->string (+ 6 line-num)) ":HALT 0,0,0\n" ) ))))))

(define generate ; make sure to check for errors from AST
  (lambda (ast)  ; find way to keep track of top of call stack
    (let ((symbol-table (symbol-table ast)))
      (letrec ((generate-everything
                (lambda (ast top-of-call-stack line-num)
                  (cond 
                    ((program? ast)     (let ((generated-runtime (generate-runtime line-num)))
                                          (let ((line-num (length generated-runtime)))
                                            (append generated-runtime
                                                    (generate-everything (program-definitions ast) (+ top-of-call-stack 1) line-num)))))
                    ((definitions? ast) (let ((generated-def (generate-everything (definitions-def ast) top-of-call-stack line-num)))
                                          (let ((line-num (+ line-num (length generated-def))))
                                            (append generated-def
                                                    (generate-everything (definitions-definitions ast) top-of-call-stack line-num)))))
                    ((def? ast)         (let ((generated-body (generate-everything (def-body ast) top-of-call-stack (+ 1 line-num))))
                                          (let ((line-num-after-body (+ (+ 1 line-num) (length generated-body))))          
                                            (hash-set! (hash-ref symbol-table (identifier-value (def-id ast))) 'tm-line line-num)
                                            (append (list (string-append "* " (symbol->string (identifier-value (def-id ast))) "\n")) ; add def to symbol table, branch number
                                                    (list (string-append (number->string line-num) ": ADD 3,5,0\n"))
                                                    generated-body
                                                    (list (string-append (number->string line-num-after-body) ":  ST 1,0(3)\n"))
                                                    (list (string-append (number->string (+ 1 line-num-after-body)) ": ADD 5,3,0\n"))
                                                    (list (string-append (number->string (+ 2 line-num-after-body)) ": LDA 7,0(6)\n")))))) ; use the let implementation to find out length of whole function
                    ((print-body? ast)  (let ((generated-print-expr (generate-everything (print-body-print-expr ast) top-of-call-stack line-num)))
                                          (let ((line-num-after-print-expr (+ line-num (length generated-print-expr))))
                                            (let ((generated-print (generate-print generated-print-expr line-num-after-print-expr top-of-call-stack)))
                                              (let ((line-num-after-print (+ line-num (length generated-print))))
                                                (append  generated-print
                                                         (generate-everything (print-body-expr ast) top-of-call-stack line-num-after-print)))))))
                    ((body? ast)        (append (generate-everything (body-expr ast) top-of-call-stack line-num)))
                    ((print~? ast)      (append (generate-everything (print~-expr ast) top-of-call-stack line-num)))
                    ((addition? ast)    (let ((generated-left-expr (generate-everything (addition-left ast) top-of-call-stack line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-right-expr (generate-everything (addition-right ast) top-of-call-stack line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (generate-addition generated-left-expr generated-right-expr line-num))))))
                    ((subtraction? ast) (let ((generated-left-expr (generate-everything (subtraction-left ast) top-of-call-stack line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-right-expr (generate-everything (subtraction-right ast) top-of-call-stack line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (generate-subtraction generated-left-expr generated-right-expr line-num))))))
                    ((multiplication? ast) (let ((generated-left-expr (generate-everything (multiplication-left ast) top-of-call-stack line-num)))
                                             (let ((line-num (+ line-num (length generated-left-expr))))
                                               (let ((generated-right-expr (generate-everything (multiplication-right ast) top-of-call-stack line-num)))
                                                 (let ((line-num (+ line-num (length generated-right-expr))))
                                                   (generate-multiplication generated-left-expr generated-right-expr line-num))))))
                    ((division? ast)    (let ((generated-left-expr (generate-everything (division-left ast) top-of-call-stack line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-right-expr (generate-everything (division-right ast) top-of-call-stack line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (generate-division generated-left-expr generated-right-expr line-num))))))
                    ((negative-value? ast) (let ((generated-neg-val-expr (generate-everything (negative-value-value ast) top-of-call-stack line-num)))
                                             (let ((line-num (+ line-num (length generated-neg-val-expr))))
                                               (generate-negative-value generated-neg-val-expr line-num))))
                    ((and~? ast)        (let ((generated-left-expr (generate-everything (and~-left ast) top-of-call-stack line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-right-expr (generate-everything (and~-right ast) top-of-call-stack line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (generate-and~ generated-left-expr generated-right-expr line-num))))))
                    ((or~? ast)         (let ((generated-left-expr (generate-everything (or~-left ast) top-of-call-stack line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-right-expr (generate-everything (or~-right ast) top-of-call-stack line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (generate-or~ generated-left-expr generated-right-expr line-num))))))
                    ((equals? ast)      (let ((generated-left-expr (generate-everything (equals-left ast) top-of-call-stack line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-right-expr (generate-everything (equals-right ast) top-of-call-stack line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (generate-equals generated-left-expr generated-right-expr line-num))))))
                    ((less-than? ast)   (let ((generated-left-expr (generate-everything (less-than-left ast) top-of-call-stack line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-right-expr (generate-everything (less-than-right ast) top-of-call-stack line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (generate-less-than generated-left-expr generated-right-expr line-num))))))
                    ((not? ast)         (let ((generated-not-expr (generate-everything (not-value ast) top-of-call-stack line-num)))
                                             (let ((line-num (+ line-num (length generated-not-expr))))
                                               (generate-not generated-not-expr line-num))))
                    ((number? ast)      (append (generate-number (number-value ast) top-of-call-stack line-num)))
                    ((boolean~? ast)    (append (generate-boolean (boolean~-value ast) top-of-call-stack line-num)))
                    ((if~? ast)         (let ((generated-test-expr (generate-everything (if~-test ast) top-of-call-stack line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-then-expr (generate-everything (if~-then ast) top-of-call-stack line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (let ((generated-else-expr (generate-everything (if~-else ast) top-of-call-stack line-num)))
                    ;(else (list "NOTHING MATCHED IN generate FUNCTION" ast))
                    ))))
        (printf (create-tm-string (generate-everything ast 1 0) symbol-table))) )));check for parser error

(define generate-number
  (lambda (value top-of-call-stack line-num)
    (append (list (string-append (number->string line-num)       (format ": LDC 1,~a(0)\n" value))
                  (string-append (number->string (+ 1 line-num)) ":  ST 1,0(5)\n")
                  (string-append (number->string (+ 2 line-num)) ": ADD 5,4,5\n") ))))

(define generate-boolean
  (lambda (value top-of-call-stack line-num)
    (append (list (string-append (number->string line-num) (if (eq? value 'true) 
                                                               ": LDC 1,1(0)\n"
                                                               ": LDC 1,0(0)\n"))
                  (string-append (number->string (+ 1 line-num)) ":  ST 1,0(5)\n")
                  (string-append (number->string (+ 2 line-num)) ": ADD 5,4,5\n")))))

(define generate-print
  (lambda (tm-print-code line-num top-of-call-stack)
    (append tm-print-code
            (list (string-append (number->string line-num)       ":  LD 1,-1(5)\n")
                  (string-append (number->string (+ 1 line-num)) ": OUT 1,0,0\n")))))


(define generate-addition
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)       ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num)) ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num)) ": ADD 1,1,2\n")
                  (string-append (number->string (+ 3 line-num)) ":  ST 1,0(5)\n")
                  (string-append (number->string (+ 4 line-num)) ": ADD 5,4,5\n")))))

(define generate-subtraction
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)       ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num)) ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num)) ": SUB 1,1,2\n")
                  (string-append (number->string (+ 3 line-num)) ":  ST 1,0(5)\n")
                  (string-append (number->string (+ 4 line-num)) ": ADD 5,4,5\n")))))

(define generate-multiplication
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)       ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num)) ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num)) ": MUL 1,1,2\n")
                  (string-append (number->string (+ 3 line-num)) ":  ST 1,0(5)\n")
                  (string-append (number->string (+ 4 line-num)) ": ADD 5,4,5\n")))))

(define generate-division
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)       ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num)) ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num)) ": DIV 1,1,2\n")
                  (string-append (number->string (+ 3 line-num)) ":  ST 1,0(5)\n")
                  (string-append (number->string (+ 4 line-num)) ": ADD 5,4,5\n")))))

(define generate-negative-value
  (lambda (neg-val-expr line-num)
    (append neg-val-expr
            (list (string-append (number->string line-num)       ":  LD 1,-1(5)\n")
                  (string-append (number->string (+ 1 line-num)) ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num)) ": SUB 1,1,2\n")
                  (string-append (number->string (+ 3 line-num)) ": SUB 1,1,2\n")
                  (string-append (number->string (+ 4 line-num)) ":  ST 1,0(5)\n")
                  (string-append (number->string (+ 5 line-num)) ": ADD 5,4,5\n")))))

(define generate-and~;optimize?
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)        ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num))  ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num))  ": JNE 1,0(7)\n" )
                  (string-append (number->string (+ 3 line-num))  ": JNE 2,1(7)\n" )
                  (string-append (number->string (+ 4 line-num))  ": JEQ 0,4(7)\n" )
                  (string-append (number->string (+ 5 line-num))  ": LDC 1,1(0)\n" );can maybe get rid of!
                  (string-append (number->string (+ 6 line-num))  ":  ST 1,0(5)\n" )
                  (string-append (number->string (+ 7 line-num))  ": ADD 5,4,5\n"  )
                  (string-append (number->string (+ 8 line-num))  ": JEQ 0,3(7)\n" )
                  (string-append (number->string (+ 9 line-num))  ":  ST 0,0(5)\n" )
                  (string-append (number->string (+ 10 line-num)) ": LDC 1,0(0)\n" )
                  (string-append (number->string (+ 11 line-num)) ": ADD 5,4,5\n"  ) ))))

(define generate-or~
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)        ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num))  ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num))  ": ADD 1,1,2\n"  )
                  (string-append (number->string (+ 3 line-num))  ": JNE 1,4(7)\n" )
                  (string-append (number->string (+ 4 line-num))  ":  ST 0,0(5)\n" )
                  (string-append (number->string (+ 5 line-num))  ": LDC 1,0(0)\n" )
                  (string-append (number->string (+ 6 line-num))  ": ADD 5,4,5\n"  )
                  (string-append (number->string (+ 7 line-num))  ": JEQ 0,3(7)\n" )
                  (string-append (number->string (+ 8 line-num))  ": LDC 1,1(0)\n" );can maybe get rid of!
                  (string-append (number->string (+ 9 line-num))  ":  ST 1,0(5)\n" )
                  (string-append (number->string (+ 10 line-num)) ": ADD 5,4,5\n"  ) ))))

(define generate-equals
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)        ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num))  ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num))  ": SUB 1,1,2\n"  )
                  (string-append (number->string (+ 3 line-num))  ": JEQ 1,4(7)\n" )
                  (string-append (number->string (+ 4 line-num))  ":  ST 0,0(5)\n" )
                  (string-append (number->string (+ 5 line-num))  ": LDC 1,0(0)\n" )
                  (string-append (number->string (+ 6 line-num))  ": ADD 5,4,5\n"  )
                  (string-append (number->string (+ 7 line-num))  ": JEQ 0,3(7)\n" )
                  (string-append (number->string (+ 8 line-num))  ": LDC 1,1(0)\n" );can maybe get rid of!
                  (string-append (number->string (+ 9 line-num))  ":  ST 1,0(5)\n" )
                  (string-append (number->string (+ 10 line-num)) ": ADD 5,4,5\n"  ) ))))

(define generate-less-than
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)        ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num))  ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num))  ": SUB 1,2,1\n"  )
                  (string-append (number->string (+ 3 line-num))  ": JGT 1,4(7)\n" )
                  (string-append (number->string (+ 4 line-num))  ":  ST 0,0(5)\n" )
                  (string-append (number->string (+ 5 line-num))  ": LDC 1,0(0)\n" )
                  (string-append (number->string (+ 6 line-num))  ": ADD 5,4,5\n"  )
                  (string-append (number->string (+ 7 line-num))  ": JEQ 0,3(7)\n" )
                  (string-append (number->string (+ 8 line-num))  ": LDC 1,1(0)\n" );can maybe get rid of!
                  (string-append (number->string (+ 9 line-num))  ":  ST 1,0(5)\n" )
                  (string-append (number->string (+ 10 line-num)) ": ADD 5,4,5\n"  ) ))))


(define generate-not
  (lambda (not-expr line-num)
    (append not-expr
            (list (string-append (number->string line-num)        ":  LD 1,-1(5)\n")
                  (string-append (number->string (+ 1 line-num))  ": JNE 1,4(7)\n")
                  (string-append (number->string (+ 2 line-num))  ": LDC 1,1(0)\n" );can maybe get rid of!
                  (string-append (number->string (+ 3 line-num))  ":  ST 1,0(5)\n" )
                  (string-append (number->string (+ 4 line-num)) ": ADD 5,4,5\n"  )
                  (string-append (number->string (+ 5 line-num))  ": JEQ 0,3(7)\n" )
                  (string-append (number->string (+ 6 line-num))  ":  ST 0,0(5)\n" )
                  (string-append (number->string (+ 7 line-num))  ": LDC 1,0(0)\n" )
                  (string-append (number->string (+ 8 line-num))  ": ADD 5,4,5\n"  ) ))))











(define eval-tm-lines
  (lambda (symbol-table)
    (lambda (line)
      (if (and (list? line) (eqv? (length line) 2))
          (format (car line) (hash-ref ((cadr line) symbol-table) 'tm-line))
          line))))




















(define create-tm-string
  (lambda (list-of-tm-lines symbol-table)
    (string-join (map (eval-tm-lines symbol-table)
                      list-of-tm-lines) 
                 "")))
  
(define write-out
  (lambda (file-name tm-file-name)
    (with-output-to-file tm-file-name
      (lambda () (generate (semantic-analysis (parser file-name))))
      #:exists 'replace)))

;(write-out "klein-programs/08-print.kln" "08-print.tm")
(generate (semantic-analysis (parser "klein-programs/08-addition.kln")))



  
  