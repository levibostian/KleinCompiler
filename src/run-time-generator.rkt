#lang racket

;it ain't pretty, but it works :)
;(Could be cleaned up very easily just didn't have time)

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
  (lambda (line-num symbol-table)
    (let ((generated-args (generate-function-args line-num)))
      (let ((line-num (length (generate-function-args line-num))))
            (append (generate-function-args line-num)
                    (list (string-append (number->string line-num)       ": LDA 6,3(7)\n")
                          (string-append (number->string (+ 1 line-num)) (format ": LDC 5,~a(0)\n" (+ 1 (hash-ref (hash-ref symbol-table 'main) 'amt-of-params))))
                          (string-append (number->string (+ 2 line-num)) ": LDC 4,1(0)\n")
                          (list (string-append (number->string (+ 3 line-num)) ": LDA 7,~a(0)\n")
                                (symbol-table-lookup 'main)) ; note this for future reference for functions
                          (string-append (number->string (+ 4 line-num)) ":  LD 1,0(3)\n")
                          (string-append (number->string (+ 5 line-num)) ": OUT 1,0,0\n" )
                          (string-append (number->string (+ 6 line-num)) ":HALT 0,0,0\n" ) ))))))

(define generate ; make sure to check for errors from AST
  (lambda (ast)  ; find way to keep track of top of call stack
    (let ((symbol-table (symbol-table ast)))
      (letrec ((generate-everything
                (lambda (ast cur-func-name line-num)
                  (cond 
                    ((program? ast)     (let ((generated-runtime (generate-runtime line-num symbol-table)))
                                          (let ((line-num (length generated-runtime)))
                                            (append generated-runtime
                                                    (generate-everything (program-definitions ast) (+ cur-func-name 1) line-num)))))
                    ((definitions? ast) (let ((generated-def (generate-everything (definitions-def ast) cur-func-name line-num)))
                                          (let ((line-num (+ line-num (length generated-def))))
                                            (append generated-def
                                                    (generate-everything (definitions-definitions ast) cur-func-name line-num)))))
                    ((def? ast)         (let ((generated-body (generate-everything (def-body ast) 
                                                                                   (identifier-value (def-id ast))
                                                                                   (+ 5 line-num))))
                                          (let ((line-num-after-body (+ (+ 5 line-num) (length generated-body))))          
                                            (hash-set! (hash-ref symbol-table (identifier-value (def-id ast))) 'tm-line line-num)
                                            (append (list (string-append "* " (symbol->string (identifier-value (def-id ast))) "\n")) ; add def to symbol table, branch number
                                                    (list (string-append (number->string line-num)       ": ADD 3,5,0\n"))
                                                    (list (string-append (number->string (+ 1 line-num)) (format ": LDC 1,~a(0)\n" (+ 1 (hash-ref (hash-ref symbol-table (identifier-value (def-id ast))) 'amt-of-params)))))
                                                    (list (string-append (number->string (+ 2 line-num)) ": SUB 3,3,1\n"))
                                                    (list (string-append (number->string (+ 3 line-num)) ":  ST 6,0(5)\n"))
                                                    (list (string-append (number->string (+ 4 line-num)) ": ADD 5,4,5\n"))
                                                    generated-body
                                                    (list (string-append (number->string line-num-after-body) ":  ST 1,0(3)\n"))
                                                    (list (string-append (number->string (+ 1 line-num-after-body)) ": ADD 5,3,0\n"))
                                                    ;(list (string-append (number->string (+ 2 line-num-after-body)) (format ":  LD 6,~a(3)\n" (+ 1 (hash-ref (hash-ref symbol-table (identifier-value (def-id ast))) 'amt-of-params)))))
                                                    (list (string-append (number->string (+ 2 line-num-after-body)) ": LDA 7,0(6)\n")))))) ; use the let implementation to find out length of whole function
                    ((print-body? ast)  (let ((generated-print-expr (generate-everything (print-body-print-expr ast) cur-func-name line-num)))
                                          (let ((line-num-after-print-expr (+ line-num (length generated-print-expr))))
                                            (let ((generated-print (generate-print generated-print-expr line-num-after-print-expr cur-func-name)))
                                              (let ((line-num-after-print (+ line-num (length generated-print))))
                                                (append  generated-print
                                                         (generate-everything (print-body-expr ast) cur-func-name line-num-after-print)))))))
                    ((body? ast)        (append (generate-everything (body-expr ast) cur-func-name line-num)))
                    ((print~? ast)      (append (generate-everything (print~-expr ast) cur-func-name line-num)))
                    ((addition? ast)    (let ((generated-left-expr (generate-everything (addition-left ast) cur-func-name line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-right-expr (generate-everything (addition-right ast) cur-func-name line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (generate-addition generated-left-expr generated-right-expr line-num))))))
                    ((subtraction? ast) (let ((generated-left-expr (generate-everything (subtraction-left ast) cur-func-name line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-right-expr (generate-everything (subtraction-right ast) cur-func-name line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (generate-subtraction generated-left-expr generated-right-expr line-num))))))
                    ((multiplication? ast) (let ((generated-left-expr (generate-everything (multiplication-left ast) cur-func-name line-num)))
                                             (let ((line-num (+ line-num (length generated-left-expr))))
                                               (let ((generated-right-expr (generate-everything (multiplication-right ast) cur-func-name line-num)))
                                                 (let ((line-num (+ line-num (length generated-right-expr))))
                                                   (generate-multiplication generated-left-expr generated-right-expr line-num))))))
                    ((division? ast)    (let ((generated-left-expr (generate-everything (division-left ast) cur-func-name line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-right-expr (generate-everything (division-right ast) cur-func-name line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (generate-division generated-left-expr generated-right-expr line-num))))))
                    ((negative-value? ast) (let ((generated-neg-val-expr (generate-everything (negative-value-value ast) cur-func-name line-num)))
                                             (let ((line-num (+ line-num (length generated-neg-val-expr))))
                                               (generate-negative-value generated-neg-val-expr line-num))))
                    ((and~? ast)        (let ((generated-left-expr (generate-everything (and~-left ast) cur-func-name line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-right-expr (generate-everything (and~-right ast) cur-func-name line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (generate-and~ generated-left-expr generated-right-expr line-num))))))
                    ((or~? ast)         (let ((generated-left-expr (generate-everything (or~-left ast) cur-func-name line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-right-expr (generate-everything (or~-right ast) cur-func-name line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (generate-or~ generated-left-expr generated-right-expr line-num))))))
                    ((equals? ast)      (let ((generated-left-expr (generate-everything (equals-left ast) cur-func-name line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-right-expr (generate-everything (equals-right ast) cur-func-name line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (generate-equals generated-left-expr generated-right-expr line-num))))))
                    ((less-than? ast)   (let ((generated-left-expr (generate-everything (less-than-left ast) cur-func-name line-num)))
                                          (let ((line-num (+ line-num (length generated-left-expr))))
                                            (let ((generated-right-expr (generate-everything (less-than-right ast) cur-func-name line-num)))
                                              (let ((line-num (+ line-num (length generated-right-expr))))
                                                (generate-less-than generated-left-expr generated-right-expr line-num))))))
                    ((not? ast)         (let ((generated-not-expr (generate-everything (not-value ast) cur-func-name line-num)))
                                          (let ((line-num (+ line-num (length generated-not-expr))))
                                            (generate-not generated-not-expr line-num))))
                    ((number? ast)      (append (generate-number (number-value ast) cur-func-name line-num)))
                    ((boolean~? ast)    (append (generate-boolean (boolean~-value ast) cur-func-name line-num)))
                    ((identifier? ast)  (append (generate-identifier (identifier-value ast) cur-func-name line-num (hash-ref symbol-table cur-func-name))))
                    ((if~? ast)         (let ((generated-test-expr (generate-everything (if~-test ast) cur-func-name line-num)))
                                          (let ((line-num (+ line-num (length generated-test-expr))))
                                            (let ((generated-test-expr (append generated-test-expr
                                                                               (list (string-append (number->string line-num) ": SUB 5,5,4\n"))
                                                                               (list (lambda (offset) 
                                                                                       (string-append (number->string (+ 1 line-num)) (format ": JEQ 1,~a(7)\n" offset)))))))
                                              (let ((line-num (+ line-num 2)))
                                                (let ((generated-then-expr (generate-everything (if~-then ast) cur-func-name line-num)))
                                                  (let ((line-num (+ line-num (length generated-then-expr))))
                                                    (let ((generated-then-expr (append generated-then-expr
                                                                                       (list (lambda (offset) 
                                                                                               (string-append (number->string line-num) (format ": JEQ 0,~a(7)\n" offset)))))))
                                                      (let ((length-then (length generated-then-expr)))
                                                        (let ((line-num (+ line-num 1)))
                                                          (let ((generated-else-expr (generate-everything (if~-else ast) cur-func-name line-num)))
                                                            (let ((line-num (+ line-num (length generated-else-expr))))
                                                              (let ((length-else (length generated-else-expr)))
                                                                (generate-if generated-test-expr 
                                                                             generated-then-expr 
                                                                             generated-else-expr 
                                                                             line-num
                                                                             length-then
                                                                             length-else))))))))))))))
                    ((nonemptyactuals-prime? ast) (let ((generated-actual (generate-everything (nonemptyactuals-prime-expr ast) cur-func-name line-num)))
                                                    (let ((line-num (+ line-num (length generated-actual))))
                                                      (let ((generated-actuals (generate-everything (nonemptyactuals-prime-nonemptyactuals ast) cur-func-name line-num)))
                                                        (let ((line-num (+ line-num (length generated-actuals))))
                                                          (append generated-actual
                                                                  generated-actuals))))))
                    ((nonemptyactuals? ast)        (append (generate-everything (nonemptyactuals-expr ast) cur-func-name line-num)))
                    ((empty-actuals? ast)          (list))
                    ((function-call? ast)        (if (equal? cur-func-name (identifier-value (function-call-name ast)))
                                                     (let ((generated-actuals (generate-everything (function-call-actuals ast) cur-func-name (+ 1 line-num))))
                                                       (let ((line-num-after-actuals (+ (+ 1 line-num) (length generated-actuals))))
                                                         (append (list (string-append (number->string line-num) ":  ST 5,0(3) store 5 location out to 3, then generate actuals\n"))
                                                                 generated-actuals
                                                                 (list (string-append (number->string line-num-after-actuals) ":  LD 5,0(3) set 5 back\n"))
                                                                 (let ((load-actuals (load-actuals-as-args (+ 1 line-num-after-actuals)
                                                                                                           (hash-ref (hash-ref symbol-table cur-func-name) 'amt-of-params)
                                                                                                           0)))
                                                                   (let ((line-num-after-actuals (+ (+ 1 line-num-after-actuals) (length load-actuals))))
                                                                     (append load-actuals
                                                                             (list (string-append (number->string line-num-after-actuals) ":  LD 5,0(3)\n")
                                                                                   (string-append (number->string (+ 1 line-num-after-actuals)) (format ": LDC 1,~a(0)\n" (hash-ref (hash-ref symbol-table cur-func-name) 'amt-of-params)))
                                                                                   (string-append (number->string (+ 2 line-num-after-actuals)) ": ADD 5,1,5\n")
                                                                                   (list (string-append (number->string (+ 3 line-num-after-actuals)) ": LDA 7,~a(0)\n")
                                                                                         (symbol-table-lookup (identifier-value (function-call-name ast)))))))))))
                                                     (let ((generated-actuals (generate-everything (function-call-actuals ast) cur-func-name (+ 4 line-num))))
                                                       (let ((line-num-after-actuals (+ (+ 4 line-num) (length generated-actuals))))
                                                         (append (append 
                                                                  (list (string-append (number->string line-num)       ":  ST 3,0(5) start of function call\n")
                                                                        (string-append (number->string (+ 1 line-num)) ": ADD 5,4,5\n")
                                                                        ;(string-append (number->string (+ 2 line-num)) ": ADD 3,0,5\n") moved down
                                                                        (string-append (number->string (+ 2 line-num)) ": ADD 5,4,5\n")
                                                                        (string-append (number->string (+ 3 line-num)) (format ": LDC 6,~a(0)\n" (+ 2 line-num-after-actuals))))
                                                                  generated-actuals)
                                                                 (list (string-append (number->string line-num-after-actuals) (format ": LDC 6,~a(0)\n" (+ 2 line-num-after-actuals)))
                                                                       (list (string-append (number->string (+ 1 line-num-after-actuals)) ": LDA 7,~a(0)\n")
                                                                             (symbol-table-lookup (identifier-value (function-call-name ast))))
                                                                       (string-append (number->string (+ 2 line-num-after-actuals)) ": ADD 3,0,5\n")
                                                                       (string-append (number->string (+ 3 line-num-after-actuals)) ":  LD 3,-1(5)\n")
                                                                       (string-append (number->string (+ 4 line-num-after-actuals)) ":  LD 1,0(5)\n")
                                                                       (string-append (number->string (+ 5 line-num-after-actuals)) ":  ST 1,-1(5)\n")
                                                                       ;(string-append (number->string (+ 6 line-num-after-actuals)) ":  ST 1,0(3)\n")
                                                                       (string-append (number->string (+ 6 line-num-after-actuals))
                                                                                      (format ":  LD 6,~a(3) end of function call\n" (+ 1 (hash-ref (hash-ref symbol-table cur-func-name) 'amt-of-params))))
                                                                       ))))))
                    ;(else (list "NOTHING MATCHED IN generate FUNCTION" ast))
                    ))))
        (if (error-ast? ast)
            ast
            (printf (create-tm-string (generate-everything ast 1 0) symbol-table)))) )));check for parser error

(define load-actuals-as-args
  (lambda (line-num amt-of-actuals count)
    (if (equal? amt-of-actuals count)
        '()
        (append (list (string-append (number->string line-num) (format ":  LD 2,~a(5) ..loading actual\n" count))
                      (string-append (number->string (+ 1 line-num)) (format ":  ST 2,~a(3) ..storing actual\n" (+ 1 count))))
                (load-actuals-as-args (+ 2 line-num) amt-of-actuals (+ 1 count))))))
        
    
    
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
            (list ;(string-append (number->string line-num)       ":  LD 1,-1(5) print starts here\n")
                  (string-append (number->string  line-num) ": OUT 1,0,0 print starts here\n")
                  (string-append (number->string (+ 1 line-num)) ": SUB 5,5,4\n")))))


(define generate-addition
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)       ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num)) ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num)) ": ADD 1,1,2\n")
                  (string-append (number->string (+ 3 line-num)) ": SUB 5,5,4\n")
                  (string-append (number->string (+ 4 line-num)) ": SUB 5,5,4\n")
                  (string-append (number->string (+ 5 line-num)) ":  ST 1,0(5)\n")
                  (string-append (number->string (+ 6 line-num)) ": ADD 5,4,5\n")))))

(define generate-subtraction
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)       ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num)) ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num)) ": SUB 1,1,2\n")
                  (string-append (number->string (+ 3 line-num)) ": SUB 5,5,4\n")
                  (string-append (number->string (+ 4 line-num)) ": SUB 5,5,4\n")
                  (string-append (number->string (+ 5 line-num)) ":  ST 1,0(5)\n")
                  (string-append (number->string (+ 6 line-num)) ": ADD 5,4,5\n")))))

(define generate-multiplication
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)       ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num)) ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num)) ": MUL 1,1,2\n")
                  (string-append (number->string (+ 3 line-num)) ": SUB 5,5,4\n")
                  (string-append (number->string (+ 4 line-num)) ": SUB 5,5,4\n")
                  (string-append (number->string (+ 5 line-num)) ":  ST 1,0(5)\n")
                  (string-append (number->string (+ 6 line-num)) ": ADD 5,4,5\n")))))

(define generate-division
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)       ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num)) ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num)) ": DIV 1,1,2\n")
                  (string-append (number->string (+ 3 line-num)) ": SUB 5,5,4\n")
                  (string-append (number->string (+ 4 line-num)) ": SUB 5,5,4\n")
                  (string-append (number->string (+ 5 line-num)) ":  ST 1,0(5)\n")
                  (string-append (number->string (+ 6 line-num)) ": ADD 5,4,5\n")))))

(define generate-negative-value
  (lambda (neg-val-expr line-num)
    (append neg-val-expr
            (list (string-append (number->string line-num)       ":  LD 1,-1(5)\n")
                  (string-append (number->string (+ 1 line-num)) ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num)) ": SUB 1,1,2\n")
                  (string-append (number->string (+ 3 line-num)) ": SUB 1,1,2\n")
                  (string-append (number->string (+ 4 line-num)) ": SUB 5,5,4\n")
                  (string-append (number->string (+ 5 line-num)) ":  ST 1,0(5)\n")
                  (string-append (number->string (+ 6 line-num)) ": ADD 5,4,5\n")))))

(define generate-and~;optimize?
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)        ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num))  ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 3 line-num))  ": SUB 5,5,4\n"  )
                  (string-append (number->string (+ 4 line-num))  ": SUB 5,5,4\n"  )
                  (string-append (number->string (+ 5 line-num))  ": JNE 1,0(7)\n" )
                  (string-append (number->string (+ 6 line-num))  ": JNE 2,1(7)\n" )
                  (string-append (number->string (+ 7 line-num))  ": JEQ 0,4(7)\n" )
                  (string-append (number->string (+ 8 line-num))  ": LDC 1,1(0)\n" );can maybe get rid of!
                  (string-append (number->string (+ 9 line-num))  ":  ST 1,0(5)\n" )
                  (string-append (number->string (+ 10 line-num)) ": ADD 5,4,5\n"  )
                  (string-append (number->string (+ 11 line-num)) ": JEQ 0,3(7)\n" )
                  (string-append (number->string (+ 12 line-num)) ":  ST 0,0(5)\n" )
                  (string-append (number->string (+ 13 line-num)) ": LDC 1,0(0)\n" )
                  (string-append (number->string (+ 14 line-num)) ": ADD 5,4,5\n"  ) ))))

(define generate-or~
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)        ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num))  ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num))  ": SUB 5,5,4\n"  )
                  (string-append (number->string (+ 3 line-num))  ": SUB 5,5,4\n"  )
                  (string-append (number->string (+ 4 line-num))  ": ADD 1,1,2\n"  )
                  (string-append (number->string (+ 5 line-num))  ": JNE 1,4(7)\n" )
                  (string-append (number->string (+ 6 line-num))  ":  ST 0,0(5)\n" )
                  (string-append (number->string (+ 7 line-num))  ": LDC 1,0(0)\n" )
                  (string-append (number->string (+ 8 line-num))  ": ADD 5,4,5\n"  )
                  (string-append (number->string (+ 9 line-num))  ": JEQ 0,3(7)\n" )
                  (string-append (number->string (+ 10 line-num)) ": LDC 1,1(0)\n" );can maybe get rid of!
                  (string-append (number->string (+ 11 line-num)) ":  ST 1,0(5)\n" )
                  (string-append (number->string (+ 12 line-num)) ": ADD 5,4,5\n"  ) ))))

(define generate-equals
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)        ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num))  ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num))  ": SUB 5,5,4\n"  )
                  (string-append (number->string (+ 3 line-num))  ": SUB 5,5,4\n"  )
                  (string-append (number->string (+ 4 line-num))  ": SUB 1,1,2\n"  )
                  (string-append (number->string (+ 5 line-num))  ": JEQ 1,4(7)\n" )
                  (string-append (number->string (+ 6 line-num))  ":  ST 0,0(5)\n" )
                  (string-append (number->string (+ 7 line-num))  ": LDC 1,0(0)\n" )
                  (string-append (number->string (+ 8 line-num))  ": ADD 5,4,5\n"  )
                  (string-append (number->string (+ 9 line-num))  ": JEQ 0,3(7)\n" )
                  (string-append (number->string (+ 10 line-num)) ": LDC 1,1(0)\n" );can maybe get rid of!
                  (string-append (number->string (+ 11 line-num)) ":  ST 1,0(5)\n" )
                  (string-append (number->string (+ 12 line-num)) ": ADD 5,4,5\n"  ) ))))

(define generate-less-than
  (lambda (left-expr right-expr line-num)
    (append left-expr
            right-expr
            (list (string-append (number->string line-num)        ":  LD 1,-2(5)\n")
                  (string-append (number->string (+ 1 line-num))  ":  LD 2,-1(5)\n")
                  (string-append (number->string (+ 2 line-num))  ": SUB 5,5,4\n"  )
                  (string-append (number->string (+ 3 line-num))  ": SUB 5,5,4\n"  )
                  (string-append (number->string (+ 4 line-num))  ": SUB 1,2,1\n"  )
                  (string-append (number->string (+ 5 line-num))  ": JGT 1,4(7)\n" )
                  (string-append (number->string (+ 6 line-num))  ":  ST 0,0(5)\n" )
                  (string-append (number->string (+ 7 line-num))  ": LDC 1,0(0)\n" )
                  (string-append (number->string (+ 8 line-num))  ": ADD 5,4,5\n"  )
                  (string-append (number->string (+ 9 line-num))  ": JEQ 0,3(7)\n" )
                  (string-append (number->string (+ 10 line-num)) ": LDC 1,1(0)\n" );can maybe get rid of!
                  (string-append (number->string (+ 11 line-num)) ":  ST 1,0(5)\n" )
                  (string-append (number->string (+ 12 line-num)) ": ADD 5,4,5\n"  ) ))))


(define generate-not
  (lambda (not-expr line-num)
    (append not-expr
            (list (string-append (number->string line-num)        ":  LD 1,-1(5)\n")
                  (string-append (number->string (+ 1 line-num))  ": SUB 5,5,4\n"  )
                  (string-append (number->string (+ 2 line-num))  ": JNE 1,4(7)\n" )
                  (string-append (number->string (+ 3 line-num))  ": LDC 1,1(0)\n" );can maybe get rid of!
                  (string-append (number->string (+ 4 line-num))  ":  ST 1,0(5)\n" )
                  (string-append (number->string (+ 5 line-num))  ": ADD 5,4,5\n"  )
                  (string-append (number->string (+ 6 line-num))  ": JEQ 0,3(7)\n" )
                  (string-append (number->string (+ 7 line-num))  ":  ST 0,0(5)\n" )
                  (string-append (number->string (+ 8 line-num))  ": LDC 1,0(0)\n" )
                  (string-append (number->string (+ 9 line-num))  ": ADD 5,4,5\n"  ) ))))

(define fillin-offset
  (lambda (offset)
    (lambda (item)
      (if (procedure? item)
          (item offset)
          item))))
(define generate-if
  (lambda (test then else line-num len-then len-else)
    (append (map (fillin-offset len-then) test)
            (map (fillin-offset len-else) then)
            else)))

(define find-pos-of-item-in-list
  (lambda (item lyst)
    (find-pos-of-item-in-list-helper item lyst 1)))
(define find-pos-of-item-in-list-helper
  (lambda (item lyst accum)
    (if (or (null? lyst) (eq? item (car lyst)))
        accum
        (find-pos-of-item-in-list-helper item (cdr lyst) (+ 1 accum)))))

(define generate-identifier
  (lambda (ident cur-func-params line-num param-hash)
    (append 
     (list (string-append (number->string line-num) (format ":  LD 1,~a(3)\n" (+ 1 (hash-ref (hash-ref param-hash 'parameters) (string->symbol (string-append (symbol->string ident) "~"))))))
           (string-append (number->string (+ 1 line-num)) ":  ST 1,0(5)\n")
           (string-append (number->string (+ 2 line-num)) ": ADD 5,4,5\n") ))))

;(define generate-func-call
;  (lambda (func-call line-num symbol-table)
;    (append 
;     (list (string-append (number->string line-num)       ":  ST 3,0(5)\n")
;           (string-append (number->string (+ 1 line-num)) ": ADD 5,4,5\n")
;           (string-append (number->string (+ 2 line-num)) ": ADD 3,0,5\n")
;           (string-append (number->string (+ 3 line-num)) ": ADD 5,4,5\n")
;           (generate-actuals (function-call-actuals func-call))
;           (string-append (number->string (+ 

  
(define generate-actuals
  (lambda (actuals)
    (append 
     (cond 
       ((nonemptyactuals? actuals)       (list (nonemptyactuals-expr actuals)))
       ((nonemptyactuals-prime? actuals) (append (generate-actuals (nonemptyactuals-prime-expr actuals))
                                                 (generate-actuals (nonemptyactuals-prime-nonemptyactuals actuals))))
        ((empty-actuals? actuals) (list))))))

(define eval-tm-lines
  (lambda (symbol-table)
    (lambda (line)
      (if (and (list? line) (eqv? (length line) 2))
          (format (car line) (hash-ref ((cadr line) symbol-table) 'tm-line))
          line))))

(define create-tm-string
  (lambda (list-of-tm-lines symbol-table)
    (string-join (flatten (map (eval-tm-lines symbol-table)
                      list-of-tm-lines))
                 "")))
  
(define write-out
  (lambda (file-name tm-file-name)
    (with-output-to-file tm-file-name
      (lambda () (generate (semantic-analysis (parser file-name))))
      #:exists 'replace)))

;(write-out "klein-programs/08-print.kln" "08-print.tm")
(generate (semantic-analysis (parser "factorial.kln")))
;(parser "klein-programs/08-addition.kln")
;(generate (semantic-analysis (parser "klein-programs/08-addition.kln")))





  
  