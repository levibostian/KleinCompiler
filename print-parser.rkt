#lang racket

(provide (all-defined-out))
(require "semantic-actions.rkt"
         "parser.rkt")

(define indent
  (lambda (amt-of-spaces)
    (make-string amt-of-spaces #\space)))
  
(define catch-all
  (lambda (some-struct error amt-of-spaces)
    (cond ((less-than? some-struct)      (print/less-than some-struct amt-of-spaces))
          ((equals? some-struct)         (print/equals some-struct amt-of-spaces))
          ((or (number? some-struct) 
               (boolean~? some-struct))  (print/literal some-struct amt-of-spaces))
          ((or~? some-struct)            (print/or some-struct amt-of-spaces))
          ((if~? some-struct)            (print/if~ some-struct amt-of-spaces))
          ((not? some-struct)            (print/not some-struct amt-of-spaces))
          ((negative-value? some-struct) (print/negative-value some-struct amt-of-spaces))
          ((function-call? some-struct)  (print/function-call some-struct amt-of-spaces))
          ((or (nonemptyactuals? some-struct)
               (nonemptyactuals-prime? some-struct)) (print/actuals some-struct amt-of-spaces))
          ((identifier? some-struct)     (print/identifier some-struct "identifier" amt-of-spaces))
          ((addition? some-struct)       (print/addition some-struct amt-of-spaces))
          ((subtraction? some-struct)    (print/subtraction some-struct amt-of-spaces))
          ((division? some-struct)       (print/division some-struct amt-of-spaces))
          ((multiplication? some-struct) (print/multiplication some-struct amt-of-spaces))
          ((and~? some-struct)           (print/and some-struct amt-of-spaces))
          (else error))))

;;;;;;;;;;;;;;;;;;;;;;;;;PROGRAM;;;;;;;;;;;;;;;;;;;;;;;;;
(define print/program
  (lambda (prog)
    (if (program? prog)
        (string-append "program\n" (print/definitions (program-definitions prog) 4))
         prog)))

(define print/definitions
  (lambda (defin amt-of-spaces)
    (if (def? defin)
        (print/def defin amt-of-spaces)
        (string-append (print/def (definitions-def defin) amt-of-spaces)
                       (print/definitions (definitions-definitions defin) amt-of-spaces)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;DEF;;;;;;;;;;;;;;;;;;;;;;;;;
(define print/def
  (lambda (def~ amt-of-spaces)
    (string-append (indent amt-of-spaces)
                   "function\n"
                   (print/identifier (def-id def~) "name" (+ 4 amt-of-spaces))
;                   (indent (+ 4 amt-of-spaces))
;                   "parameters\n"
                   (print/nonemptyformals (def-formals def~) (+ 4 amt-of-spaces))
                   (print/type (def-type def~) "returns" amt-of-spaces)
                   (print/body (def-body def~) (+ 4 amt-of-spaces)))))
(define print/identifier
  (lambda (ident name amt-of-spaces)
    (string-append (indent amt-of-spaces) name "(" (symbol->string (identifier-value ident)) ") -> " (symbol->string (identifier-type ident)) "\n")))

(define print/empty-formals
  (lambda (formals amt-of-spaces)
    (string-append (indent amt-of-spaces) "no parameters\n")))

(define print/nonemptyformals 
  (lambda (nonempform amt-of-spaces)
    (cond ((formal? nonempform) (string-append (indent amt-of-spaces)
                                               "parameters\n"
                                               (print/formal nonempform (+ 4 amt-of-spaces))))
          ((nonemptyformals? nonempform)(string-append  
                                         (indent (+ 4 amt-of-spaces))
                                         "parameters\n"
                                         (print/formal (nonemptyformals-formal nonempform) (+ 4 amt-of-spaces))
                                         (print/nonemptyformals-prime (nonemptyformals-nonemptyformals nonempform) (+ 4 amt-of-spaces))))
          ((empty-formals? nonempform) (print/empty-formals nonempform amt-of-spaces))
          (else (list "error - nonemptyformals" nonempform)))))
  
(define print/nonemptyformals-prime
  (lambda (nonempform amt-of-spaces)
    (if (formal? nonempform)
        (print/formal nonempform amt-of-spaces)
        (string-append (print/formal (nonemptyformals-formal nonempform) amt-of-spaces)
                       (print/nonemptyformals-prime (nonemptyformals-nonemptyformals nonempform) amt-of-spaces)))))

(define print/formal
  (lambda (form amt-of-spaces)
    (if (nonemptyformals? form)
        (print/nonemptyformals form amt-of-spaces)
        (if (empty-formals? form)
            (print/empty-formals form amt-of-spaces)
            (string-append (print/identifier (formal-id form) "identifier" amt-of-spaces)  
                           (print/type (formal-type form) "type" amt-of-spaces))))))
(define print/type
  (lambda (type~ name amt-of-spaces)
    (string-append (indent (+ 4 amt-of-spaces)) name "(" (symbol->string (type-value type~)) ")\n")))

(define print/body
  (lambda (bod amt-of-spaces)
    (cond ((print-body? bod) (string-append (indent amt-of-spaces)
                                            "body -> " (symbol->string (print-body-type bod)) "\n" 
                                            (print/print~ (print-body-print-expr bod) (+ 4 amt-of-spaces))
                                            "\n"
                                            (print/body (print-body-expr bod) (+ 4 amt-of-spaces))))
          ((body? bod) (string-append (indent amt-of-spaces)
                                      "body -> " (symbol->string (body-type bod)) "\n"
                                      (print/expr (body-expr bod) amt-of-spaces)))
          (else (list "error - body" bod)))))

(define print/print~
  (lambda (prnt amt-of-spaces)
    (string-append (indent amt-of-spaces)
                   "print -> " (symbol->string (print~-type prnt)) "\n" 
                   (print/expr (print~-expr prnt) amt-of-spaces))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;EXPR;;;;;;;;;;;;;;;;;;;;;;;;;
(define print/expr
  (lambda (exp amt-of-spaces)
    (catch-all exp (list "error - expr" exp) (+ 4 amt-of-spaces))))

(define print/less-than
  (lambda (less amt-of-spaces)
    (string-append (indent amt-of-spaces)
                   "less-than_expression -> " (symbol->string (less-than-type less)) "\n"
                   (print/simple-expr (less-than-left less) amt-of-spaces)
                   (indent (+ 4 amt-of-spaces))
                   "<\n"
                   (print/simple-expr (less-than-right less) amt-of-spaces))))

(define print/equals
  (lambda (eq amt-of-spaces)
    (string-append (indent amt-of-spaces)
                   "equals_expression -> " (symbol->string (equals-type eq)) "\n" 
                   (print/simple-expr (equals-left eq) amt-of-spaces)
                   (indent (+ 4 amt-of-spaces))
                   "=\n"
                   (print/simple-expr (equals-right eq) amt-of-spaces))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;SIMP-EXPR;;;;;;;;;;;;;;;;;;;;;;;;;
(define print/simple-expr
  (lambda (simp amt-of-spaces)
    (catch-all simp (list "error - simple-expr" simp) (+ 4 amt-of-spaces))))

(define print/addition
  (lambda (addi amt-of-spaces)
    (string-append (indent amt-of-spaces)
                   "addition_expression -> " (symbol->string (addition-type addi)) "\n"
                   (print/term (addition-left addi) amt-of-spaces)
                   (indent (+ 4 amt-of-spaces))
                   "+\n"
                   (print/term (addition-right addi) amt-of-spaces))))

(define print/subtraction
  (lambda (sub amt-of-spaces)
    (string-append (indent amt-of-spaces)
                   "subtraction_expression -> " (symbol->string (subtraction-type sub)) "\n"
                   (print/term (subtraction-left sub) amt-of-spaces)
                   (indent (+ 4 amt-of-spaces))
                   "-\n"
                   (print/term (subtraction-right sub) amt-of-spaces))))

(define print/or
  (lambda (or~ amt-of-spaces)
    (string-append (indent amt-of-spaces)
                   "or_expression -> " (symbol->string (or~-type or~)) "\n"
                   (print/term (or~-left or~) amt-of-spaces)
                   (indent (+ 4 amt-of-spaces))
                   "or\n"
                   (print/term (or~-right or~) amt-of-spaces))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;TERM;;;;;;;;;;;;;;;;;;;;;;;;;
(define print/term
  (lambda (ter amt-of-spaces)
    (catch-all ter (list "error - term" ter) (+ 4 amt-of-spaces))))
(define print/multiplication
  (lambda (multi amt-of-spaces)
    (string-append (indent amt-of-spaces)
                   "multiplication_expression -> " (symbol->string (multiplication-type multi)) "\n"
                   (print/factor (multiplication-left multi) amt-of-spaces)
                   (indent (+ 4 amt-of-spaces))
                   "*\n"
                   (print/factor (multiplication-right multi) amt-of-spaces))))
(define print/division
  (lambda (div amt-of-spaces)
    (string-append (indent amt-of-spaces)
                   "division_expression -> " (symbol->string (division-type div)) "\n"
                   (print/factor (division-left div) amt-of-spaces)
                   (indent (+ 4 amt-of-spaces))
                   "/\n"
                   (print/factor (division-right div) amt-of-spaces))))
(define print/and
  (lambda (& amt-of-spaces)
    (string-append (indent amt-of-spaces)
                   "and_expression -> " (symbol->string (and~-type &)) "\n"
                   (print/factor (and~-left &) amt-of-spaces)
                   (indent amt-of-spaces)
                   "and\n"
                   (print/factor (and~-right &) amt-of-spaces))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;FACTOR;;;;;;;;;;;;;;;;;;;;;;;;;
(define print/factor
  (lambda (fact amt-of-spaces)
    (catch-all fact (list "error - factor" fact) (+ 4 amt-of-spaces))))

(define print/not
  (lambda (not~ amt-of-spaces)
    (string-append (indent amt-of-spaces)
                   "not_expression -> " (symbol->string (not-type not~)) "\n" 
                   (print/factor (not-value not~) amt-of-spaces))))

(define print/function-call
  (lambda (func-call amt-of-spaces)
    (string-append (indent amt-of-spaces)
                   "function_call -> " (symbol->string (function-call-type func-call)) "\n" 
                   (print/identifier (function-call-name func-call) "function_name" (+ 4 amt-of-spaces))
                   (print/actuals (function-call-actuals func-call) (+ 4 amt-of-spaces)))))

(define print/literal
  (lambda (lit amt-of-spaces)
    (if (boolean~? lit)
        (string-append (indent amt-of-spaces) 
                       "literal(" 
                       (symbol->string (boolean~-value lit)) 
                       ") -> " (symbol->string (boolean~-type lit)) "\n")
        (string-append (indent amt-of-spaces) 
                       "literal(" 
                       (symbol->string (number-value lit)) 
                       ") -> " (symbol->string (number-type lit)) "\n"))))

(define print/negative-value
  (lambda (neg-val amt-of-spaces)
    (string-append (indent amt-of-spaces)
                   "negative_expression -> " (symbol->string (negative-value-type neg-val)) "\n"
                   (print/factor (negative-value-value neg-val) (+ 4 amt-of-spaces))
                   "\n")))

(define print/if~
  (lambda (iffy amt-of-spaces)
    (string-append (indent amt-of-spaces)
                   "if_expression -> " (symbol->string (if~-type iffy)) "\n"
                   (indent (+ 4 amt-of-spaces))
                   "if\n"
                   (print/expr (if~-test iffy) (+ 4 amt-of-spaces))
                   (indent (+ 4 amt-of-spaces))
                   "then\n"
                   (print/expr (if~-then iffy) (+ 4 amt-of-spaces))
                   (indent (+ 4 amt-of-spaces))
                   "else\n"
                   (print/expr (if~-else iffy) (+ 4 amt-of-spaces)))))

(define print/actuals
  (lambda (acts amt-of-spaces)
    (cond ((nonemptyactuals? acts)
           (string-append (indent amt-of-spaces)
                          "actuals\n" 
                          (catch-all (nonemptyactuals-expr acts) 
                                     (list "error - actuals" acts) 
                                     (+ 4 amt-of-spaces)) ))
        ((nonemptyactuals-prime? acts)
         (string-append (catch-all (nonemptyactuals-prime-expr acts)
                                   (list "error - actuals" acts) 
                                   amt-of-spaces)
                        (print/actuals (nonemptyactuals-prime-nonemptyactuals acts) amt-of-spaces)
                        ))
        ((empty-actuals? acts) (string-append (indent amt-of-spaces)
                                              "actuals()\n"))
        (else (catch-all acts (list "error - actuals" acts) (+ 4 amt-of-spaces))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(display (print/program (semantic-analysis (parser "klein-programs/08-addition.kln"))))
;(display (print/program (parser "klein-programs/circular-prime.kln")))
;(display (print/program (parser "klein-programs/test.kln")))
;(display (print/program (parser "klein-programs/euclid.kln")))
;(display (print/program (parser "klein-programs/horner.kln")))
;(display (print/program (parser "klein-programs/circular-prime.kln")))
;(display (print/program (parser "klein-programs/farey.kln")))
;(display (print/program (parser "klein-programs/fibonacci.kln")))
;(display (print/program (parser "klein-programs/horner-parameterized.kln")))
