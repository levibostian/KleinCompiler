#lang racket

(require "semantic-actions.rkt"
         "parser.rkt")


(define print/identifier
  (lambda (ident name)
    (string-append name "(" (symbol->string (identifier-value ident)) ")\n")))

(define print/type
  (lambda (type~ name)
    (string-append name "(" (symbol->string (type-value type~)) ")\n")))

(define print/formal
  (lambda (form)
    (if (nonemptyformals? form)
        (print/nonemptyformals form)
        (string-append (print/identifier (formal-id form) "    identifier")  
                       (print/type (formal-type form) "        type")))))

(define print/nonemptyformals 
  (lambda (nonempform)
    (if (formal? nonempform)
        (print/formal nonempform)
        (string-append "parameters\n" 
                       (print/formal (nonemptyformals-formal nonempform))
                       (print/nonemptyformals (nonemptyformals-nonemptyformals nonempform))))))
                   

(define print/def
  (lambda (def~)
    (string-append (print/identifier (def-id def~) "name")
                   (print/formal (def-formals def~))
                   (print/type (def-type def~) "returns")
                   (print/body (def-body def~)))))

(define print/definitions
  (lambda (defin)
    (if (def? defin)
        (print/def defin)
        (string-append (print/def (definitions-def defin))
                       (print/definitions (definitions-definitions defin))))))

(define print/program
  (lambda (prog)
    (string-append (print/definitions (program-definitions prog)))))




(define print/literal
  (lambda (lit)
    (if (boolean~? lit)
        (string-append "literal(" (symbol->string (boolean~-value lit)) ")")
        (string-append "literal(" (symbol->string (number-value lit)) ")"))))

(define print/not
  (lambda (not~)
    (string-append "not\n" "    " (print/factor (not-value not~)))))

(define print/function-call
  (lambda (func-call)
    (string-append "function_call\n" (print/identifier (function-call-name func-call) "         function_name")
                   "\n    " (print/actuals (function-call-actuals func-call)))))

(define print/print~
  (lambda (prnt)
    (string-append "print\n    " (print~-expr prnt))))

(define print/equals
  (lambda (eq)
    (string-append "equals\n    " 
                   (print/simple-expr (equals-left eq))
                   "\n    ="
                   (print/simple-expr (equals-right eq))
                   "\n")))

(define print/less-than
  (lambda (less)
    (string-append "less-than\n    "
                   (print/simple-expr (less-than-left less))
                   "\n    <\n    "
                   (print/simple-expr (less-than-right less))
                   "\n")))
(define print/multiplication
  (lambda (multi)
    (string-append "multiplication\n    "
                   (print/factor (multiplication-left multi))
                   "\n    *\n"
                   (print/factor (multiplication-right multi))
                   "\n")))
(define print/division
  (lambda (div)
    (string-append "division\n    "
                   (print/factor (division-left div))
                   "\n    /\n    "
                   (print/factor (division-right div))
                   "\n")))
(define print/and
  (lambda (&)
    (string-append "and\n    "
                   (print/factor (and~-left &))
                   "\n    and\n    "
                   (print/factor (and~-right &))
                   "\n")))

(define print/addition
  (lambda (addi)
    (string-append "addition\n    "
                   (print/term (addition-left addi))
                   "\n    +\n    "
                   (print/term (addition-right addi))
                   "\n")))

(define print/subtraction
  (lambda (sub)
    (string-append "subtraction\n    "
                   (print/term (subtraction-left sub))
                   "\n    -\n    "
                   (print/term (subtraction-right sub))
                   "\n")))

(define print/or
  (lambda (or~)
    (string-append "or_expression\n        "
                   (print/term (or~-left or~))
                   "\n        or\n        "
                   (print/term (or~-right or~))
                   "\n")))

(define print/factor
  (lambda (fact)
    (cond ((if~? fact) (print/if~ fact))
          ((not? fact) (print/not fact))
          ((or (boolean~? fact)
               (number? fact)) (print/literal fact))
          ((negative-value? fact) (print/negative-value fact))
          ((function-call? fact) (print/function-call fact))
          (else (list "error - factor" fact)))))

(define print/negative-value
  (lambda (neg-val)
    (string-append "negative\n    "
                   (print/factor (negative-value neg-val))
                   "\n")))

(define print/actuals
  (lambda (acts)
    (cond ((nonemptyactuals? acts)
           (string-append "actuals\n        " (print/expr (nonemptyactuals-expr acts)) ))
        ((nonemptyactuals-prime? acts) 
         (string-append (print/expr (nonemptyactuals-prime-expr acts))
                        "\n        "
                        (print/actuals (nonemptyactuals-prime-nonemptyactuals acts))
                        ))
        ((empty-actuals? acts) (string-append "actuals()\n"))
        ((number? acts) (print/literal acts))
        ((identifier? acts) (print/identifier acts))
        (else (list "error - actuals" acts)))))
                        

(define print/expr
  (lambda (exp)
    (catch-all exp)

          

(define print/simple-expr
  (lambda (simp)
    (cond ((or~? simp) (print/or simp))
          ((addition? simp) (print/addition simp))
          ((subtraction? simp) (print/subtraction simp))
          ((or (number? simp) (boolean~? simp)) (print/literal simp))
          ((if~? simp) (print/if~ simp))
          ((not? simp) (print/not simp))
          ((negative-value? simp) (print/negative-value simp))
          ((function-call? simp) (print/function-call simp))
          ((or (nonemptyactuals? simp) 
               (nonemptyactuals-prime? simp)) (print/actuals simp))
          ((identifier? simp) (print/identifier simp "simp_exp_name"))

          (else (list "error - simple-expr" simp)))))

(define print/term
  (lambda (ter)
    (cond ((multiplication? ter) (print/multiplication ter))
          ((division? ter) (print/division ter))
          ((and~? ter) (print/and ter))
          ((or (number? ter) (boolean~? ter)) (print/literal ter))
          ((if~? ter) (print/if~ ter))
          ((not? ter) (print/not ter))
          ((negative-value? ter) (print/negative-value ter))
          ((function-call? ter) (print/function-call ter))
          ((or (nonemptyactuals? ter) 
               (nonemptyactuals-prime? ter)) (print/actuals ter))
          (else (list "error - term" ter)))))
(define print/body
  (lambda (bod)
    (cond ((print-body? bod) (string-append "body\n    " 
                                            (print/print~ (print-body-print-expr bod))
                                            "\n"
                                            (print/body (print-body-expr bod))))
          ((body? bod) (string-append "body\n    "
                                      (print/expr (body-expr bod))))
          (else (list "error - body" bod)))))

(define print/if~
  (lambda (iffy)
    (string-append "if_expression\n    "
                   "if\n    "
                   (print/expr (if~-test iffy))
                   "then\n    "
                   (print/expr (if~-then iffy))
                   "else\n    "
                   (print/expr (if~-else iffy))
                   "\n")))
          

                               
  
  
(printf (print/program (parser "klein-programs/circular-prime.kln")))

  
  
(define catch-call
  (lambda (some-struct)
    (cond ((less-than? some-struct) (print/less-than some-struct))
          ((equals? some-struct) (print/equals some-struct))
          ((or (number? some-struct) (boolean~? some-struct)) (print/literal some-struct))
          ((or~? some-struct) (print/or some-struct))
          ((if~? some-struct) (print/if~ some-struct))
          ((not? some-struct) (print/not some-struct))
          ((negative-value? some-struct) (print/negative-value some-struct))
          ((function-call? some-struct) (print/function-call some-struct))
          ((or (nonemptyactuals? some-struct) 
               (nonemptyactuals-prime? some-struct)) (print/actuals some-struct))
          ((identifier? some-struct) (print/identifier some-struct "identifier"))
          (else (list "error - something wrong!" some-struct)))))

  
  
;  (printf (print/nonemptyformals (make-nonemptyformals (make-formal (make-identifier 'x) (make-type 'integer))
;                                               (make-nonemptyformals
;                                                (make-formal (make-identifier 'y) (make-type 'integer))
;                                                (make-formal (make-identifier 'z) (make-type 'integer))))))