#lang racket

;(define amt-of-columns 22)
(define err "err")
;(define row-in-table (lambda () (make-vector amt-of-columns err))); lambda so it re-evaluates on each call. (so it doesn't give back same vector each time)
;
;(define amt-of-rows 23)
;
;;(define parse-table (vector-map (lambda (x) (row-in-table)) (make-vector amt-of-rows)));fills table with vectors
;
;(define-values 
;  (program
;   defintions
;   defintions-prime
;   def
;   formals
;   non-empty-formals
;   non-empty-formals-prime
;   formal
;   body
;   type
;   expr
;   expr-prime
;   simple-expr
;   simple-expr-prime
;   term
;   term-prime
;   factor
;   factor-prime
;   actuals
;   non-empty-actuals
;   non-empty-actuals-prime
;   literal
;   print)
;  (values 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22)); this should be done for columns as well
;
;(define add-rule
;  (lambda (row col rule)
;    (vector-set! (vector-ref parse-table row) col rule))) 
;(add-rule program 0 '(definitions))
;(add-rule defintions 0 '(def defintions-prime))

;map over and apply and you can fill rows out really easily??? **
;parse-table
(define terminal-columns
  (hash "identifier" err
        "+"          err
        "-"          err
        "*"          err
        "/"          err
        "("          err
        ")"          err
        ","          err
        "="          err
        "<"          err
        "if"         err
        "not"        err
        "or"         err
        "and"        err
        "boolean"    err
        "integer"    err
        "number"     err
        "print"      err
        "then"       err
        "else"       err
        "endif"      err
        "$"          err))
;
(define program-cols (hash-copy terminal-columns))
(hash-set! program-cols "identifier" '(defintions) )
;
(define defintions-cols (hash-copy terminal-columns))
(hash-set! defintions-cols "identifier" '(def definitions-prime))
;
(define definitions-prime (hash-copy terminal-columns))
(hash-set*! definitions-prime "identifier" '(def defintions-prime) "$" '(epsilon))
;
(define def-cols (hash-copy terminal-columns))
(hash-set! def-cols "identifier" '(identifier |(| formals |)| : type body))
;
(define formals-cols (hash-copy terminal-columns))
(hash-set*! formals-cols "identifier" '(nonemptyformals) ")" '(epsilon))
;
(define nonemptyformals (hash-copy terminal-columns))
(hash-set*! nonemptyformals "identifier" '(formal nonemptyformals-prime))
;
(define nonemptyformals-prime (hash-copy terminal-columns))
(hash-set*! nonemptyformals-prime "," '(|,| formal nonemptyformals-prime))
;
(define formal (hash-copy terminal-columns))
;(hash-set*! formal "identifier" '(identifier : type)) INCOMPLETE
;
(define body (hash-copy terminal-columns))
(hash-set*! body "identifier" '(expr) "-" '(expr) "if" '(expr) "not" '(expr) "boolean" '(expr) "print" '(print body) )
;
(define type (hash-copy terminal-columns))
(hash-set*! type "boolean" '(boolean) "integer" '(integer))
;
(define expr (hash-copy terminal-columns))
;(hash-set*! expr "expr" ;INCOMPLETE
;
(define expr-prime (hash-copy terminal-columns))
(hash-set*! expr-prime "identifier" '(epsilon) 
                       "+" '(epsilon) 
                       "-" '(epsilon) 
                       "*" '(epsilon) 
                       "/" '(epsilon) 
                       ")" '(epsilon) 
                       "," '(epsilon) 
                       "=" '(= simple-expr expr-prime) 
                       "<" '(< simple-expr expr-prime)
                       "or" '(epsilon) 
                       "and" '(epsilon) 
                       "then" '(epsilon) 
                       "else" '(epsilon) 
                       "endif" '(epsilon) 
                       "$" '(epsilon) )
(define simple-expr (hash-copy terminal-columns))
(hash-set*! simple-expr "identifier" '(term simple-expr-prime) 
                        "-" '(term simple-expr-prime)
                        "if" '(term simple-expr-prime)
                        "not" '(term simple-expr-prime)
                        "boolean" '(term simple-expr-prime)
                        "number" '(term simple-expr-prime))
;
(define simple-expr-prime (hash-copy terminal-columns))
(hash-set*! simple-expr-prime "+" '(+ term simple-expr-prime)
                              "-" '(- term simple-expr-prime)
                              "=" '(epsilon) 
                              "<" '(epsilon) 
                              "or" '(or term simple-expr-prime))
;
(define term (hash-copy terminal-columns))
(hash-set*! term "identifer" '(factor term-prime)
                 "if" '(factor term-prime)
                 "not" '(factor term-prime)
                 "boolean" '(factor term-prime)
                 "number" '(factor term-prime))
;
(define term-prime (hash-copy terminal-columns))
(hash-set*! term-prime "+" '(epsilon) 
                       "-" '(epsilon) 
                       "*" '(* factor term-prime)
                       "/" '(/ factor term-prime)
                       "=" '(epsilon) 
                       "<" '(epsilon)
                       "or" '(epsilon) 
                       "and" '(and factor term-prime))
;
(define factor (hash-copy terminal-columns))
(hash-set*! factor "identifier" '(identifier factor-prime)
                   "-" '(- factor)
                   "if" '(if expr then expr else expr endif)
                   "not" '(not factor)
                   "boolean" '(literal)
                   "number" '(literal))
;
(define factor-prime (hash-copy terminal-columns))
(hash-set*! factor-prime "+" '(epsilon) 
                         "-" '(epsilon) 
                         "*" '(epsilon) 
                         "/" '(epsilon) 
                         "(" '(|(| actuals |)|)
                         "=" '(epsilon) 
                         "<" '(epsilon)
                         "or" '(epsilon) 
                         "and" '(epsilon) )
;
(define actuals (hash-copy terminal-columns))
(hash-set*! actuals "identifier" '(nonemptyactuals)
                    "-" '(nonemptyactuals)
                    ")" '(epsilon) 
                    "if" '(nonemptyactuals)
                    "not" '(nonemptyactuals)
                    "boolean" '(nonemptyactuals)
                    "number" '(nonemptyactuals))
;
(define nonemptyactuals (hash-copy terminal-columns))
(hash-set*! nonemptyactuals "identifier" '(expr nonemptyactuals-prime)
                            "-" '(expr nonemptyactuals-prime)
                            "if" '(expr nonemptyactuals-prime)
                            "not" '(expr nonemptyactuals-prime)
                            "boolean" '(expr nonemptyactuals-prime)
                            "number" '(expr nonemptyactuals-prime))
;
(define nonemptyactuals-prime (hash-copy terminal-columns))
(hash-set*! nonemptyactuals-prime ")" '(epsilon) 
                                  "," '(|,| expr nonemptyactuals-prime))
;
(define literal (hash-copy terminal-columns))
(hash-set*! literal "boolean" '(boolean) "number" '(number))
;
(define print (hash-copy terminal-columns))
(hash-set*! print "print" '(print |(| expr |)|))
;
                       
                        


(define parse-table
  (hash 'program 
        'defintions 
        'defintions-prime 
        'def 
        'formals 
        'non-empty-formals 
        'non-empty-formals-prime 
        'formal 
        'body 
        'type 
        'expr-prime 
        'simple-expr 
        'simple-expr-prime 
        'term 
        'term-prime 
        'factor 
        'factor-prime 
        'actuals 
        'non-empty-actuals 
        'non-empty-actuals-prime 
        'literal 
        'print ))

;parse-table-