#lang racket
; Klein compiler
; cs4550 - Fall 2013
; 
; Team RackAttack

(provide (all-defined-out))

(define err "error")

;;lookup
(define rule-for 
  (lambda (non-terminal terminal)
    (hash-ref (hash-ref parse-table non-terminal) terminal) ))
;;self-explanatory
(define terminal?
  (lambda (key)
    (hash-has-key? terminal-columns key) ))

(define terminal-columns
  (hash 'identifier err
        '+          err
        '-          err
        '*          err
        '/          err
        '|(|        err
        '|)|        err
        '|,|        err
        '=          err
        '<          err
        'if         err
        'not        err
        'or         err
        'and        err
        'boolean    err
        'integer    err
        'number     err
        'print      err
        'then       err
        'else       err
        'endif      err
        ':          err
        '$          err))
;
(define program-cols (hash-copy terminal-columns))
(hash-set! program-cols 'identifier '(definitions) )
;
(define definitions-cols (hash-copy terminal-columns))
(hash-set! definitions-cols 'identifier '(def definitions-prime))
;
(define definitions-prime-cols (hash-copy terminal-columns))
(hash-set*! definitions-prime-cols 'identifier '(def definitions-prime) '$ '(epsilon))
;
(define def-cols (hash-copy terminal-columns))
(hash-set! def-cols 'identifier '(identifier |(| formals |)| : type body))
;
(define formals-cols (hash-copy terminal-columns))
(hash-set*! formals-cols 'identifier '(nonemptyformals) '|)| '(epsilon))
;
(define nonemptyformals-cols (hash-copy terminal-columns))
(hash-set*! nonemptyformals-cols 'identifier '(formal nonemptyformals-prime))
;
(define nonemptyformals-prime-cols (hash-copy terminal-columns))
(hash-set*! nonemptyformals-prime-cols '|,| '(|,| formal nonemptyformals-prime)
                                       '|)| '(epsilon))
;
(define formal-cols (hash-copy terminal-columns))
(hash-set*! formal-cols 'identifier '(identifier : type))
;
(define body-cols (hash-copy terminal-columns))
(hash-set*! body-cols 'identifier '(expr) 
                  '- '(expr) 
                  'if '(expr) 
                  'not '(expr) 
                  'boolean '(expr) 
                  'print '(print |(| expr |)| body)
                  'number '(expr) )
;
(define type-cols (hash-copy terminal-columns))
(hash-set*! type-cols 'boolean '(boolean) 'integer '(integer))
;
(define expr-cols (hash-copy terminal-columns))
(hash-set*! expr-cols 'if '(simple-expr expr-prime)
                      'not '(simple-expr expr-prime)
                      'identifier '(simple-expr expr-prime)
                      'number '(simple-expr expr-prime)
                      'boolean '(simple-expr expr-prime)
                      '- '(simple-expr expr-prime))
;
(define expr-prime-cols (hash-copy terminal-columns))
(hash-set*! expr-prime-cols 'identifier '(epsilon)
                       '|)| '(epsilon) 
                       '|,| '(epsilon) 
                       '= '(= simple-expr expr-prime) 
                       '< '(< simple-expr expr-prime)
                       'if '(expr expr-prime)
                       'then '(epsilon) 
                       'else '(epsilon) 
                       'endif '(epsilon) 
                       '$ '(epsilon) )

;
(define simple-expr-cols (hash-copy terminal-columns))
(hash-set*! simple-expr-cols 'identifier '(term simple-expr-prime) 
                        '- '(term simple-expr-prime)
                        'if '(term simple-expr-prime)
                        'not '(term simple-expr-prime)
                        'boolean '(term simple-expr-prime)
                        'number '(term simple-expr-prime))
;
(define simple-expr-prime-cols (hash-copy terminal-columns))
(hash-set*! simple-expr-prime-cols 'identifier '(epsilon)
                              '+ '(+ term simple-expr-prime)
                              '- '(- term simple-expr-prime)
                              '|,| '(epsilon) 
                              '= '(epsilon) 
                              '< '(epsilon) 
                              'or '(or term simple-expr-prime)
                              'then '(epsilon) 
                              'else '(epsilon) 
                              'endif '(epsilon) 
                              '|)| '(epsilon)
                              '$ '(epsilon))

;
(define term-cols (hash-copy terminal-columns))
(hash-set*! term-cols 'identifier '(factor term-prime)
                 '- '(factor term-prime)
                 'if '(factor term-prime)
                 'not '(factor term-prime)
                 'boolean '(factor term-prime)
                 'number '(factor term-prime))
;
(define term-prime-cols (hash-copy terminal-columns))
(hash-set*! term-prime-cols 'identifier '(epsilon)
                       '+ '(epsilon) 
                       '- '(epsilon) 
                       '* '(* factor term-prime)
                       '/ '(/ factor term-prime)
                       '|,| '(epsilon)
                       '= '(epsilon) 
                       '< '(epsilon)
                       'or '(epsilon) 
                       'and '(and factor term-prime)
                       'then '(epsilon) 
                       'else '(epsilon) 
                        'endif '(epsilon) 
                        '|)| '(epsilon)
                         '$ '(epsilon))
;
(define factor-cols (hash-copy terminal-columns))
(hash-set*! factor-cols 'identifier '(identifier factor-prime)
                   '- '(- factor)
                   'if '(if expr then expr else expr endif)
                   'not '(not factor)
                   'boolean '(literal)
                   'number '(literal))
;
(define factor-prime-cols (hash-copy terminal-columns))
(hash-set*! factor-prime-cols 'identifier '(epsilon)

                         '+ '(epsilon) 
                         '- '(epsilon) 
                         '* '(epsilon) 
                         '/ '(epsilon) 
                         '|(| '(|(| actuals |)|)
                         '|)| '(epsilon)
                        '|,| '(epsilon)
                         '= '(epsilon) 
                         '< '(epsilon)
                         'or '(epsilon) 
                         'and '(epsilon)
                         'then '(epsilon) 
                       'else '(epsilon) 
                        'endif '(epsilon) 
                         '$ '(epsilon))
;
(define actuals-cols (hash-copy terminal-columns))
(hash-set*! actuals-cols 'identifier '(nonemptyactuals)
                    '- '(nonemptyactuals)
                    '|)| '(epsilon) 
                    'if '(nonemptyactuals)
                    'not '(nonemptyactuals)
                    'boolean '(nonemptyactuals)
                    'number '(nonemptyactuals))
;
(define nonemptyactuals-cols (hash-copy terminal-columns))
(hash-set*! nonemptyactuals-cols 'identifier '(expr nonemptyactuals-prime)
                            '- '(expr nonemptyactuals-prime)
                            'if '(expr nonemptyactuals-prime)
                            'not '(expr nonemptyactuals-prime)
                            'boolean '(expr nonemptyactuals-prime)
                            'number '(expr nonemptyactuals-prime))
;
(define nonemptyactuals-prime-cols (hash-copy terminal-columns))
(hash-set*! nonemptyactuals-prime-cols '|)| '(epsilon) 
                                  '|,| '(|,| expr nonemptyactuals-prime))
;
(define literal-cols (hash-copy terminal-columns))
(hash-set*! literal-cols 'boolean '(boolean) 'number '(number))
;
(define print-cols (hash-copy terminal-columns))
(hash-set*! print-cols 'print '(print |(| expr |)|))
;
(define parse-table
  (hash 'program program-cols
        'definitions definitions-cols
        'definitions-prime definitions-prime-cols
        'def def-cols
        'formals formals-cols
        'nonemptyformals nonemptyformals-cols
        'nonemptyformals-prime nonemptyformals-prime-cols
        'formal formal-cols
        'body body-cols
        'type type-cols
        'expr expr-cols
        'expr-prime expr-prime-cols
        'simple-expr simple-expr-cols
        'simple-expr-prime simple-expr-prime-cols
        'term term-cols
        'term-prime term-prime-cols
        'factor factor-cols
        'factor-prime factor-prime-cols
        'actuals actuals-cols
        'nonemptyactuals nonemptyactuals-cols 
        'nonemptyactuals-prime nonemptyactuals-prime-cols 
        'literal literal-cols
        'print print-cols))
