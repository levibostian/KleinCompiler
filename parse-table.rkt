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
(define non-terminal?
  (lambda (key)
    (hash-has-key? parse-table key)))

(define terminal-columns
  (hash 'identifier err
        'invalid-identifier err
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
(hash-set! program-cols 'identifier (list 'definitions) )
;
(define definitions-cols (hash-copy terminal-columns))
(hash-set! definitions-cols 'identifier (list 'def 'definitions-prime))
;
(define definitions-prime-cols (hash-copy terminal-columns))
(hash-set*! definitions-prime-cols 'identifier (list 'def 'definitions-prime) 
                                   '$          '(epsilon))
;
(define def-cols (hash-copy terminal-columns))
(hash-set! def-cols 'identifier (list 'identifier '|(| 'formals '|)| ': 'type 'body))
;
(define formals-cols (hash-copy terminal-columns))
(hash-set*! formals-cols 'identifier (list 'nonemptyformals) 
                         '|)|        '(epsilon))
;
(define nonemptyformals-cols (hash-copy terminal-columns))
(hash-set*! nonemptyformals-cols 'identifier (list 'formal 'nonemptyformals-prime))
;
(define nonemptyformals-prime-cols (hash-copy terminal-columns))
(hash-set*! nonemptyformals-prime-cols '|,| (list '|,| 'formal 'nonemptyformals-prime)
                                       '|)| '(epsilon))
;
(define formal-cols (hash-copy terminal-columns))
(hash-set*! formal-cols 'identifier (list 'identifier ': 'type))
;
(define body-cols (hash-copy terminal-columns))
(hash-set*! body-cols 'identifier (list 'expr) 
                      '-       (list 'expr)
                      'if      (list 'expr)
                      'not     (list 'expr) 
                      'boolean (list 'expr)
                      'print   (list 'print '|(| 'expr '|)| 'body)
                      'number  (list 'expr) )
;
(define type-cols (hash-copy terminal-columns))
(hash-set*! type-cols 'boolean (list 'boolean) 
                      'integer (list 'integer))
;
(define expr-cols (hash-copy terminal-columns))
(hash-set*! expr-cols 'if         (list 'simple-expr 'expr-prime)
                      'not        (list 'simple-expr 'expr-prime)
                      'identifier (list 'simple-expr 'expr-prime)
                      'number     (list 'simple-expr 'expr-prime)
                      'boolean    (list 'simple-expr 'expr-prime)
                      '-          (list 'simple-expr 'expr-prime))
;
(define expr-prime-cols (hash-copy terminal-columns))
(hash-set*! expr-prime-cols 'identifier '(epsilon)
                            '|)|   '(epsilon) 
                            '|,|   '(epsilon) 
                            '=     (list '= 'simple-expr 'expr-prime) 
                            '<     (list '< 'simple-expr 'expr-prime)
                            'if    (list 'expr 'expr-prime)
                            'then  '(epsilon) 
                            'else  '(epsilon) 
                            'endif '(epsilon) 
                            '$     '(epsilon) )

;
(define simple-expr-cols (hash-copy terminal-columns))
(hash-set*! simple-expr-cols 'identifier (list 'term 'simple-expr-prime) 
                             '-       (list 'term 'simple-expr-prime)
                             'if      (list 'term 'simple-expr-prime)
                             'not     (list 'term 'simple-expr-prime)
                             'boolean (list 'term 'simple-expr-prime)
                             'number  (list 'term 'simple-expr-prime))
;
(define simple-expr-prime-cols (hash-copy terminal-columns))
(hash-set*! simple-expr-prime-cols 'identifier '(epsilon)
                                   '+     (list '+ 'term 'simple-expr-prime)
                                   '-     (list '- 'term 'simple-expr-prime)
                                   '|,|   '(epsilon) 
                                   '=     '(epsilon) 
                                   '<     '(epsilon) 
                                   'or    (list 'or 'term 'simple-expr-prime)
                                   'then  '(epsilon) 
                                   'else  '(epsilon) 
                                   'endif '(epsilon) 
                                   '|)|   '(epsilon)
                                   '$     '(epsilon))

;
(define term-cols (hash-copy terminal-columns))
(hash-set*! term-cols 'identifier (list 'factor 'term-prime)
                      '-       (list 'factor 'term-prime)
                      'if      (list 'factor 'term-prime)
                      'not     (list 'factor 'term-prime)
                      'boolean (list 'factor 'term-prime)
                      'number  (list 'factor 'term-prime))
;
(define term-prime-cols (hash-copy terminal-columns))
(hash-set*! term-prime-cols 'identifier '(epsilon)
                            '+          '(epsilon) 
                            '-          '(epsilon) 
                            '*          (list '* 'factor 'term-prime)
                            '/          (list '/ 'factor 'term-prime)
                            '|,|        '(epsilon)
                            '=          '(epsilon) 
                            '<          '(epsilon)
                            'or         '(epsilon) 
                            'and        (list 'and 'factor 'term-prime)
                            'then       '(epsilon) 
                            'else       '(epsilon) 
                            'endif      '(epsilon) 
                            '|)|        '(epsilon)
                            '$          '(epsilon))
;
(define factor-cols (hash-copy terminal-columns))
(hash-set*! factor-cols 'identifier (list 'identifier 'factor-prime)
                        '-          (list '- 'factor)
                        'if         (list 'if 'expr 'then 'expr 'else 'expr 'endif)
                        'not        (list 'not 'factor)
                        'boolean    (list 'literal)
                        'number     (list 'literal))
;
(define factor-prime-cols (hash-copy terminal-columns))
(hash-set*! factor-prime-cols 'identifier '(epsilon)
                              '+     '(epsilon) 
                              '-     '(epsilon) 
                              '*     '(epsilon) 
                              '/     '(epsilon) 
                              '|(|   (list '|(| 'actuals '|)|)
                              '|)|   '(epsilon)
                              '|,|   '(epsilon)
                              '=     '(epsilon) 
                              '<     '(epsilon)
                              'or    '(epsilon) 
                              'and   '(epsilon)
                              'then  '(epsilon) 
                              'else  '(epsilon) 
                              'endif '(epsilon) 
                              '$     '(epsilon))
;
(define actuals-cols (hash-copy terminal-columns))
(hash-set*! actuals-cols 'identifier (list 'nonemptyactuals)
                         '-          (list 'nonemptyactuals)
                         '|)|        '(epsilon) 
                         'if         (list 'nonemptyactuals)
                         'not        (list 'nonemptyactuals)
                         'boolean    (list 'nonemptyactuals)
                         'number     (list 'nonemptyactuals))
;
(define nonemptyactuals-cols (hash-copy terminal-columns))
(hash-set*! nonemptyactuals-cols 'identifier (list 'expr 'nonemptyactuals-prime)
                                 '-          (list 'expr 'nonemptyactuals-prime)
                                 'if         (list 'expr 'nonemptyactuals-prime)
                                 'not        (list 'expr 'nonemptyactuals-prime)
                                 'boolean    (list 'expr 'nonemptyactuals-prime)
                                 'number     (list 'expr 'nonemptyactuals-prime))
;
(define nonemptyactuals-prime-cols (hash-copy terminal-columns))
(hash-set*! nonemptyactuals-prime-cols '|)| '(epsilon) 
                                       '|,| (list '|,| 'expr 'nonemptyactuals-prime))
;
(define literal-cols (hash-copy terminal-columns))
(hash-set*! literal-cols 'boolean (list 'boolean) 
                         'number  (list 'number))
;
(define print-cols (hash-copy terminal-columns))
(hash-set*! print-cols 'print (list 'print '|(| 'expr '|)|))
;
;; Do not hash-set! to make all has lookups result in error
(define invalid-identifier-cols (hash-copy terminal-columns))
;
(define parse-table
  (hash 'program               program-cols
        'definitions           definitions-cols
        'definitions-prime     definitions-prime-cols
        'def                   def-cols
        'formals               formals-cols
        'nonemptyformals       nonemptyformals-cols
        'nonemptyformals-prime nonemptyformals-prime-cols
        'formal                formal-cols
        'body                  body-cols
        'type                  type-cols
        'expr                  expr-cols
        'expr-prime            expr-prime-cols
        'simple-expr           simple-expr-cols
        'simple-expr-prime     simple-expr-prime-cols
        'term                  term-cols
        'term-prime            term-prime-cols
        'factor                factor-cols
        'factor-prime          factor-prime-cols
        'actuals               actuals-cols
        'nonemptyactuals       nonemptyactuals-cols 
        'nonemptyactuals-prime nonemptyactuals-prime-cols 
        'literal               literal-cols
        'print                 print-cols
        'invalid-identifier    invalid-identifier-cols))
