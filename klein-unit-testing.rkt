#lang racket
; Klein compiler - UNIT TESTING
; cs4550 - Fall 2013
; 
; Team RackAttack

(require rackunit
         "scanner.rkt"
         "scanner-output-klein.rkt")

(check-equal? (generate-token "boolean") "<keyword> boolean")
(check-equal? (generate-token "integer") "<keyword> integer")
(check-equal? (generate-token "(") "<punctuation> (")
(check-equal? (generate-token ")") "<punctuation> )")
(check-equal? (generate-token ":") "<separator> :")
(check-equal? (generate-token ",") "<separator> ,")
(check-equal? (generate-token "rackAttack") "<identifier> rackAttack")

;;----------------------------------------
;; Helper functions: [EXPAND]
;;----------------------------------------
(check-equal? (rest-of "racket") "acket")
(check-true (member? "a" (list "a" "b" "c")))
(check-true (punctuation? "("))
(check-true (punctuation? ")"))
(check-true (separator? ":"))
(check-true (separator? ","))
(check-false (punctuation? " "))
(check-false (punctuation? "a"))
(check-true (whitespace? " "))
(check-false (whitespace? ""))
(check-false (whitespace? "  "))
(check-false (whitespace? #\tab))
(check-true (keyword? "integer"))
(check-true (keyword? "boolean"))
(check-true (keyword? "true"))
(check-true (keyword? "false"))
(check-false (keyword? "<"))
(check-false (keyword? "="))
(check-true (keyword? "if"))
(check-true (keyword? "then"))
(check-true (keyword? "else"))
(check-true (keyword? "endif"))
(check-true (keyword? "or"))
(check-true (keyword? "and"))
(check-true (keyword? "not"))
(check-true (keyword? "main"))
(check-true (keyword? "print"))
(check-false (keyword? ")"))
(check-false (keyword? "rackAttack"))
(check-true (num? "56"))
(check-false (num? "5.5"))
(check-false (num? "5f"))
(check-true (operator? "+"))
(check-true (operator? "-"))
(check-true (operator? "/"))
(check-true (operator? "*"))
(check-true (operator? "<"))
(check-true (operator? "="))
(check-false (operator? ""))
(check-false (operator? " "))
(check-false (operator? ":"))
(check-false (operator? "a"))
(check-false (operator? "A"))

(check-equal? (reset-or-accum-chars "(" "main") "")
(check-equal? (check-for/add-tokens "*" '() "main") '("<operator> *" "<keyword> main"))
(check-equal? (check-for/add-tokens "}" '() "main") '())

;SCANNER OUTPUT CHECKS
(check-equal? (scanner "klein-programs/euclid.kln")               klein/euclid-output)
(check-equal? (scanner "klein-programs/circular-prime.kln")       klein/circ-prime-output)
(check-equal? (scanner "klein-programs/factors.kln")              klein/factors-output)
(check-equal? (scanner "klein-programs/farey.kln")                klein/farey-output)
(check-equal? (scanner "klein-programs/fibonacci.kln")            klein/fib-output)
(check-equal? (scanner "klein-programs/horner.kln")               klein/horner-output)
(check-equal? (scanner "klein-programs/horner-parameterized.kln") klein/horner-param-output)
(check-equal? (scanner "klein-programs/lib.kln")                  klein/lib-output)
(check-equal? (scanner "klein-programs/sieve.kln")                klein/sieve-output)

(define token-stream (scanner-stream (scanner "klein-programs/euclid.kln")))
(check-equal? (send token-stream 'current) "<identifier> remainder" )
(check-equal? (send token-stream 'peek) "<punctuation> (" )
(check-equal? (send token-stream 'next) "<punctuation> (" )
(check-equal? (send token-stream 'peek) "<identifier> a" )

