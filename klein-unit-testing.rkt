#lang racket
; Klein compiler - UNIT TESTING
; cs4550 - Fall 2013
; 
; Team RackAttack

(require rackunit
         "scanner.rkt"
         "scanner-output-klein.rkt")

(check-equal? (generate-token "boolean" "1" 1) "<keyword> boolean 1 1")
(check-equal? (generate-token "integer" "10" 5) "<keyword> integer 10 5")
(check-equal? (generate-token "(" "5" 5) "<punctuation> ( 5 5")
(check-equal? (generate-token ")" "2" 1) "<punctuation> ) 2 1")
(check-equal? (generate-token ":" "1" 10) "<separator> : 1 10")
(check-equal? (generate-token "," "3" 19) "<separator> , 3 19")
(check-equal? (generate-token "rackAttack" "5" 13) "<identifier> rackAttack 5 13")

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

(check-equal? (get-column-num "main" 5) "1")
(check-equal? (get-column-num "integer" 8) "1") 

(check-equal? (reset-or-accum-chars "(" "main") "")
<<<<<<< HEAD
(check-equal? (check-for/add-tokens "*" '() "main" 10 3) '("<operator> * 10 3" "<keyword> main 6 3"))

(check-equal? (check-for/add-tokens "}" '() "main" 5 1) '())
=======
(check-equal? (check-for/add-tokens "*" '() "main") '("<operator> *" "<keyword> main"))
(check-equal? (check-for/add-tokens "}" '() "main") '())
>>>>>>> master

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
(check-equal? (send-args-to-func token-stream 'current) "<identifier> remainder" )
(check-equal? (send-args-to-func token-stream 'peek) "<punctuation> (" )
(check-equal? (send-args-to-func token-stream 'next) "<punctuation> (" )
(check-equal? (send-args-to-func token-stream 'peek) "<identifier> a" )

