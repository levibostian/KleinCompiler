#lang racket
; Klein compiler - UNIT TESTING
; cs4550 - Fall 2013
; 
; Team RackAttack

(require rackunit
         "scanner.rkt")

;we do not have finished compiler yet, so cannot test yet
;(check-eq? (klein "path-to-source.kln") )

(check-equal? (generate-token "boolean") "<keyword> boolean")
(check-equal? (generate-token "integer") "<keyword> integer")
(check-equal? (generate-token "(") "<punctuation> (")
(check-equal? (generate-token ")") "<punctuation> )")
(check-equal? (generate-token ":") "<separator> :")
(check-equal? (generate-token ",") "<separator> ,")
(check-equal? (generate-token "rackAttack") "<identifier> rackAttack")

;;----------------------------------------
;; Helper functions:
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
;
;these tests should be expanded
(check-equal? (reset/accum-chars "(" "main") "")
(check-equal? (check-for/add-tokens "*" '() "main") '("<operator> *" "<keyword> main"));;this is depended on token
;factory and it is not working yet
(check-equal? (check-for/add-tokens "}" '() "main") '())
;

