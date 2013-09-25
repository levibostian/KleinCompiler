#lang racket
; Klein compiler - UNIT TESTING
; cs4550 - Fall 2013
; 
; Team RackAttack

(require rackunit
         "scanner.rkt")

;we do not have finished compiler yet, so cannot test yet
;(check-eq? (klein "path-to-source.kln") )

(check-equal? (token-factory "boolean") "<type> boolean")
(check-equal? (token-factory "integer") "<type> integer")
(check-equal? (token-factory "(") "<punctuation> (")
(check-equal? (token-factory ")") "<punctuation> )")
(check-equal? (token-factory ":") "<punctuation> :")
(check-equal? (token-factory ",") "<punctuation> ,")
(check-equal? (token-factory "rackAttack") "<identifier> rackAttack")

;;----------------------------------------
;; Helper functions:
;;----------------------------------------
(check-equal? (next-char "racket") "r")
(check-true (member? "a" (list "a" "b" "c")))
(check-true (punctuation? "("))
(check-true (punctuation? ")"))
(check-true (punctuation? ":"))
(check-true (punctuation? ","))
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
(check-true (operator? ">"))
(check-true (operator? "="))
(check-false (operator? ""))
(check-false (operator? " "))
(check-false (operator? ":"))
(check-false (operator? "a"))
(check-false (operator? "A"))