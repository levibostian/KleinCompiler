#lang racket
; Klein compiler - UNIT TESTING
; cs4550 - Fall 2013
; 
; Team RackAttack

(require rackunit
         "klein.rkt")

<<<<<<< HEAD
;we do not have finished compiler yet, so cannot test yet
;(check-eq? (klein "path-to-source.kln") )

(check-equal? (token-factory "boolean") "<type> boolean")
(check-equal? (token-factory "integer") "<type> integer")
(check-equal? (token-factory "(") "<punctuation> (")
(check-equal? (token-factory ")") "<punctuation> )")
(check-equal? (token-factory ":") "<punctuation> :")
(check-equal? (token-factory ",") "<punctuation> ,")
(check-equal? (token-factory "rackAttack") "<identifier> rackAttack")
=======
>>>>>>> master
