#lang racket
; Klein compiler - PARSER UNIT TESTING
; cs4550 - Fall 2013
; 
; Team RackAttack

(require rackunit
         "parser.rkt"
         "parser-output-klein.rkt")

;PARSER OUTPUT CHECKS ;;Parser prints to user, does not return value -- OUTDATED
;Now just checks based on boolean return, or error. 
(check-equal? (parser "klein-programs/euclid.kln")               klein/euclid-output)
(check-equal? (parser "klein-programs/circular-prime.kln")       klein/circ-prime-output)
(check-equal? (parser "klein-programs/factors.kln")              klein/factors-output)
(check-equal? (parser "klein-programs/farey.kln")                klein/farey-output)
(check-equal? (parser "klein-programs/fibonacci.kln")            klein/fib-output)
(check-equal? (parser "klein-programs/horner.kln")               klein/horner-output)
(check-equal? (parser "klein-programs/horner-parameterized.kln") klein/horner-param-output)
(check-equal? (parser "klein-programs/lib.kln")                  klein/lib-output)
(check-equal? (parser "klein-programs/sieve.kln")                klein/sieve-output)

