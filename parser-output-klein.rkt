#lang racket

(require "parser.rkt")

(provide (all-defined-out))

(define klein/euclid-output 
"remainder a b 
gcd a b 
main a b")

(define klein/circ-prime-output
"main x 
circularPrimesTo x 
circularPrimesToHelper top x count 
isCircularPrime x 
isCircularPrimeHelper x turns 
report x

andy x y 
rotate x 
pow x y 
powHelper base power total 
log10 x 
log10Helper x y 
add x y 
div x y 
isPrime n 
hasDivisorFrom i n 
divides a b 
mod num den")
  
(define klein/factors-output
"main n 
loopToN n current 
testAndLoop n current 
printAndLoop n current 

divides a b 
remainder num den")
  
(define klein/farey-output
"main xNum xDen N 

fareyNum xNum xDen N 
fareyDen xNum xDen N 
fareySelectNum N a b c d 
fareySelectDen N a b c d 
whileLoopFor selector xNum xDen N a b c d 
fractionEqual x xd y yd 
fractionGreater x xd y yd 
greater x y")
  
(define klein/fib-output
"main elementWanted 
addNext currentElement elementWanted previousSum currentSum")
  
(define klein/horner-output 
"main x 
horner x n value 
coefficient i 
NEG i 
TIMES i k")
  
(define klein/horner-param-output
"main coeff3 coeff2 coeff1 coeff0 x 
horner x n value coeff3 coeff2 coeff1 coeff0 
coefficient i coeff3 coeff2 coeff1 coeff0 
NEG i 
TIMES i k")
  
(define klein/lib-output
"main testArgument 

AND p q 
LT p q 
EQ p q 
LE p q 
GE p q 
GT p q 
PLUS p q 
MINUS p q 
TIMES p q 
DIV p q 
NEG n 
ABS n 
ODD n 
SQRT n 
SQRTSEARCH n low high 
SQRTSPLIT n low high mid")
  
(define klein/sieve-output
"main n 
sieveAt current max 
doSieveAt current max 

isPrime n 
hasDivisorFrom i n 
divides a b 
rem num den")