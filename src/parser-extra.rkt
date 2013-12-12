#lang racket

(require "scanner.rkt")
(provide (all-defined-out))


(define err "error")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Edit list of functions to be displayed per line;;;
(define convert-to-display
  (lambda (parser-accum)
    (string-join 
     (reverse 
      (map (compose (lambda (x) (string-join x " "))
                    (compose reverse (lambda (x) (map symbol->string x))))
           parser-accum))
     "\n")))
(define gather-parser-output
  (lambda (parser-accum top-of-stack token)
    (cond ((eq? top-of-stack 'def)    (cons (list (token-value token)) parser-accum))
          ((eq? top-of-stack 'formal) (cons (cons (token-value token) (car parser-accum)) 
                                            (cdr parser-accum)))
          (else parser-accum ))))
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Misc helper function;;;
(define end? 
  (lambda (stack token-list)
    (and (end-of-stream? (get-top-of-stack stack)) (end-input-stream? token-list))))
(define top=token?
  (lambda (top-of-stack token)
    (or (equal? top-of-stack (token-value token))
        (eq? top-of-stack (terminal-for token)))))
(define end-of-file-token?
  (lambda (token)
    (equal? (token-type token) '<end-of-file>) ))
(define compose
  (lambda (f g)
    (lambda (lst)
      (f (g lst)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;Stack;;;;;;;;;;;;;;
(define get-top-of-stack car)
(define pop cdr)
(define push
  (lambda (stack value)
    (append value stack) ))
(define end-of-stream?
  (lambda (item)
    (equal? '$ item)) )
(define check-for-push
  (lambda (stack rule)
    (if (equal? rule '(epsilon))
        stack
        (push stack rule))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;Tokens;;;;;;;;;;;;;;;
(define main-check?
  (lambda (token)
    (eq? (token-value token) 'main) ))
(define boolean?
  (lambda (token)
    (and (or (eq? (token-value token) 'true)
             (eq? (token-value token) 'false))
         (eq? (token-type token) '<keyword>)) ))

(define ident-terminal     (lambda (token) 'identifier        ))
(define main-terminal      (lambda (token) 'identifier        ))
(define int-terminal       (lambda (token) 'number            ))
(define invalid-terminal   (lambda (token) 'invalid-identifier))
(define boolean-terminal   (lambda (token) 'boolean           ))
(define operator-terminal  token-value)
(define separator-terminal token-value)
(define punct-terminal     token-value)
(define keyword-terminal
  (lambda (token)
    (cond ((boolean? token   ) (boolean-terminal token))
          ((main-check? token) (main-terminal token   ))
          (else (token-value token)))))

(define terminal-for
  (lambda (token)
    (cond ((<invalid-ident>-token? token) (invalid-terminal   token))
          ((<identifier>-token?    token) (ident-terminal     token))
          ((<keyword>-token?       token) (keyword-terminal   token))
          ((<integer>-token?       token) (int-terminal       token))
          ((<separator>-token?     token) (separator-terminal token))
          ((<operator>-token?      token) (operator-terminal  token))
          ((<punctuation>-token?   token) (punct-terminal     token))
          (else (token-value token)) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;Error-check;;;;;;;;;;;;;;;;;;;;
(define print-error
  (lambda (token stack)
    ; (if (end-of-file-token? token)
    ;     (list 'Compile 'abort. 'Source 'code 'file 'empty.)
        (list  'ERROR: 'with 
               (symbol->string (token-value token)) 
               'on 'column:  (token-col token) 
               'on 'row: (token-row token)
               'top-of-stack: (get-top-of-stack stack) )) )
(define print-transition-error
  (lambda (token stack)
    (list  'ERROR: 'with 
           (symbol->string (token-value token)) 
           'on 'column:  (token-col token) 
           'on 'row: (token-row token))))

(define transition-error?
  (lambda (grammar-rule)
    (eq? grammar-rule err)) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;