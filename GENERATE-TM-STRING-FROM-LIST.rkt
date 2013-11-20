#lang racket
;(list ("0: ST 0,~a(7)" (sym-table-lookup ident-name))
;      ("1: ST 0,~a(7)" (sym-table-lookup ident-name))
;      ("2: ST 0,~a(7)" (sym-table-lookup ident-name))
;      ("3: ST 0,~a(7)" (sym-table-lookup ident-name))
;      ("4: ST 0,~a(7)" (sym-table-lookup ident-name))
;      ("5: ST 0,~a(7)" (sym-table-lookup ident-name)))
; ident-name replaced with the identifier in struct currently at. 
; at end when iterating through list, run: format on each item
;   in list to create the string. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define symbol-lookup
  (lambda (ident)
    'test))

(define lyst (cons (list " ~s 1st " '(symbol-lookup 'main))
                    (cons (list " ~s 2nd " '(symbol-lookup 'square)) '())))

      
(define print-lyst 
  (lambda (lyst accum)
    (if (null? lyst)
        accum
        (print-lyst (cdr lyst) (string-append (format (car (car lyst)) (eval (cadr (car lyst)))) accum)))))

(print-lyst lyst "")