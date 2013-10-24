(require "semantic-actions.rkt")

(define print/identifier
  (lambda (ident name)
    (string-append name "(" (symbol->string (identifier-value ident)) ")\n")))

(define print/type
  (lambda (type~ name)
    (string-append name "(" (symbol->string (type-value type~)) ")\n")))

(define print/formal
  (lambda (form)
    (string-append (print/identifier (formal-id form) "    identifier") 
                   "\n" 
                   (print/type (formal-type form) "    type"))))

(define print/def
  (lambda (def~)
    (string-append (print/identifier (def-id def~) "name")
                   (print/formal (def-formals def~))
                   (print/type (def-type def~) "returns"))))
                   ;(print/body (def-body def~))
