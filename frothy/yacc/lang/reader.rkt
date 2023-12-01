#lang racket

; A reader that uses lex/yacc

(provide read-syntax)

(define (read-syntax path port)
  (displayln "Frothy Yacc Parser")
    ; Must return a module
  (append 
   '(module anonymous "frothy.rkt")
   (frothyparser (lambda () (next-token port))))
  )

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

; Specification for words/symbols in the input
(define-tokens value-tokens (NUMBER SYMBOL))
(define-empty-tokens literal-tokens (EOF NEWLINE COLON))

(define next-token
  (lexer
   ; Matching rules
   [(eof) (token-EOF)]
   ["\n" (token-NEWLINE)]
   [":"  (token-COLON)]
   [(repetition 1 +inf.0 " ") (next-token input-port)]
   [(union (concatenation (repetition 1 +inf.0 numeric) "."
                          (repetition 0 +inf.0 numeric))
           (repetition 1 +inf.0 numeric)) (token-NUMBER (string->number lexeme)) ]
   [(repetition 1 +inf.0 graphic) (token-SYMBOL (string->symbol lexeme))]
   )
  )

(require parser-tools/yacc)

(define frothyparser
  (parser
   (start program)
   (end EOF)
   (tokens value-tokens literal-tokens)
   (error (lambda args (displayln "An error occurred") (void)))
   (grammar
    (program [() '()]
             [(program statement) (append $1 $2)])
    (statement [(do-statement) $1]
               [(def-statement) $1]
               [(NEWLINE) '()])
    (do-statement [(atom NEWLINE) (list (list 'do $1))])
    (def-statement [(COLON atoms NEWLINE) (list (cons 'def $2))])
    (atoms [(atom) (list $1)]
           [(atoms atom) (append $1 (list $2))])
    (atom [(NUMBER) $1]
          [(SYMBOL) $1])
    )
  ))
  