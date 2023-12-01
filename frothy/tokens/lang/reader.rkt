#lang racket

; A reader that uses a tokenizer defined via lex

(provide read-syntax)

(define (read-syntax path port)
  (displayln "Frothy token parser")
  (append '(module frothy "frothy.rkt")
          (parse-tokens (lambda () (next-token port)))))

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

; A hand-written parser that operates on the token stream
; "next" is a zero-argument function that produces the next token.

(define (expect next tokname)
  (let ([t (next)])
    (if (equal? (token-name t) tokname)
        t
        (error "Syntax error")
        )))

(define (parse-tokens next)
  (let ([t (next)])
    (case (token-name t)
      [(EOF) '()]
      [(NEWLINE) (parse-tokens next)]
      [(COLON) (append (list (cons 'def (parse-def next))) (parse-tokens next))]
      [(NUMBER) (expect next 'NEWLINE) (append (list (list 'do (token-value t)))
                                             (parse-tokens next))]
      [(SYMBOL) (expect next 'NEWLINE) (append (list (list 'do (token-value t)))
                                             (parse-tokens next))]      
      [else (error "What?")])))

(define (parse-def next)
  (let ([t (next)])
    (case (token-name t)
      [(NEWLINE) '()]
      [(NUMBER) (cons (token-value t) (parse-def next))]
      [(SYMBOL) (cons (token-value t) (parse-def next))]
      [else (error "What?")])))

