#lang racket

; This is the code live-coded in class

; Custom reader
; Requirement

(provide read-syntax)

#; (define (read-syntax path port)   ; path (filename), port an input stream
  (displayln "Frothy line parser")
  ; What happens with port?  (high level)
  (define src-lines (port->lines port))     ; Read input
  (append '(module frothy "frothy.rkt")
          (parse-lines src-lines))
     
  )

; How do write parse-lines?  How do even know what to write for this?
; What do I need?  What would nice to have?
;
; Specification
; Frothy Grammar.   { e } means zero or more e's
;
;  program := { statement } EOF
;
;  statement := do_statement
;            |  def_statement
;
;  do_statement := atom <newline>
;
;  def_statement := ":" SYMBOL { atom } <newline>
;
;  atom := NUMBER
;       |  SYMBOL
;

(define (parse-lines src-lines)
  (map parse-line
       (filter non-empty-string?       ; Getting blank lines
       (map string-trim src-lines))))  ; Getting whitespace

(define (parse-line line)
  (if (equal? (string-ref line 0) #\:)
      (cons 'def (parse-atoms (substring line 1)))
      (list 'do (parse-atom line))))

(define (parse-atoms line)
  (map parse-atom (string-split line)))

(define (parse-atom s)
  (let ([v (string->number s)])
    (if v
        v
        (string->symbol s))))

; Libraries/tools that can help with parsing
; Write a "lexer" or "tokenizer"

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

#; (define (read-syntax path port)   ; path (filename), port an input stream
  (displayln "Frothy token parser")
  ; What happens with port?  (high level)
  (append '(module frothy "frothy.rkt")
          (parse-tokens (lambda () (next-token port)))))

(define (expect nt tokname)
  (let ([t (nt)])
    (if (equal? (token-name t) tokname)
        t
        (error "Syntax error")
        )))

(define (parse-tokens nt)
  (let ([t (nt)])
    (case (token-name t)
      [(EOF) '()]
      [(NEWLINE) (parse-tokens nt)]
      [(COLON) (append (list (cons 'def (parse-def nt))) (parse-tokens nt))]
      [(NUMBER) (expect nt 'NEWLINE) (append (list (list 'do (token-value t)))
                                             (parse-tokens nt))]
      [(SYMBOL) (expect nt 'NEWLINE) (append (list (list 'do (token-value t)))
                                             (parse-tokens nt))]      
      [else (error "What?")])))

(define (parse-def nt)
  (let ([t (nt)])
    (case (token-name t)
      [(NEWLINE) '()]
      [(NUMBER) (cons (token-value t) (parse-def nt))]
      [(SYMBOL) (cons (token-value t) (parse-def nt))]
      [else (error "What?")])))

; Example of a parse-generator tool
; Classic: YACC (Yet Another Compiler Compiler)  (1971)
; GNU: Bison

(require parser-tools/yacc)

; Grammar specification + Actions
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
  
(define (read-syntax path port)
  (displayln "Frothy Yacc Parser")
    ; Must return a module
  (append 
   '(module anonymous "frothy.rkt")
   (frothyparser (lambda () (next-token port))))
  )
    


