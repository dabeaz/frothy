#lang racket

; Frothy. The language. 

; There is a stack
(define stack empty)
(define (push v)
  (set! stack (cons v stack)))
(define (pop)
  (let ([v (first stack)])
    (set! stack (rest stack))
    v))

; There is an environment of "words".
; Words are 0-argument functions
(define words (make-hash))
(define (def-word w f)
  (hash-set! words w f))
(define (exec-word w)
  ((hash-ref words w)))

; Builtins
; Example of a words
(def-word 'DISPLAY (lambda () (displayln (pop))))
(def-word '+ (lambda () (push (+ (pop) (pop)))))
(def-word '* (lambda () (push (* (pop) (pop)))))
(def-word 'DUP (lambda () (let ([v (pop)]) (push v) (push v))))

; This is the core language.
;    - Numbers : Push on stack
;    - Symbols : Execute as a word

(define (exec cmd)
  (cond [(number? cmd) (push cmd)]
        [(symbol? cmd) (exec-word cmd)]
        [else (error "Huh?")]))

; Uh. All the quoting.   We can fix that with macros!
(define-syntax do
  (syntax-rules ()
    [(_ v) (exec (quote v))]))

(define-syntax def
  (syntax-rules ()
    [(_ name w ...) (def-word (quote name)
                      (lambda () (do w) ...))]))

; Must provide the definitions to export
(provide do def)

; Required macro for an s-exp language
(define-syntax froth-module-begin
  (syntax-rules ()
    [(_ c ...) (#%module-begin
                c ...)]))

(provide (rename-out [froth-module-begin #%module-begin]))
(provide #%top-interaction)
 
; Can define useful new words using exec
(def-word 'PI (lambda () (exec 3.14159)))
(def-word 'SQUARE (lambda ()
                    (exec 'DUP)
                    (exec '*)))
(def-word 'CIRCLE-AREA (lambda ()
                         (exec 'SQUARE)
                         (exec 'PI)
                         (exec '*)))






