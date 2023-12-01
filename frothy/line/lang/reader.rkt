#lang racket

; Read input files using a line-oriented approach.

(provide read-syntax)

(define (read-syntax path port)
  (displayln "Frothy Line Parser")
  (define src-lines (port->lines port))

  ; Must return a module
  (append 
   '(module anonymous "frothy.rkt")
   (parse-lines src-lines))
  )

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

