#lang reader "s-exp/lang/reader.rkt"

(do 3)
(do 4)
(do +)
(do DISPLAY)

(def SQUARE DUP *)
(do 10)
(do SQUARE)
(do DISPLAY)



