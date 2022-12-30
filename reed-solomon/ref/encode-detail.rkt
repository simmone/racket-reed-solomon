#lang racket

(require "lib.rkt")

(require rackunit "../lib/encode/encode.rkt")

;;    (check-equal?
;;     (rs-encode '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17) 10)
;;     (list 196  35  39  119  235  215  231  226  93  23))
    
(let ([input_data_list '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17)]
      [parity_length 10])

  (printf "start enode\n\n")

  (printf "input data list:\n\n")
  
  (display-list input_data_list)
  
  (printf "parity_length:~a\n\n" parity_length)
  )
