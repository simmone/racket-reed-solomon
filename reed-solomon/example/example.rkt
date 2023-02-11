#lang racket

(require "../main.rkt")

(let* ([rs_code 
        (rs-encode '(1 2 3 4 5 6 7 8 9 10 11) 4 #:bit_width 4 #:primitive_poly_value 19)]
       [polluted_data_list (append '(1 2 3 4 5 11 7 8 9 10 1) rs_code)])

  (printf "~a\n" (rs-decode polluted_data_list 4 #:bit_width 4 #:primitive_poly_value 19)))
  ;; (1 2 3 4 5 6 7 8 9 10 11 3 3 12 12)

(let* ([rs_code
        (rs-encode (bytes->list (string->bytes/utf-8 "Chen Xiao is just a programmer.")) 34)]
       [polluted_data_list
        (append
         (bytes->list #"Chen Xiao is a fabulous artist.")
         rs_code)])

  (printf "~a\n" (list->bytes (take (rs-decode polluted_data_list 34) 31))))
  ;; Chen Xiao is just a programmer.


