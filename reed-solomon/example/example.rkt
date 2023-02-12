#lang racket

(require "../main.rkt")

(let* ([rs_code 
        (rs-encode '(1 2 3 4 5 6 7 8 9 10 11) 4 #:bit_width 4 #:primitive_poly_value 19)]
       [polluted_data_list (append '(1 2 3 4 5 11 7 8 9 10 1) rs_code)])

  (printf "~a\n" (rs-decode polluted_data_list 4 #:bit_width 4 #:primitive_poly_value 19)))
  ;; (1 2 3 4 5 6 7 8 9 10 11 3 3 12 12)

(let* ([original_data "Chen Xiao is just a programmer."]
       [polluted_data "Chen Xiao is a fabulous artist."]
       [rs_code
        (rs-encode (bytes->list (string->bytes/utf-8 original_data)) 34)]
       [encoded_polluted_data
        (bytes-append
         (string->bytes/utf-8 polluted_data)
         (list->bytes rs_code))])

  (printf "original_data: ~a\n" original_data)
  
  (printf "polluted_data: ~a\n" polluted_data)

  (printf "rs-encode(original_data, 34) = ~a\n" (list->bytes rs_code))

  (printf "encoded_polluted_data: ~a\n" encoded_polluted_data)

  (printf "rs-decode(encoded_polluted_data, 34) = ~a\n" (list->bytes (rs-decode (bytes->list encoded_polluted_data) 34))))


