#lang racket

(require "../main.rkt")

;; random generate 1000's random size's data list
;; encode 16's parity data
;; random set max 8's error
;; decode to the correct data list
;; encode 1000 times, decode 1000 times.

(define *BENCHMARK_DATA_COUNT* 1000)
(define *RANDOM_ERROR_COUNT* 8)
(define *PARITY_LENGTH* 16)

(let* ([random_data_list
        (let loop-all ([count 1]
                       [result_list '()])
          (if (<= count *BENCHMARK_DATA_COUNT*)
              (loop-all
               (add1 count)
               (cons
                (let loop-data ([data_length (random 1 240)]
                                [data_count 1]
                                [data_list '()])
                  (if (<= data_count data_length)
                      (loop-data data_length (add1 data_count) (cons (random 0 256) data_list))
                      (reverse data_list)))
                result_list))
              (reverse result_list)))]
       [encoded_data_list
        (map
         (lambda (data)
           `(,@data ,@(rs-encode data *PARITY_LENGTH*)))
         random_data_list)])
  
  (let loop ([encoded_data encoded_data_list])



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


