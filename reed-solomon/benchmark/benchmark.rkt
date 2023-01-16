#lang racket

(require "../main.rkt")

(require rackunit)

(define (display-list input_list [col_width 12] [line_count 10])
  (let loop ([loop_list input_list]
             [item_count 1])
    (when (not (null? loop_list))
      (let ([item (~a #:min-width col_width #:align 'left #:right-pad-string " " (format "[~a]" (car loop_list)))])
        (if (<= item_count line_count)
            (begin
              (printf "~a" item)
              
              (loop (cdr loop_list) (add1 item_count)))
            (begin
              (printf "\n~a" item)

              (loop (cdr loop_list) 2))))))
  (printf "\n\n"))

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
    (when (not (null? encoded_data))
      (let ([data (car encoded_data)]
            [random_polluted_places
               (let loop-error-place ([count 1]
                                      [error_places '()])
                 (if (<= count *RANDOM_ERROR_COUNT*)
                     (loop-error-place
                      (add1 count)
                      (cons (random 0 255) error_places))
                     (reverse error_places)))])
        
        (let ([polluted_data
               (let loop-pollute ([data_list data]
                                  [index 0]
                                  [polluted_result_list '()])
                 (if (not (null? data_list))
                     (loop-pollute
                      (cdr data_list)
                      (add1 index)
                      (cons
                       (if (member index random_polluted_places)
                           (random 0 256)
                           (car data_list))
                       polluted_result_list))
                     (reverse polluted_result_list)))])

          (let ([decoded_result (rs-decode polluted_data *PARITY_LENGTH*)])
;            (printf "data:\n")
;            (display-list data)
;            
;            (printf "random error places:\n")
;            (display-list random_polluted_places)
;
;            (printf "polluted_data:\n")
;            (display-list polluted_data)
;
;            (printf "decoded_data:\n")
;            (display-list decoded_result)

            (check-equal? decoded_result data))))
      (loop (cdr encoded_data)))))
