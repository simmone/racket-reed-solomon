#lang racket

(require "../main.rkt")
(require "../src/lib.rkt")

;; random generate 100's random size's data list
;; encode 16's parity data
;; set random's error
;; decode to the correct data list
;; if error <= 8, can recover or not recover
;; encode 100 times, decode 100 times.

(define *BENCHMARK_DATA_COUNT* 100)
(define *PARITY_LENGTH* 16)

(let* ([random_data_list
        (let loop-all ([count 1]
                       [result_list '()])
          (if (<= count *BENCHMARK_DATA_COUNT*)
              (loop-all
               (add1 count)
               (cons
                (let loop-data ([data_length (random 8 241)]
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
      (let* ([data (car encoded_data)]
             [random_error_count (random 0 *PARITY_LENGTH*)]
             [random_polluted_places
              (let loop-error-place ([count 1]
                                     [error_places '()])
                (if (<= count random_error_count)
                    (loop-error-place
                     (add1 count)
                     (let loop-random-place ([random_place (random 0 (length data))])
                       (if (member random_place error_places)
                           (loop-random-place (random 0 (length data)))
                           (cons random_place error_places))))
                    (reverse error_places)))]
             [polluted_data
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
                     (reverse polluted_result_list)))]
             [decoded_result #f])

        (printf "encoded_data[~a]" (length data))
        (printf "error_count[~a]" random_error_count)
        (printf "~a\n" (if (<= random_error_count 8) "recovered" "can't recover"))

        (with-handlers
         ([exn:fail?
           (lambda (exn)
             (printf "~a\n" (exn-message exn))
             (printf "polluted data:~a\n" polluted_data)
             (error "error happens."))])
         (set! decoded_result (rs-decode polluted_data *PARITY_LENGTH*)))

        (when
         (or
          (and (<= random_error_count 8)
               (not (equal? decoded_result data)))
          (and (> random_error_count 8)
               (equal? decoded_result data)))
         (printf "data:")
         (printf "~a\n" data)
         
         (printf "random error places:\n")
         (printf "~a\n" random_polluted_places)

         (printf "polluted_data:\n")
         (printf "~a\n" polluted_data)

         (printf "decoded_data:\n")
         (printf "~a\n" decoded_result))
        (loop (cdr encoded_data))))))
