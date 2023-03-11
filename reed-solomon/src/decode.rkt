#lang racket

(require "../src/field-math.rkt")
(require "../src/decode/syndrome.rkt")
(require "../src/decode/error-locator.rkt")
(require "../src/decode/chien-search.rkt")
(require "../src/decode/forney.rkt")
(require "../src/primitive_poly_table.rkt")

(provide (contract-out
          [rs-decode (->* 
                      ((listof exact-integer?) natural?) 
                      (#:bit_width natural? #:primitive_poly_value natural?) 
                      (listof exact-integer?))]
          ))

(define (rs-decode 
         data_list
         parity_length
         #:bit_width [bit_width 8]
         #:primitive_poly_value [primitive_poly_value 285])

  (parameterize*
   ([*bit_width* bit_width]
    [*field_generator_poly* (hash-ref *primitive_poly_table* primitive_poly_value)]
    [*galios_index->number_map* (get-galios-index->number_map)]
    [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])


   (let* ([*t* (floor (/ parity_length 2))]
          [*2^m* (expt 2 (*bit_width*))]
          [appended_data_list 
           (if (> *2^m* (length data_list))
               (append
                data_list
                (make-list (- *2^m* (length data_list)) 0))
               data_list)])

     (define syndromes (get-syndromes appended_data_list (* 2 *t*)))
     
     (if (null? syndromes)
         data_list
         (let-values ([(ome_poly lam_poly) (error-locator syndromes *t*)])
           (if (not lam_poly)
               data_list
               (let ([err_places (chien-search lam_poly)]
                     [restored_list #f])
                 (set! restored_list
                       (if (null? err_places)
                           data_list
                           (let ([err_correct_pairs (forney lam_poly ome_poly err_places)])
                             (let loop ([patches err_correct_pairs]
                                        [loop_data_list (reverse appended_data_list)])
                               (if (not (null? patches))
                                   (let ([restored_data (bitwise-xor (cdar patches) (list-ref loop_data_list (caar patches)))])
                                     (loop
                                      (cdr patches)
                                      (list-set loop_data_list (caar patches) restored_data)))
                                   (reverse loop_data_list))))))
                 (take restored_list (length data_list)))))))))
