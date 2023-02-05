#lang racket

(require "../../src/lib.rkt")
(require "../../src/field-math.rkt")
(require "../../src/new_decode/syndrome.rkt")
(require "../../src/new_decode/error-locator.rkt")
(require "../../src/new_decode/chien-search.rkt")
(require "../../src/primitive_poly_table.rkt")

(require rackunit)

(define (_rs-decode 
         data_list
         parity_length
         #:bit_width [bit_width 8]
         #:primitive_poly_value [primitive_poly_value 285])

  (parameterize*
   ([*bit_width* bit_width]
    [*field_generator_poly* (hash-ref *primitive_poly_table* primitive_poly_value)]
    [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
    [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

   (printf "Reed Solomon Decoding Explanation:\n\n")
   
   (printf "data list[~a]:\n\n" (length data_list))
   
   (print-line data_list)

   (printf "parity_length: ~a\n\n" parity_length)
   
   (printf "bit_width: ~a\n\n" (*bit_width*))

   (printf "primitive_poly_value: ~a\n\n" primitive_poly_value)

   (define syndromes (get-syndromes data_list parity_length))
   
   (printf "syndromes: ~a\n\n" syndromes)

   (if (= (foldr + 0 syndromes) 0)
       data_list
       (let-values ([(ome_poly lam_poly) (error-locator syndromes parity_length)])

         (printf "error-locater's ome_poly = ~a\n\n" ome_poly)
         (printf "error-locater's lam_poly = ~a\n\n" lam_poly)

         (if (not lam_poly)
             data_list
             (let ([err_places (chien-search lam_poly)])
               (printf "err_places = ~a\n\n" err_places)
               (if (null? err_places)
                   data_list
                   (let ([err_correct_pairs (forney lam_poly ome_poly err_places)])
                     (correct-error data_list err_correct_pairs)))))))
   ))

(check-equal? (_rs-decode 
               '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12) 4
               #:bit_width 4 #:primitive_poly_value 19)
'(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12))
