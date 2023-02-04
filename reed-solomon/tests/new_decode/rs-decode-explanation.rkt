#lang racket

(require "../../src/field-math.rkt")

(require rackunit)

(define (_rs-decode 
         data_list
         parity_length
         #:bit_width [bit_width 8]
         #:primitive_poly_value [primitive_poly_value 285])

    (parameterize*
     ([*bit_width* 4]
      [*field_generator_poly* "x4+x+1"]
      [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
      [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))]
      [syndromes #f]
      [result #f])

     (set! syndromes (get-syndromes data_list parity_length))

     (if (= (foldr + 0 syndromes) 0)
         (set! result data_list)
         (let-values ([(ome_poly lam_poly) (error-locator-poly (poly-n-strip syndrome_poly) t)])
           (if (not lam_poly)
               (set! result data_list)
               (begin
                 (set! err_places (chien-search lam_poly))

                 (if (null? err_places)
                     (set! result data_list)
                     (begin
                       (set! err_correct_pairs (forney lam_poly ome_poly err_places))

                       (set! corrected_values 
                             (correct-error 
                              appended_data_list
                              err_correct_pairs))

                       (set! result (take corrected_values (length data_list)))))))))

     result
     )))

(check-equal? (_rs-decode 
               '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12) 4
               #:bit_width 4 #:primitive_poly_value 19)
              '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12))
