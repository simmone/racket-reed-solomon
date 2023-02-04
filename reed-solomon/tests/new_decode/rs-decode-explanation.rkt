#lang racket

(require "../../src/field-math.rkt")

(require rackunit)

(define (_rs-decode 
         raw_list
         parity_length
         #:bit_width [bit_width 8]
         #:primitive_poly_value [primitive_poly_value 285])

  (parameterize*
   ([*bit_width* bit_width]
    [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
    [*primitive_poly_value* primitive_poly_value]
    [*gf_aton_map* (get-gf-aton-hash)]
    [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
   
   (hash-set! (*gf_ntoa_map*) 0 0)

   (let (
         [appended_data_list 
          (if (> (*2^m_1*) (length raw_list))
              (append
               raw_list
               (make-list (- (*2^m_1*) (length raw_list)) 0))
              raw_list)]
         [t #f]
         [syndromes #f]
         [syndromes_sum #f]
         [syndrome_poly #f]
         [lam_derivative_poly #f]
         [Yj_poly #f]
         [err_places #f]
         [err_correct_pairs #f]
         [corrected_values #f]
         [result #f]
         )
     
     (set! t (floor (/ parity_length 2)))

     (set! syndromes (get-syndromes appended_data_list (* 2 t)))

     (set! syndrome_poly (coeffients->poly-n syndromes))

     (set! syndromes_sum (foldr + 0 syndromes))
     
     (if (= syndromes_sum 0)
         (set! result raw_list)
         (let-values ([(ome_poly lam_poly) (error-locator-poly (poly-n-strip syndrome_poly) t)])
           (if (not lam_poly)
               (set! result raw_list)
               (begin
                 (set! err_places (chien-search lam_poly))

                 (if (null? err_places)
                     (set! result raw_list)
                     (begin
                       (set! err_correct_pairs (forney lam_poly ome_poly err_places))

                       (set! corrected_values 
                             (correct-error 
                              appended_data_list
                              err_correct_pairs))

                       (set! result (take corrected_values (length raw_list)))))))))

     result
     )))

(parameterize*
 ([*bit_width* 4]
  [*field_generator_poly* "x4+x+1"]
  [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
  [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

 (let-values ([(quotient remainder)
;;             (_galios-poly-divide "x6+x5+x4+x1" "x4+x1+1")])
;;               (_galios-poly-divide "12x3+4x2+3x+15" "6x2+6x1+4")])
               (_galios-poly-divide "7x2+7x+9" "9")])
   (printf "quotient: ~a\nremainder: ~a\n" quotient remainder)))

;(parameterize*
; ([*bit_width* 8]
;  [*field_generator_poly* "x8+x4+x3+x2+1"]
;  [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
;  [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])
;
; (let-values ([(quotient remainder)
;               (_galios-poly-divide
;                "x16"
;                "49x14+195x13+228x12+166x11+225x10+133x9+24x8+105x7+4x6+9x5+222x4+119x3+138x2+193x+87")])
;   (check-equal? quotient "135x+225")
;   (check-equal? remainder "90x31+37x30")))
;
       
(check-equal? (_rs-decode 
               '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12) 4
               #:bit_width 4 #:primitive_poly_value 19)
              '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12))
