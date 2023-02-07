#lang racket

(require "../../../src/field-math.rkt")

(require rackunit)

(define (_error-locator syndromes error_length)

  (printf "syndromes: ~a, error_length: [~a]\n\n" syndromes error_length)

  (let ([syndrome_poly
         (index_coe_pairs->poly
          (let loop ([loop_syndromes syndromes]
                     [loop_index (sub1 (length syndromes))]
                     [coe_pairs '()])
            (if (not (null? loop_syndromes))
                (loop (cdr loop_syndromes) (sub1 loop_index) (cons (cons loop_index (car loop_syndromes)) coe_pairs))
                (reverse coe_pairs))))])

    (printf "syndrome_poly: ~a\n" syndrome_poly)

    (let loop ([loop_dividend (format "x~a" (* 2 error_length))]
               [loop_divisor syndrome_poly]
               [loop_add_factor "0"]
               [loop_multiply_factor "1"])

      (printf "\nloop_dividend: ~a\n" loop_dividend)
      (printf "loop_divisor: ~a\n" loop_divisor)
      (printf "loop_add_factor: ~a\n" loop_add_factor)
      (printf "loop_multiply_factor: ~a\n" loop_multiply_factor)

      (let ([loop_result #f])
        (let-values ([(quotient remainder) (galios-poly-divide loop_dividend loop_divisor)])
          (printf "(galios-poly-divide ~a ~a) = (~a ~a)\n\n" loop_dividend loop_divisor quotient remainder)

          (set! loop_result (galios-poly-add loop_add_factor (galios-poly-multiply quotient loop_multiply_factor)))
          (printf "loop_result = (galios-poly-add ~a (galios-poly-multiply ~a ~a)) = ~a\n\n" loop_add_factor quotient loop_multiply_factor loop_result)
          
          (if (>= (caar (poly->index_coe_pairs remainder)) error_length)
              (loop
               loop_divisor
               remainder
               loop_multiply_factor
               loop_result)
              (let ([last_coe_index (cdr (last (poly->index_coe_pairs loop_result)))])
                (if (= last_coe_index 0)
                    (values #f #f)
                    (let-values ([(ome_quotient ome_remainder) (galios-poly-divide remainder (number->string last_coe_index))]
                                 [(lam_quotient lam_remainder) (galios-poly-divide loop_result (number->string last_coe_index))])
                      (printf "galios-poly-divide ~a ~a) = (values ~a ~a)\n" remainder last_coe_index ome_quotient ome_remainder)
                      (printf "galios-poly-divide ~a ~a) = (values ~a ~a)\n" loop_result last_coe_index lam_quotient lam_remainder)
                      (printf "result: (values ~a ~a)\n\n" ome_quotient lam_quotient)
                      (values ome_quotient lam_quotient))))))))))

;(parameterize*
; ([*bit_width* 4]
;  [*field_generator_poly* "x4+x+1"]
;  [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
;  [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])
;
; (let-values ([(ome lam) (_error-locator '(12 4  3 15) 2)])
;   (check-equal? ome "6x+15")
;   (check-equal? lam "14x2+14x+1")))

(parameterize*
 ([*bit_width* 8]
  [*field_generator_poly* "x8+x4+x3+x2+1"]
  [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
  [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

 (let-values ([(ome lam) (_error-locator '(208 221 122 7 253 253 76 219 69 7) 5)])
   (check-equal? ome "29x4+161x3+50x2+49x+7")
   (check-equal? lam "141x5+214x4+47x3+252x2+194x+1")
   )
)

