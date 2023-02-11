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
              (let ([last_coe (cdr (last (poly->index_coe_pairs loop_result)))]
                    [last_index (car (last (poly->index_coe_pairs loop_result)))])
                (if (not (= last_index 0))
                    (values #f #f)
                    (let-values ([(ome_quotient ome_remainder) (galios-poly-divide remainder (number->string last_coe))]
                                 [(lam_quotient lam_remainder) (galios-poly-divide loop_result (number->string last_coe))])
                      (printf "galios-poly-divide ~a ~a) = (values ~a ~a)\n" remainder last_coe ome_quotient ome_remainder)
                      (printf "galios-poly-divide ~a ~a) = (values ~a ~a)\n" loop_result last_coe lam_quotient lam_remainder)
                      (printf "result: (values ~a ~a)\n\n" ome_quotient lam_quotient)
                      (values ome_quotient lam_quotient))))))))))

;(parameterize*
; ([*bit_width* 4]
;  [*field_generator_poly* "x4+x+1"]
;  [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
;  [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

; (let-values ([(ome lam) (_error-locator '(12 4  3 15) 2)])
;   (check-equal? ome "6x+15")
;   (check-equal? lam "14x2+14x+1"))

; (let-values ([(ome lam) (_error-locator '(5 5 1 11) 2)])
;   (check-false ome)
;   (check-false lam))
; )

;(parameterize*
; ([*bit_width* 8]
;  [*field_generator_poly* "x8+x4+x3+x2+1"]
;  [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
;  [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])
;
; (let-values ([(ome lam) (_error-locator '(17 0 135 209 16 239 171 210 53 216 13 247 89 197 161 90) 16)])
;   (check-equal? ome "175x14+194x13+164x12+53x11+102x10+105x9+36x8+54x7+69x6+28x5+230x4+251x3+137x2+83x+90")
;   (check-equal? lam "59x17+39x15+49x14+183x13+235x12+137x11+112x10+114x9+101x8+221x7+11x6+218x5+122x4+x3+166x2+50x+1")
;   )
;)

