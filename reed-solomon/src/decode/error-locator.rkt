#lang racket

(require "../field-math.rkt")

(provide (contract-out
          [error-locator (-> (listof natural?) natural? (values (or/c #f string?) (or/c #f string?)))]
          ))

(define (error-locator syndromes error_length)
  (let ([syndrome_poly
         (index_coe_pairs->poly
          (let loop ([loop_syndromes syndromes]
                     [loop_index (sub1 (length syndromes))]
                     [coe_pairs '()])
            (if (not (null? loop_syndromes))
                (loop (cdr loop_syndromes) (sub1 loop_index) (cons (cons loop_index (car loop_syndromes)) coe_pairs))
                (reverse coe_pairs))))])

    (let loop ([loop_dividend (format "x~a" (* 2 error_length))]
               [loop_divisor syndrome_poly]
               [loop_add_factor "0"]
               [loop_multiply_factor "1"])

      (let ([loop_result #f])
        (let-values ([(quotient remainder) (galios-poly-divide loop_dividend loop_divisor)])
          (set! loop_result (galios-poly-add loop_add_factor (galios-poly-multiply quotient loop_multiply_factor)))

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
                      (values ome_quotient lam_quotient))))))))))
