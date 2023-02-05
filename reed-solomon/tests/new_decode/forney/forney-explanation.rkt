#lang racket

(require "../../../src/field-math.rkt")

(define (_forney lam_poly ome_poly err_places)
  (printf "Fornet Explanation\n\n")

  (let ([only_odd_poly
         (index_coe_pairs->poly
          (filter (lambda (poly) (odd? (car poly))) (poly->index_coe_pairs lam_poly)))])

    (printf "remove even index's ploly: ~a\n\n" only_odd_poly)

    (let-values ([(derivative_lam _none)
                (galios-poly-divide only_odd_poly "x")])
      
      (printf "(galios-poly-divide ~a ~a)'s quotient = ~a\n\n" only_odd_poly "x" derivative_lam)

      (map
       (lambda (error_index)
         (let ([ome_a #f]
               [delam_a #f]
               [cal_a #f]
               [positive_a #f]
               [modulo_a #f]
               [result_n #f])
         
           (set! ome_a (poly-gf-n-sub-x->a ome_poly (- (*2^m_1*) error_index)))
         
         (set! delam_a (poly-gf-n-sub-x->a derivative_lam (- (*2^m_1*) error_index)))

         (set! cal_a (+ error_index (- ome_a delam_a)))

         (set! positive_a (+ cal_a (*2^m_1*)))

         (set! modulo_a (modulo positive_a (*2^m_1*)))

         (set! result_n (hash-ref (*gf_aton_map*) modulo_a))

         (cons error_index result_n)))
     err_places)))

(parameterize*
 ([*bit_width* 4]
  [*field_generator_poly* "x4+x+1"]
  [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
  [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

 (check-equal? (derivative-lam "14x2+14x+1") "14")

 (check-equal? (forney "14x2+14x+1" "6x+15" '(9 2)) '( (9 . 13)  (2 . 2))))
