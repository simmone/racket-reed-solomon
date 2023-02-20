#lang racket

(require "../field-math.rkt")

(provide (contract-out
          [forney (-> string? string? (listof natural?) (listof pair?))]
          [calculate-factor (-> string? natural? natural?)]
          ))

(define (calculate-factor poly factor)
  (let ([polys (poly->items poly)]
        [*2^m_1 (sub1 (expt 2 (*bit_width*)))]
        [index_list #f]
        [bitwise_xor_result #f]
        [result #f])
    
    (set! index_list
          (map
           (lambda (pitem)
             (let* ([coe->a
                     (string->number
                      (substring
                       (hash-ref
                        (*galios_number->index_map*)
                        (PITEM-coe pitem))
                       1))]
                    [index_multiply_factor (* factor (PITEM-x_index pitem))]
                    [coe_add_last_result (+ coe->a index_multiply_factor)]
                    [modulo_last_result (modulo coe_add_last_result *2^m_1)]
                    [index_number
                     (hash-ref
                      (*galios_index->number_map*)
                      (format "a~a" modulo_last_result))])
               index_number))
           polys))
    
    (set! bitwise_xor_result (apply bitwise-xor index_list))
    
    (set! result
          (if (= bitwise_xor_result 0)
              0
              (string->number (substring (hash-ref (*galios_number->index_map*) bitwise_xor_result) 1))))
    
    result))

(define (forney lam_poly ome_poly err_places)
  (let ([only_odd_poly
         (items->poly
          (filter (lambda (poly) (odd? (PITEM-x_index poly))) (poly->items lam_poly)))]
        [result_list #f])

    (let-values ([(derivative_lam _none)
                  (galios-poly-divide only_odd_poly "x")])
      
      (set! result_list
            (map
             (lambda (error_index)
               (let* ([*2^m_1* (sub1 (expt 2 (*bit_width*)))]
                      [factor (- *2^m_1* error_index)]
                      [ome_a #f]
                      [delam_a #f]
                      [cal_a #f]
                      [positive_a #f]
                      [modulo_a #f]
                      [result_n #f])
                 
                 (set! ome_a (calculate-factor ome_poly factor))
                 
                 (set! delam_a (calculate-factor derivative_lam factor))
                 
                 (set! cal_a (+ error_index (- ome_a delam_a)))

                 (set! positive_a (+ cal_a *2^m_1*))

                 (set! modulo_a (modulo positive_a *2^m_1*))

                 (set! result_n (hash-ref (*galios_index->number_map*) (format "a~a" modulo_a)))

                 (cons error_index result_n)))
             err_places)))
      
    result_list))
