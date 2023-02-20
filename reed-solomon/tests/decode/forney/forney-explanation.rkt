#lang racket

(require "../../../src/field-math.rkt")

(require rackunit)

(define (_calculate-factor poly factor) 
  (let ([polys (poly->items poly)]
        [*2^m_1 (sub1 (expt 2 (*bit_width*)))]
        [index_list #f]
        [bitwise_xor_result #f]
        [result #f])
    
    (printf "\npoly: ~a, factor: ~a\n" poly factor)

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
               (printf "coe(~a) to index = ~a\n" (PITEM-coe pitem) coe->a)
               (printf "index(~a) * factor(~a) = ~a\n" (PITEM-x_index pitem) factor index_multiply_factor)
               (printf "coe->a(~a) + last_result = ~a\n" coe->a coe_add_last_result)
               (printf "modulo last_result = ~a\n" modulo_last_result)
               (printf "convert index to number = ~a\n" index_number)
               index_number))
           polys))
    
    (printf "index_list: ~a\n" index_list)
    
    (set! bitwise_xor_result (apply bitwise-xor index_list))
    
    (printf "bitwise_xor_result: ~a\n" bitwise_xor_result)

    (set! result
          (if (= bitwise_xor_result 0)
              0
              (string->number (substring (hash-ref (*galios_number->index_map*) bitwise_xor_result) 1))))
    
    (printf "result: ~a\n" result)
    
    result))

(define (_forney lam_poly ome_poly err_places)
  (printf "Fornet Explanation\n\n")

  (let ([only_odd_poly
         (items->poly
          (filter (lambda (poly) (odd? (PITEM-x_index poly))) (poly->items lam_poly)))]
        [result_list #f])

    (printf "remove even index's ploly: ~a\n\n" only_odd_poly)

    (let-values ([(derivative_lam _none)
                  (galios-poly-divide only_odd_poly "x")])
      
      (printf "(galios-poly-divide ~a ~a)'s quotient = ~a\n\n" only_odd_poly "x" derivative_lam)

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
                 
                 (printf "\nerror_index: ~a, factor = (- *2^m_1 error_index) = ~a\n" error_index factor)
                 
                 (set! ome_a (_calculate-factor ome_poly factor))
                 (printf "_calculate ~a ~a = ~a\n\n" ome_poly factor ome_a)
                 
                 (set! delam_a (_calculate-factor derivative_lam factor))
                 (printf "_calculate-factor ~a ~a = ~a\n\n" derivative_lam factor delam_a)
                 
                 (set! cal_a (+ error_index (- ome_a delam_a)))
                 (printf "cal_a = (+ error_index(~a) (- ome_a(~a) delam_a(~a))) = ~a\n\n" error_index ome_a delam_a cal_a)

                 (set! positive_a (+ cal_a *2^m_1*))
                 (printf "positive_a = (+ cal_a *2^m_1*) = ~a\n\n" positive_a)

                 (set! modulo_a (modulo positive_a *2^m_1*))
                 (printf "modulo_a = (modulo positive_a *2^m_1*) = ~a\n\n" modulo_a)

                 (set! result_n (hash-ref (*galios_index->number_map*) (format "a~a" modulo_a)))
                 (printf "result_n = (hash-ref (*galios_index->number_map*) modulo_a) = ~a\n\n" result_n)

                 (cons error_index result_n)))
             err_places))
      
      (printf "result_list: ~a\n" result_list))
    
    result_list))

;(parameterize*
; ([*bit_width* 4]
;  [*field_generator_poly* "x4+x+1"]
;  [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
;  [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])
;
; (check-equal? (_forney "14x2+14x+1" "6x+15" '(9 2)) '( (9 . 13)  (2 . 2)))
;)
;
;(parameterize*
; ([*bit_width* 8]
;  [*field_generator_poly* "x8+x4+x3+x2+1"]
;  [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
;  [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])
; 
; (check-equal?
;  (_forney
;   "62x8+237x7+88x6+121x5+63x4+218x3+249x2+179x+1"
;   "152x7+230x6+254x5+208x4+156x3+118x2+152x+134"
;   '(116 102)
;   )
;  '()))
