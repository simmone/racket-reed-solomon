#lang racket

(require "../../../src/field-math.rkt")

(require rackunit)

(define (_chien-value lam_poly seq)
  (printf "chien-value lam_poly = ~a, gf_seq = ~a\n\n" lam_poly seq)
  (let* ([2^m_1 (sub1 (expt 2 (*bit_width*)))]
         [2^m_1_seq (- 2^m_1 seq)]
         [item_list #f]
         [result #f])

    (printf "2^m_1 - seq = ~a\n" 2^m_1_seq)

    (set! item_list
          (map
           (lambda (index_coe_pair)
             (let* ([last_multiply_index (* 2^m_1_seq (car index_coe_pair))]
                    [convert_coe->index
                     (string->number (substring (hash-ref (*galios_number->index_map*) (cdr index_coe_pair)) 1))]
                    [add_and_modulo (modulo (+ convert_coe->index last_multiply_index) 2^m_1)]
                    [convert_index->coe (hash-ref (*galios_index->number_map*) (format "a~a" add_and_modulo))])

               (printf "\n1. 2^m_1_seq(~a) * index(~a) = ~a\n" 2^m_1_seq (car index_coe_pair) last_multiply_index)
               (printf "2. convert_coe->index ~a = ~a\n" (cdr index_coe_pair) convert_coe->index)
               (printf "3. add and modulo (modulo (+ ~a ~a) ~a) = ~a\n" convert_coe->index last_multiply_index 2^m_1 add_and_modulo)
               (printf "4. convert_index->coe ~a = ~a\n" add_and_modulo convert_index->coe)
               
               convert_index->coe))
           (poly->index_coe_pairs lam_poly)))

    (set! result (foldr bitwise-xor 0 item_list))
    
    (printf "\nbitwise result list (bitwise-xor ~a) = ~a\n" item_list result)
    
    result))

(define (_chien-search lam_poly)
  (let loop ([loop_index (sub1 (expt 2 (*bit_width*)))]
             [result_list '()])
    (if (>= loop_index 0)
        (let ([chien_value (_chien-value lam_poly loop_index)])
          (printf "(chien-value ~a ~a) = ~a\n\n" lam_poly loop_index chien_value)
          (if (= chien_value 0)
              (loop (sub1 loop_index) (cons loop_index result_list))
              (loop (sub1 loop_index) result_list)))
        (reverse result_list))))

(parameterize*
 ([*bit_width* 4]
  [*field_generator_poly* "x4+x+1"]
  [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
  [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

; (check-equal? (_chien-value "14x2+14x1+1x0" 14) 3)
 
 (check-equal? (_chien-search "14x2+14x+1") '(9 2))
 )
