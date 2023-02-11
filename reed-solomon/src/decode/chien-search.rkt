#lang racket

(require "../field-math.rkt")

(provide (contract-out
          [chien-value (-> string? natural? exact-integer?)]
          [chien-search (-> string? (listof exact-integer?))]
          ))

(define (chien-value lam_poly seq)
  (let* ([2^m_1 (sub1 (expt 2 (*bit_width*)))])

    (foldr bitwise-xor 0
           (map
            (lambda (index_coe_pair)
              (let* ([last_multiply_index (* (- 2^m_1 seq) (car index_coe_pair))]
                     [convert_coe->index
                      (string->number (substring (hash-ref (*galios_number->index_map*) (cdr index_coe_pair)) 1))]
                     [add_and_modulo (modulo (+ convert_coe->index last_multiply_index) 2^m_1)]
                     [convert_index->coe (hash-ref (*galios_index->number_map*) (format "a~a" add_and_modulo))])
                convert_index->coe))
            (poly->index_coe_pairs lam_poly)))))

(define (chien-search lam_poly)
  (let loop ([loop_index (sub1 (expt 2 (*bit_width*)))]
             [result_list '()])
    (if (>= loop_index 0)
        (let ([chien_value (chien-value lam_poly loop_index)])
          (if (= chien_value 0)
              (loop (sub1 loop_index) (cons loop_index result_list))
              (loop (sub1 loop_index) result_list)))
        (reverse result_list))))
