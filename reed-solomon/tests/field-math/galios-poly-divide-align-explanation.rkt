#lang racket

(require "../../src/field-math.rkt")

(require rackunit)

(define (galios-divide-align dividend divisor)
  (let* (
         [divisor_pitems (poly->items divisor)]
         [dividend_pitems (poly->items dividend)]
         [src_index_n (PITEM-x_index (car divisor_pitems))]
         [src_coe_n (PITEM-coe (car divisor_pitems))]
         [src_coe_a #f]
         [src_coe_a_n #f]
         [dst_index_n (PITEM-x_index (car dividend_pitems))]
         [dst_coe_n (PITEM-coe (car dividend_pitems))]
         [dst_coe_a #f]
         [dst_coe_a_n #f]
         [2^m_1 (sub1 (expt 2 (*bit_width*)))]
         )

    (printf "divisor_pitems:~a\n\n" divisor_pitems)

    (printf "dividend_pitems:~a\n\n" dividend_pitems)
    
    (set! src_coe_a (hash-ref (*galios_number->index_map*) src_coe_n))
    
    (set! src_coe_a_n (string->number (substring src_coe_a 1)))
    
    (printf "src_coe_a:[~a][~a]\n\n" src_coe_a src_coe_a_n)

    (set! dst_coe_a (hash-ref (*galios_number->index_map*) dst_coe_n))

    (set! dst_coe_a_n (string->number (substring dst_coe_a 1)))
    
    (printf "dst_coe_a:[~a][~a]\n\n" dst_coe_a dst_coe_a_n)

    (printf "(- (+ 2^m_1 dst_coe_a_n) src_coe_a_n) = ~a\n\n" (- (+ 2^m_1 dst_coe_a_n) src_coe_a_n))

    (printf "modulo last_result ~a = ~a\n\n" 2^m_1 (modulo (- (+ 2^m_1 dst_coe_a_n) src_coe_a_n) 2^m_1))
    
    (printf "(hash-ref (*galios_index->number_map*) ~a = ~a\n\n"
            (format "a~a" (modulo (- (+ 2^m_1 dst_coe_a_n) src_coe_a_n) 2^m_1))
            (hash-ref (*galios_index->number_map*) (format "a~a" (modulo (- (+ 2^m_1 dst_coe_a_n) src_coe_a_n) 2^m_1))))

    (items->poly
     (list
      (PITEM
       (- dst_index_n src_index_n)
       (hash-ref (*galios_index->number_map*) (format "a~a" (modulo (- (+ 2^m_1 dst_coe_a_n) src_coe_a_n) 2^m_1))))))))

;(parameterize*
; ([*bit_width* 4]
;  [*field_generator_poly* "x4+x+1"]
;  [*galios_index->number_map* (get-galios-index->number_map)]
;  [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])
;
; (check-equal? (galios-divide-align "x4" "12x3") "10x")
;
; (check-equal? (galios-divide-align "x4" "12x3+4x2+3x+15") "10x")
;)

