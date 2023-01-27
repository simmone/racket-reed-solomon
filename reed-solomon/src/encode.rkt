#lang racket

(require "field-math.rkt")
(require "primitive_poly_table.rkt")

(provide (contract-out
          [rs-encode (->* 
                      ((listof exact-integer?) natural?) 
                      (#:bit_width natural? #:primitive_poly_value natural?) 
                      (listof exact-integer?))]
          ))

(define (rs-encode 
         data_list
         parity_length
         #:bit_width [bit_width 8]
         #:primitive_poly_value [primitive_poly_value 285]
         )

  (parameterize*
   ([*bit_width* bit_width]
    [*t* (floor (/ parity_length))]
    [*field_generator_poly* (hash-ref *primitive_poly_table* primitive_poly_value)]
    [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))])
   
   (let ([code_generator_poly #f]
         [code_generator_list #f]
         [dividend_num_list #f]
         [code_generator_length (add1 parity_length)])

     (set! code_generator_poly (get-code-generator-poly))
     
     (set! code_generator_list (map (lambda (p) (cdr p)) (poly->index_coe_pairs code_generator_poly)))

     (set! dividend_num_list `(,@data_list ,@(make-list (- (expt 2 (*bit_width*)) 1 (length data_list)) 0)))

     (let loop ([loop_index (- (expt 2 (*bit_width*)) 2)]
                [loop_remainder_list ]
                [loop_dividend_list (drop dividend_num_list parity_length)])
       (if (>= loop_index (sub1 code_generator_length))
           (let* ([loop_dividend_full_list `(,@loop_remainder_list ,(car loop_dividend_list))]
                  [aligned_code_generator_list (map (lambda (v) (galios-multiply v (car loop_dividend_full_list))) code_generator_list)]
                  [remainder_list
                   (let loop-bitwise ([dividends loop_dividend_full_list]
                                      [divisors aligned_code_generator_list]
                                      [result_list '()])
                     (if (not (null? dividends))
                         (loop-bitwise (cdr dividends) (cdr divisors) (cons (bitwise-xor (car dividends) (car divisors)) result_list))
                         (reverse result_list)))])
             (loop (sub1 loop_index) (drop remainder_list 1) (cdr loop_dividend_list)))
           loop_remainder_list)))))
