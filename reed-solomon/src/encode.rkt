#lang racket

(require "field-math.rkt")
(require "primitive_poly_table.rkt")

(provide (contract-out
          [rs-encode (->* 
                      ((listof natural?) natural?) 
                      (#:bit_width natural? #:primitive_poly_value natural?) 
                      (listof natural?))]
          ))

(define (rs-encode 
         data_list
         parity_length
         #:bit_width [bit_width 8]
         #:primitive_poly_value [primitive_poly_value 285]
         )

  (parameterize*
   ([*bit_width* bit_width]
    [*field_generator_poly* (hash-ref *primitive_poly_table* primitive_poly_value)]
    [*galios_index->number_map* (get-galios-index->number_map)])
   
   (let ([code_generator_poly #f]
         [code_generator_list #f]
         [dividend_num_list #f]
         [code_generator_length (add1 parity_length)])

     (set! code_generator_poly (get-code-generator-poly parity_length))
     
     (set! code_generator_list
           (map
            (lambda (p)
              (PITEM-coe p))
            (poly->items code_generator_poly)))

     (set! dividend_num_list `(,@data_list ,@(make-list parity_length 0)))

     (let loop ([loop_dividend_list (drop dividend_num_list parity_length)]
                [loop_remainder_list (take dividend_num_list parity_length)])

       (if (not (null? loop_dividend_list))
           (let* ([appended_dividend_list #f]
                  [aligned_code_generator_list #f]
                  [remainder_list #f])

             (set! appended_dividend_list `(,@loop_remainder_list ,(car loop_dividend_list)))

             (set! aligned_code_generator_list
                   (map (lambda (v) (galios-num-multiply v (car appended_dividend_list))) code_generator_list))

             (set! remainder_list
                   (let loop-bitwise ([dividends appended_dividend_list]
                                      [divisors aligned_code_generator_list]
                                      [result_list '()])
                     (if (not (null? dividends))
                         (loop-bitwise
                          (cdr dividends)
                          (cdr divisors)
                          (cons (bitwise-xor (car dividends) (car divisors)) result_list))
                         (reverse result_list))))

             (loop (cdr loop_dividend_list) (drop remainder_list 1)))
           loop_remainder_list)))))
