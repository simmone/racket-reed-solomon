#lang racket

(require "../../src/field-math.rkt")
(require "../../src/primitive_poly_table.rkt")

(define (print-divide-elements elements)
  (let loop ([loop_elements elements])
    (when (not (null? loop_elements))
      (printf "[~a]" (~a #:min-width 3 #:align 'left #:right-pad-string " " (car loop_elements)))
      (loop (cdr loop_elements))))
  (printf "\n"))

(define (print-line item_list)
  (let loop ([items item_list])
    (if (not (null? items))
        (begin
          (printf "~a|" (~a #:min-width 2 #:align 'left #:right-pad-string " " (car items)))
          (loop (cdr items)))
        (printf "\n\n"))))

;; Reed Solomon Encode Explanation

(define (rs-encode 
         data_list
         parity_length
         #:bit_width [bit_width 8]
         #:primitive_poly_value [primitive_poly_value 285]
         )

  (parameterize*
   ([*bit_width* bit_width]
    [*t* (floor (/ parity_length 2))]
    [*field_generator_poly* (hash-ref *primitive_poly_table* primitive_poly_value)]
    [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))])
   
   (printf "Reed Solomon Encoding Explanation:\n\n")
   
   (printf "data list[~a]:\n\n" (length data_list))
   
   (print-line data_list)

   (printf "parity_length: ~a\n\n" parity_length)
   
   (printf "bit_width: ~a\n\n" (*bit_width*))

   (printf "t: ~a\n\n" (*t*))

   (printf "field_generator_poly:[~a][~a]\n\n" primitive_poly_value (*field_generator_poly*))
   
   (let ([code_generator_poly #f]
         [code_generator_list #f]
         [dividend_num_list `(,@data_list ,@(make-list (- (expt 2 (*bit_width*)) 1 (length data_list)) 0))]
         [code_generator_length (add1 parity_length)]
         [result_list #f])

     (set! code_generator_poly (get-code-generator-poly))
     
     (set! code_generator_list
           (map
            (lambda (p)
              (cdr p))
            (poly->index_coe_pairs code_generator_poly)))

     (printf "code_generator_poly:[~a], coefficient_list: [~a]\n\n" code_generator_poly code_generator_list)

     (printf "append 0 to data list to a full length list:~a\n\n" dividend_num_list) 
     
     (printf "poly divide start:\n\n")

     (print-divide-elements dividend_num_list)

     (set! result_list
           (let loop ([loop_dividend_list (drop dividend_num_list (* 2 (*t*)))]
                      [loop_remainder_list (take dividend_num_list (* 2 (*t*)))])
             (printf "\nLoop Start:\n")

             (printf "loop_remainder_list: ~a\n" loop_remainder_list)

             (printf "loop_dividend_list: ~a\n" loop_dividend_list)

            (if (not (null? loop_dividend_list))
                 (let* ([appended_dividend_list #f]
                        [aligned_code_generator_list #f]
                        [remainder_list #f])

                   (set! appended_dividend_list `(,@loop_remainder_list ,(car loop_dividend_list)))
                   (printf "step1: remainder list: ~a  + first item of rest dividend list: ~a as dividend:\n" loop_remainder_list (car loop_dividend_list))
                   (print-divide-elements appended_dividend_list)

                   (set! aligned_code_generator_list (map (lambda (v) (galios-multiply v (car appended_dividend_list))) code_generator_list))
                   (printf "step2: code_generator_list * dividend_list's first item:~a*~a\n" code_generator_list (car appended_dividend_list))
                   (print-divide-elements aligned_code_generator_list)

                   (set! remainder_list
                         (let loop-bitwise ([dividends appended_dividend_list]
                                            [divisors aligned_code_generator_list]
                                            [result_list '()])
                           (if (not (null? dividends))
                               (loop-bitwise (cdr dividends) (cdr divisors) (cons (bitwise-xor (car dividends) (car divisors)) result_list))
                               (reverse result_list))))
                   (printf "step3: appended_dividend_list bitwise-xor aligned_code_generator:\n")
                   (print-divide-elements remainder_list)

                   (loop (cdr loop_dividend_list) (drop remainder_list 1)))
                 loop_remainder_list)))
     
     (printf "result list:~a\n\n" result_list)
     )))

 (rs-encode '(1 2 3 4 5 6 7 8 9 10 11) 4 #:bit_width 4 #:primitive_poly_value 19)
;;(rs-encode '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17) 10)
