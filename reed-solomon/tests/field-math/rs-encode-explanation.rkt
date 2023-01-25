#lang racket

(require "../../src/field-math.rkt")
(require "../../src/primitive_poly_table.rkt")

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
    [*field_generator_poly* (hash-ref *primitive_poly_table* primitive_poly_value)]
    [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))])
   
   (printf "Reed Solomon Encoding Explanation:\n\n")
   
   (printf "data list[~a]:\n\n" (length data_list))
   
   (print-line data_list)

   (printf "parity_length: ~a\n\n" parity_length)
   
   (printf "bit_width: ~a\n\n" (*bit_width*))

   (printf "field_generator_poly:[~a][~a]\n\n" primitive_poly_value (*field_generator_poly*))
   ))

(rs-encode '(1 2 3 4 5 6 7 8 9 10 11) 4 #:bit_width 4 #:primitive_poly_value 19)




      
