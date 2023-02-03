#lang racket

(require "../../src/field-math.rkt")

(define (_galios-poly-divide dividend divisor)
  (let ([divisor_index (caar (poly->index_coe_pairs divisor))])
    (printf "divisor_index:~a\n\n" divisor_index)

    (let loop ([remainder dividend]
               [quotient ""]
               [last_op ""])

      (printf "remainder: [~a]\n\n" remainder)
      
      (if (not (string=? remainder ""))
          (let ([remainder_index (caar (poly->index_coe_pairs remainder))])
            (printf "remainder:~a\ndivisor=~a\n\n" remainder divisor)
            
            (if (>= remainder_index divisor_index)
                (let ([loop_align_factor #f]
                      [loop_divisor_multiply_factor #f]
                      [loop_substract #f])

                  (set! loop_align_factor (galios-poly-divide-align remainder divisor))
                  (printf "loop_align_factor = (galios-divide-align ~a ~a) = ~a\n\n"
                          divisor remainder loop_align_factor)

                  (set! loop_divisor_multiply_factor (galios-poly-multiply divisor loop_align_factor))
                  (printf "loop_divisor_multiply_factor = (galios-poly-multiply ~a ~a) = ~a\n\n"
                          divisor loop_align_factor loop_divisor_multiply_factor)

                  (set! loop_substract (galios-poly-add remainder loop_divisor_multiply_factor))
                  (printf "loop_substract = (galios-poly-add ~a ~a) = ~a\n\n"
                          remainder loop_divisor_multiply_factor loop_substract)
                  
                  (loop loop_substract (string-append quotient last_op loop_align_factor) "+"))
                (values quotient remainder)))
          (values quotient remainder)))))

(parameterize*
 ([*bit_width* 4]
  [*field_generator_poly* "x4+x+1"]
  [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
  [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

 (let-values ([(quotient remainder)
;;             (_galios-poly-divide "x6+x5+x4+x1" "x4+x1+1")])
;;               (_galios-poly-divide "12x3+4x2+3x+15" "6x2+6x1+4")])
               (_galios-poly-divide "7x2+7x+9" "9")])
   (printf "quotient: ~a\nremainder: ~a\n" quotient remainder)))
