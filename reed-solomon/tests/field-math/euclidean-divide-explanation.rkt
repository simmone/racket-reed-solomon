#lang racket

(require "../../src/field-math.rkt")

(define (euc-divide dividend divisor)
  (let ([divisor_index (caar (poly->index_coe_pairs divisor))])
    (printf "divisor_index:~a\n\n" divisor_index)

    (let loop ([remainder dividend]
               [quotient_list '()])
      
      (let ([remainder_degree (caar (poly->index_coe_pairs remainder))])
        (if (>= remainder_degree divisor_degree)
            (let (
                  [loop_align_factor #f]
                  [loop_divisor_multiply_factor #f]
                  [loop_substract #f]
                  )
              (set! loop_align_factor (poly-gf-n-divide-align divisor remainder))
              
              (set! loop_divisor_multiply_factor (poly-gf-n-multiply divisor loop_align_factor))

              (set! loop_substract (poly-n-add remainder loop_divisor_multiply_factor))
              
              (loop loop_substract (cons loop_align_factor quotient_list)))
            (let ([quotient
                   (foldr (lambda (a b) (if b (string-append a "+" b) a)) #f (reverse quotient_list))])
              (values
               quotient
               remainder)))))))

    ))

(euc-divide "x4" "12x3+4x2+3x+15")

;; (euc-divide "12x3+4x2+3x+15" "6x2+6x1+4x0")

