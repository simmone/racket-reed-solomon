#lang racket

(require "gf.rkt")
(require "poly.rkt")

(provide (contract-out
          [euc-divide (-> string? string? (values string? string?))]
          ))

(define (euc-divide dividend divisor)
  (let ([divisor_degree (cdr (poly-n-car divisor))])
    (let loop ([loop_dividend dividend]
               [quotient_list '()])
      
      (let ([loop_dividend_degree (cdr (poly-n-car loop_dividend))])
        (if (and (not (string=? loop_dividend "")) (>= loop_dividend_degree divisor_degree))
            (let (
                  [loop_align_factor #f]
                  [loop_divisor_multiply_factor #f]
                  [loop_substract #f]
                  )
              (set! loop_align_factor (poly-gf-n-divide-align divisor loop_dividend))
              
              (set! loop_divisor_multiply_factor (poly-gf-n-multiply divisor loop_align_factor))

              (set! loop_substract (poly-n-add loop_dividend loop_divisor_multiply_factor))
              
              (loop loop_substract (cons loop_align_factor quotient_list)))
            (let ([quotient
                   (foldr (lambda (a b) (if b (string-append a "+" b) a)) #f (reverse quotient_list))])
              (values
               quotient
               loop_dividend)))))))
