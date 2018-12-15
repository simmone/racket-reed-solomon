#lang racket

(require "gf.rkt")
(require "poly.rkt")

(provide (contract-out
          [euc-divide (-> string? string? (values string? string?))]
          ))

(define (euc-divide dividend divisor)
  (let ([divisor_degree (cdr (poly-car divisor))])
    (let loop ([loop_dividend dividend]
               [quotient_list '()])
      (let ([loop_dividend_degree (cdr (poly-car loop_dividend))])

        (printf "loop_dividend_degree:~a\n" loop_dividend_degree)

        (if (>= loop_dividend_degree divisor_degree)
            (let (
                  [loop_align_factor #f]
                  [loop_divisor_multiply_factor #f]
                  [loop_substract #f]
                  )
              
              (set! loop_align_factor (poly-gf-n-divide-align divisor loop_dividend))
              
              (printf "loop_align_factor:~a\n" loop_align_factor)
              
              (set! loop_divisor_multiply_factor (poly-gf-n-multiply divisor loop_align_factor))
              
              (printf "loop_divisor_multiply_factor:~a\n" loop_divisor_multiply_factor)
        
              (set! loop_substract (poly-n-add loop_dividend loop_divisor_multiply_factor))
              
              (printf "loop_substract:~a\n" loop_substract)
        
              (loop loop_substract (cons loop_align_factor quotient_list)))
            (values
             (foldr (lambda (a b) (if b (string-append a "+" b) a)) #f (reverse quotient_list))
             loop_dividend))))))

;(define (euc-divide syndrome_poly_n divisor_poly_n t)
;  (let loop ([i 1]
;             [r_/i-2/x/ (format "~ax" (* 2 t))]
;             [r_/i-1/x/ syndrome_poly_n]
;             [s_/i-2/x/ 1]
;             [s_/i-1/x/ 0]
;             [t_/i-2/x/ 0]
;             [t_/i-1/x/ 1])
;    (let ([q_/i/x/ #f]
;          [r_/i/x/ #f]
;          [s_/i/x/ #f]
;          [t_/i/x/ #f])
;      (set! q_/i/x/ (poly-gf-
;
