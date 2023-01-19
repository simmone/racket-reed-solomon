#lang racket

(require "../../src/field-math.rkt")

;; explain each step of Galios divide

(define (galios-divide dividend_poly divisor_poly)
  (printf "Galios Divide Explanation\n\n")
  
  (printf "dividend_poly: ~a\n" dividend_poly)
  (printf "convert dividend_poly to coefficient list: ~a\n\n" (poly->coefficients dividend_poly))

  (printf "divisor_poly: ~a\n" divisor_poly)
  (printf "convert divisor_poly to coefficient list: ~a\n\n" (poly->coefficients divisor_poly))
  )

(galios-divide "a6+a5+a4+a1" "a4+a1+1")

