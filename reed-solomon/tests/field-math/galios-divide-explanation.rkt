#lang racket

(require "../../src/field-math.rkt")

(define (print-divide-elements elements)
  (let loop ([loop_elements elements])
    (when (not (null? loop_elements))
      (printf "[~a]" (~a #:min-width 3 #:align 'left #:right-pad-string " " (car loop_elements)))
      (loop (cdr loop_elements))))
  (printf "\n"))

;; explain each step of Galios divide

(define (galios-divide dividend_poly divisor_poly)
  (printf "Galios Divide Explanation\n\n")
  
  (printf "dividend_poly: ~a\n" dividend_poly)
  (printf "convert dividend_poly to coefficient list: ~a\n\n" (poly->coefficients dividend_poly))

  (printf "divisor_poly: ~a\n" divisor_poly)
  (printf "convert divisor_poly to coefficient list: ~a\n\n" (poly->coefficients divisor_poly))

  (let* ([dividend_poly_bits (poly->coefficients dividend_poly)]
         [dividend_bits_length (string-length dividend_poly_bits)]
         [divisor_poly_bits (poly->coefficients divisor_poly)]
         [divisor_bits_length (string-length divisor_poly_bits)])

    (print-divide-elements
     (let loop ([index (sub1 dividend_bits_length)]
                [result_list '()])
       (if (>= index 0)
           (loop (sub1 index) (cons (format "x~a" index) result_list))
           (reverse result_list))))
    
    (let loop ([loop_bits dividend_poly_bits])
      (when (>= (string-length loop_bits) divisor_bits_length)
        (printf "step1: loop_bits:\n")
        (print-divide-elements
         (string->list
          (~a #:min-width dividend_bits_length #:align 'right #:left-pad-string " " loop_bits)))
        
        (let ([head_loop_bits (substring loop_bits 0 divisor_bits_length)]
              [bitwise_result #f])
          (set! bitwise_result
                (number->string
                 (bitwise-xor
                  (string->number head_loop_bits 2)
                  (string->number divisor_poly_bits 2))
                 2))
          (printf "step2: head loop_bits bitwise-xor divisor_bits:\n")
          (printf "~a bitwise-xor ~a = ~a\n" head_loop_bits divisor_poly_bits bitwise_result)

          (loop (string-append bitwise_result (substring loop_bits divisor_bits_length))))))
  ))

(galios-divide "x6+x5+x4+x1" "x4+x1+1")
