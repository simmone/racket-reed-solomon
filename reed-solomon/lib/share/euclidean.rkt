#lang racket

(require "lib.rkt")
(require "gf.rkt")
(require "poly.rkt")

(provide (contract-out
          [euc-divide (->* (string? string?) (boolean?) (values string? string?))]
          ))

(define (euc-divide dividend divisor [need-express? #f])
;  (let ([out (open-output-nowhere)])
  (let ([out (current-output-port)])
    (when (and need-express? (*express?*))
      (let* ([scrbl_dir (build-path (*express_path*) "euclidean-divide")]
             [scrbl_file (build-path scrbl_dir "euclidean-divide.scrbl")])

        (make-directory* scrbl_dir)
            
        (set! out (open-output-file scrbl_file #:exists 'replace))))
    
    (fprintf out "#lang scribble/base\n\n")

    (fprintf out "@title{Euclidean Divide}\n\n")
    
    (let ([divisor_degree (cdr (poly-n-car divisor))])
      (let loop ([loop_dividend dividend]
                 [quotient_list '()])
        
        (fprintf out "@section{~a}\n" (if (null? quotient_list) "start" (car quotient_list)))

        (let ([loop_dividend_degree (cdr (poly-n-car loop_dividend))])

          (fprintf out "@verbatim{dividend:~a}\n" loop_dividend)

          (fprintf out "@verbatim{divisor:~a}\n" divisor)

          (fprintf out "@verbatim{dividend_degree:~a, divisor_degree:~a}\n" loop_dividend_degree divisor_degree)
          
          (if (and (not (string=? loop_dividend "")) (>= loop_dividend_degree divisor_degree))
              (let (
                    [loop_align_factor #f]
                    [loop_divisor_multiply_factor #f]
                    [loop_substract #f]
                    )
                (set! loop_align_factor (poly-gf-n-divide-align divisor loop_dividend))
                          
                (fprintf out "@verbatim{align_factor:~a}\n" loop_align_factor)
                          
                (set! loop_divisor_multiply_factor (poly-gf-n-multiply divisor loop_align_factor))
                          
                (fprintf out "@verbatim{divisor_multiply_factor:~a}\n" loop_divisor_multiply_factor)
                          
                (set! loop_substract (poly-n-add loop_dividend loop_divisor_multiply_factor))
                          
                (fprintf out "@verbatim{substract:~a}\n" loop_substract)
                          
                (loop loop_substract (cons loop_align_factor quotient_list)))
              (let ([quotient
                     (foldr (lambda (a b) (if b (string-append a "+" b) a)) #f (reverse quotient_list))])
                (fprintf out "@section{end}\n")
                (fprintf out "@verbatim{quotient:~a}\n" quotient)
                (fprintf out "@verbatim{remainer:~a}\n" loop_dividend)
              (values
               quotient
               loop_dividend))))))))

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
