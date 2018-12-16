#lang racket

(require "../share/lib.rkt")
(require "../share/gf.rkt")
(require "../share/poly.rkt")
(require "../share/euclidean.rkt")

(provide (contract-out
          [error-locator-poly (-> string? natural? (values string? string?))]
          ))

(define (error-locator-poly syndrome_poly_n error_length)
  (let loop ([i 1]
             [r-2 (format "x~a" (* 2 error_length))]
             [r-1 syndrome_poly_n]
             [t-2 "0"]
             [t-1 "1"])
    (let ([q #f]
          [r #f]
          [t #f])
      
      (printf "i:~a\n" i)
      (printf "r~a:~a\n" (- i 2) r-2)
      (printf "r~a:~a\n" (- i 1) r-1)
      (printf "t~a:~a\n" (- i 2) t-2)
      (printf "t~a:~a\n" (- i 1) t-1)

      (let-values ([(quotient remainder) (euc-divide r-2 r-1)])
        (set! q quotient)
        (set! r remainder))

      (printf "q~a:~a\n" i q)
      (printf "r~a:~a\n" i r)
      
      (set! t (poly-n-add t-2 (poly-gf-n-multiply q t-1)))
      
      (printf "errlor length:~a\n" error_length)
      
      (if (>= (cdr (poly-n-car r)) error_length)
          (loop
           (add1 i)
           r-1
           r
           t-1
           t)
          (let ([t0 (number->string (car (poly-n-tail t)))])
            (printf "t~a0:~a\n" i t0)

            (let-values ([(ome_quotient ome_remainder) (euc-divide r t0)]
                         [(lam_quotient lam_remainder) (euc-divide t t0)])
              (printf "ome_quotient: ~a\n" ome_quotient)
              (printf "lam_quotient:~a\n" lam_quotient)
              (values ome_quotient lam_quotient)))))))
              
           
      
      
      
