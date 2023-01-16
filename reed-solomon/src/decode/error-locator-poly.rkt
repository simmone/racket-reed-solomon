#lang racket

(require "../lib/gf.rkt")
(require "../lib/poly.rkt")
(require "../lib/euclidean.rkt")

(provide (contract-out
          [error-locator-poly (-> string? natural? (values (or/c #f string?) (or/c #f string?)))]
          ))

(define (error-locator-poly syndrome_poly_n error_length)
  (let loop ([i 1]
             [r-2 (format "x~a" (* 2 error_length))]
             [r-1 syndrome_poly_n]
             [t-2 "0"]
             [t-1 "1"])

    (let ([q #f]
          [r #f]
          [t #f]
          [r_degree #f])
      
      (let-values ([(quotient remainder) (euc-divide r-2 r-1)])
        (set! q quotient)
        (set! r remainder))

      (set! t (poly-n-add t-2 (poly-gf-n-multiply q t-1)))

      (set! r_degree (cdr (poly-n-car r)))
      
      (if (>= r_degree error_length)
          (loop
           (add1 i)
           r-1
           r
           t-1
           t)
          (let ([t0 (number->string (car (poly-n-tail t)))])

            (if (string=? t0 "0")
                (values #f #f)
                (let-values ([(ome_quotient ome_remainder) (euc-divide r t0)]
                             [(lam_quotient lam_remainder) (euc-divide t t0)])
                  (values ome_quotient lam_quotient))))))))
