#lang racket

(require "../share/lib.rkt")
(require "../share/gf.rkt")
(require "../share/poly.rkt")
(require "../share/euclidean.rkt")

(provide (contract-out
          [error-locator-poly (-> string? natural? string?)]
          ))

(define (error-locator-poly syndrome_poly_n t)
  (let loop ([i 1]
             [r_/i-2/x/ (format "~ax" (* 2 t))]
             [r_/i-1/x/ syndrome_poly_n]
             [s_/i-2/x/ 1]
             [s_/i-1/x/ 0]
             [t_/i-2/x/ 0]
             [t_/i-1/x/ 1])
    (let ([q_/i/x/ #f]
          [r_/i/x/ #f]
          [s_/i/x/ #f]
          [t_/i/x/ #f])

      (let-values ([(quotient remainder) (euc-divide r_/i-2/x/ r_/i-1/x/)])
        (set! q_/i/x/ quotient)
        (set! r_/i/x/ remainder))
      
      (set! s_/i/x/ (poly-n-add s_/i-2/x/ (poly-gf-n-multiply q_/i/x/ s_/i-1/x/)))
      
      (set! t_/i/x/ (poly-n-add t_/i-2/x/ (poly-gf-n-multiply q_/i/x/ t_/i-1/x/)))
      
      (if (>= (cdr (poly-n-car r_/i/x/)) t)
          (loop
           (add1 i)
           r_/i-1/x/
           r_/i/x/
           s_/i-1/x/
           s_/i/x/
           t_/i-1/x/
           t_/i/x/)
          (let ([t_/i/0/ (car (poly-n-tail t_/i/x/))])
            (let-values ([(ome_quotient ome_remainder) (euc-divide r_/i/x/ t_/i/0/)]
                         [(lam_quotient lam_remainder) (euc-divide t_/i/x/ t_/i/0/)])
              (values ome_quotient lam_quotient)))))))
              
           
      
      
      
