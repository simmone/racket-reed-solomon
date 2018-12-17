#lang racket

(require "../share/lib.rkt")
(require "../share/gf.rkt")
(require "../share/poly.rkt")
(require "../share/euclidean.rkt")

(provide (contract-out
          [derivative-lam (-> string? string?)]
          ))

(define (derivative-lam lam_poly)
  (let-values ([(quotient remainder)
                (euc-divide
                 (poly-n->string
                  (filter
                   (lambda (item)
                     (odd? (cdr item)))
                   (string-n->poly lam_poly)))
                 "x")])
    quotient))
